use std::{cell::RefCell, collections::HashMap, mem::MaybeUninit, ops::ControlFlow, sync::{Arc, Mutex, Weak}};

use crate::{
    bytecode::{Command, SourceId}, dinobj::{AllocatedObject, AllocatedRef, DinObject, DinoResult, DinoStack, DinoValue, Pending, SourceFnFunc, SourceFnResult, StackItem, TailCall, TailCallAvailability, UserFn, VariantObject}, errors::{RuntimeError, RuntimeViolation}, maybe_owned::MaybeOwned
};

#[derive(Debug)]
pub enum RuntimeCell<'s> {
    Uninitialized,
    Value(AllocatedRef<'s>),
}

struct SharedRuntime<'s> {
    allocated_space: usize,
    // common values
    // TODO replace these with MaybeUninit when that stabilizes
    true_: Option<AllocatedRef<'s>>,
    false_: Option<AllocatedRef<'s>>,
    null: Option<AllocatedRef<'s>>,
    nil: Option<AllocatedRef<'s>>,
}

pub(crate) const REPORT_MEMORY_USAGE: bool = false;
pub(crate) const INTERN_CONSTS: bool = true;

impl<'s> SharedRuntime<'s>{

    fn clone_ref(&mut self, obj: &AllocatedRef<'s>) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        if REPORT_MEMORY_USAGE {
            println!("cloning ref ({} bytes) to {:?}", AllocatedRef::SIZE, obj);
            println!("total allocated space: {} bytes", self.allocated_space);
        }
        Ok(unsafe{obj.clone()})
    }

    fn clone_true(&mut self) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        assert!(INTERN_CONSTS);
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        let r = self.true_.as_ref().unwrap();
        Ok(unsafe{r.clone()})
    }

    fn clone_false(&mut self) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        assert!(INTERN_CONSTS);
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        let r = self.false_.as_ref().unwrap();
        Ok(unsafe{r.clone()})
    }
}

// todo in the future, this shouldn't be an arc, objects should instead store a pointer to the runtime
#[derive(Clone)]
pub struct Runtime<'s>(Arc<Mutex<SharedRuntime<'s>>>);

impl<'s> Runtime<'s> {
    pub fn allocate(&self, obj: Result<DinObject<'s>, RuntimeError>) -> DinoResult<'s> {
        let Ok(obj) = obj else {todo!()};
        let size = obj.allocated_size();
        {
            let mut rt = self.0.lock().unwrap();
            rt.allocated_space += size + AllocatedRef::SIZE;
            if REPORT_MEMORY_USAGE {
                println!("allocating {} bytes for {:?}", size + AllocatedRef::SIZE, obj);
                println!("total allocated space: {} bytes", rt.allocated_space);
            }
        }
        let allocated_object = AllocatedObject::new(obj, self.clone());
        // todo check against max size
        let ptr = AllocatedRef::new(Arc::new(allocated_object));
        Ok(Ok(ptr))
    }

    pub fn new() -> Self {
        let shared_runtime = SharedRuntime{
            allocated_space: 0,
            true_: None,
            false_: None,
            null: None,
            nil: None,
        };
        let ret = Self(Arc::new(Mutex::new(shared_runtime)));
        let true_ = ret.allocate(Ok(DinObject::Bool(true))).unwrap().unwrap();
        let false_ = ret.allocate(Ok(DinObject::Bool(false))).unwrap().unwrap();
        let nil = ret.allocate(Ok(DinObject::Struct(vec![]))).unwrap().unwrap();
        let null = ret.allocate(Ok(DinObject::Variant(VariantObject::new(0, ret.clone_ref(&nil).unwrap())))).unwrap().unwrap();
        
        if INTERN_CONSTS{
            let mut rt = ret.0.lock().unwrap();
            rt.true_ = Some(true_);
            rt.false_ = Some(false_);
            rt.nil = Some(nil);
            rt.null = Some(null);
        }
        
        ret
    }

    pub fn bool(&self, b: bool) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        if INTERN_CONSTS{
            let mut rt = self.0.lock().unwrap();
            if b {
                rt.clone_true()
            } else {
                rt.clone_false()
            }
        }
        else{
            self.allocate(Ok(DinObject::Bool(b)))?
        }
    }

    pub fn null(&self) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        let x = self.0.lock().unwrap();
        self.clone_ref(
            x.null.as_ref().unwrap()
        )
    }

    pub fn clone_ref(&self, obj: &AllocatedRef<'s>) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        let mut rt = self.0.lock().unwrap();
        rt.clone_ref(obj)
    }

    pub fn deallocate(&self, size: usize) {
        let mut rt = self.0.lock().unwrap();
        rt.allocated_space -= size;
        if REPORT_MEMORY_USAGE {
            println!("total allocated space: {} bytes", rt.allocated_space);
        }
    }

    pub fn into_weak(self) -> Weak<Mutex<impl Sized + 's>>{
        Arc::downgrade(&self.0)
    }
}

pub struct Sources<'s> {
    sources: HashMap<SourceId, Vec<AllocatedRef<'s>>>,
}

impl<'s> Sources<'s> {
    fn new() -> Self {
        Self {
            sources: HashMap::new(),
        }
    }
}

pub struct RuntimeFrame<'s, 'r> {
    pub stack: DinoStack<'s>,
    pub runtime: &'r Runtime<'s>,

    user_fn: Option<AllocatedRef<'s>>, // guaranteed to be a UserFn if present
    pub cells: Vec<RuntimeCell<'s>>,
    global_frame: Option<&'r RuntimeFrame<'s, 'r>>,
    sources: Option<Sources<'s>>,
}

impl<'s, 'r> RuntimeFrame<'s, 'r> {
    fn empty_cells(n_cells: usize) -> Vec<RuntimeCell<'s>> {
        let mut cells = Vec::with_capacity(n_cells);
        for _ in 0..n_cells {
            cells.push(RuntimeCell::Uninitialized);
        }
        cells
    }

    pub fn root(n_cells: usize, runtime: &'r Runtime<'s>) -> Self {
        Self {
            stack: Vec::new(),
            runtime,
            user_fn: None,
            cells: Self::empty_cells(n_cells),
            global_frame: None,
            sources: Some(Sources::new()),
        }
    }

    fn is_root(&self) -> bool {
        self.global_frame.is_none()
    }

    fn get_global_frame<'a>(&'a self) -> &'a RuntimeFrame<'s, 'r> {
        self.global_frame.unwrap_or(self)
    }

    pub fn add_source(&mut self, source_id: SourceId, source: Vec<AllocatedRef<'s>>) {
        assert!(self.is_root());
        let sources = self.sources.as_mut().unwrap();
        assert!(!sources.sources.contains_key(&source_id));
        sources.sources.insert(source_id, source);
    }

    fn get_sources(&self) -> &Sources<'s> {
        self.get_global_frame().sources.as_ref().unwrap()
    }

    pub fn child(&'r self, user_fn: AllocatedRef<'s>) -> Self {
        let n_cells = match user_fn.as_ref() {
            DinObject::UserFn(user_fn) => user_fn.n_cells,
            _ => unreachable!(),
        };
        Self {
            stack: Vec::new(),
            runtime: self.runtime,
            user_fn: Some(user_fn),
            cells: Self::empty_cells(n_cells),
            global_frame: Some(self.get_global_frame()),
            sources: None,
        }
    }

    fn eval_top(&mut self, tca: TailCallAvailability) -> Result<ControlFlow<(), TailCall<'s>>, RuntimeViolation> {
        loop {
            match self.stack.last() {
                Some(StackItem::Value(_)) => return Ok(ControlFlow::Break(())),
                Some(StackItem::Pending(..)) => {
                    let Some(StackItem::Pending(Pending { func, arguments })) = self.stack.pop() else {
                        unreachable!()
                    };
                    // note that tail-call-optimization is not allowed here, only from system functions
                    match func.as_ref() {
                        DinObject::UserFn(user_fn) => {
                            // todo the frame could hold a ref to the user_fn instead of cloning the captures
                            let func = self.runtime.clone_ref(&func)?;
                            let mut child_frame = self.child(func);
                            let val = child_frame.exec_fn(arguments)?;
                            self.stack.push(StackItem::Value(val));
                        }
                        DinObject::SourceFn(source_fn) => {
                            let mut frame = SystemRuntimeFrame::from_parent(&self, arguments, tca);
                            let ret = source_fn(&mut frame)?;
                            match ret {
                                ControlFlow::Break(val) => {
                                    self.stack.push(StackItem::Value(val));
                                }
                                ControlFlow::Continue(cont) => {
                                    return Ok(ControlFlow::Continue(cont));
                                }
                            }
                        }
                        DinObject::Tail => {
                            // this is a similar case to user functions, but we use the frame's user_fn
                            let Some(user_fn) = self.user_fn.as_ref() else {
                                panic!("tail call with no user function")
                            };
                            let func = self.runtime.clone_ref(&user_fn)?;
                            let mut new_frame = self.child(func);
                            let val = new_frame.exec_fn(arguments)?;
                            self.stack.push(StackItem::Value(val));
                        }
                        _ => {
                            todo!() // err
                        }
                    }
                }
                None => {
                    panic!("the stack is empty!") // very error
                }
            }
        }
    }

    fn get_capture<'a>(&'a mut self, i: usize) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        match self.user_fn.as_ref().unwrap().as_ref() {
            DinObject::UserFn(user_fn) => self.runtime.clone_ref(&user_fn.captures[i]),
            _ => unreachable!(),
        }
    }

    pub fn exec_fn(&mut self, mut args: Vec<StackItem<'s>>) -> DinoResult<'s> {
        'tail: loop {
            let user_fn = self.user_fn.as_ref().unwrap();
            match user_fn.as_ref() {
                DinObject::UserFn(user_fn) => {
                    self.stack.extend(args);
                    for command in user_fn.commands.iter() {
                        if let ControlFlow::Continue(new_args) = self.execute(command)?{
                            args = new_args;
                            self.stack.clear();
                            // note: for now we also clear all the runtime cells, this is not necessary
                            self.cells = Self::empty_cells(self.cells.len());
                            continue 'tail;
                        }
                    }
                    let ret = self.stack.pop().unwrap();
                    let StackItem::Value(val) = ret else {
                        todo!()
                    };
                    break Ok(val)
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn execute<'a: 's>(&mut self, command: &'a Command<'s>) -> Result<ControlFlow<(), TailCall<'s>>, RuntimeViolation> {
        match command {
            Command::PopToCell(i) => {
                debug_assert!(matches!(self.cells[*i], RuntimeCell::Uninitialized));
                self.eval_top(TailCallAvailability::Disallowed)?;
                let StackItem::Value(val) = self.stack.pop().unwrap() else {
                    todo!()
                };
                match val {
                    Ok(val) => {
                        self.cells[*i] = RuntimeCell::Value(val);
                        Ok(ControlFlow::Break(()))
                    }
                    Err(_) => {
                        todo!() // exit the current frame
                    }
                }
            }
            Command::EvalTop => self.eval_top(TailCallAvailability::Allowed),
            Command::PushInt(i) => {
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Int(*i)))?));
                Ok(ControlFlow::Break(()))
            }
            Command::PushFloat(f) => {
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Float(*f)))?));
                Ok(ControlFlow::Break(()))
            }
            Command::PushBool(b) => {
                self.stack
                    .push(StackItem::Value(Ok(self.runtime.bool(*b)?)));
                Ok(ControlFlow::Break(()))
            }
            Command::PushString(s) => {
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Str(s.clone())))?));
                Ok(ControlFlow::Break(()))
            }
            Command::PushFromCell(i) => {
                match &self.cells[*i] {
                    RuntimeCell::Uninitialized => panic!("cell {i} is uninitialized"), // err
                    RuntimeCell::Value(val) => {
                        self.stack.push(StackItem::Value(DinoValue::Ok(self.runtime.clone_ref(val)?)));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::PushFromSource(pfs) => {
                let source_fn = self.runtime.clone_ref(&self.get_sources().sources[pfs.source][pfs.id])?;
                self.stack.push(StackItem::Value(DinoValue::Ok(source_fn)));
                Ok(ControlFlow::Break(()))
            }
            Command::PushFromCapture(i) => {
                let val = self.get_capture(*i)?;
                self.stack.push(StackItem::Value(DinoValue::Ok(val)));
                Ok(ControlFlow::Break(()))
            }
            Command::PushGlobal(i) => {
                let cell = &self.get_global_frame().cells[*i];
                match cell {
                    RuntimeCell::Uninitialized => todo!(), // err
                    RuntimeCell::Value(val) => {
                        let new_val = self.runtime.clone_ref(val)?;
                        self.stack.push(StackItem::Value(DinoValue::Ok(new_val)));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::PushTail => {
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Tail))?));
                Ok(ControlFlow::Break(()))
            }
            Command::MakeFunction(mfn) => {
                let captures = {
                    let mut captures = Vec::with_capacity(mfn.n_captures);
                    for _ in 0..mfn.n_captures {
                        self.eval_top(TailCallAvailability::Disallowed)?; // NOTE: i think this is actually never needed, aren't captures always retrieved from cells?
                        let StackItem::Value(Ok(val)) = self.stack.pop().unwrap() else {
                            todo!()
                        };
                        captures.push(val);
                    }
                    captures
                };
                let user_fn = UserFn {
                    captures,
                    n_cells: mfn.n_cells,
                    commands: &mfn.commands,
                };
                let value = self.runtime.allocate(Ok(DinObject::UserFn(user_fn)))?;
                self.stack
                    .push(StackItem::Value(value));
                Ok(ControlFlow::Break(()))
            }
            Command::MakePending(n_args) => {
                self.eval_top(TailCallAvailability::Disallowed)?;
                let StackItem::Value(func) = self.stack.pop().unwrap() else {
                    unreachable!()
                };
                let Ok(func) = func else { todo!() };
                let arguments = self.stack.drain(self.stack.len() - n_args..).collect();
                self.stack.push(StackItem::Pending(Pending { func, arguments }));
                Ok(ControlFlow::Break(()))
            }
            Command::Attr(i) => {
                self.eval_top(TailCallAvailability::Disallowed)?;
                let StackItem::Value(val) = self.stack.pop().unwrap() else {
                    unreachable!()
                };
                match val {
                    Ok(item) => {
                        match item.as_ref(){
                            DinObject::Struct(fields) => {
                                let attr = &fields[*i];
                                self.stack.push(StackItem::Value(Ok(self.runtime.clone_ref(attr)?)));
                                Ok(ControlFlow::Break(()))
                            }
                            _ => todo!()
                        }
                    }
                    _ => todo!()
                }
            }
            Command::Struct(i) => {
                let mut fields = Vec::with_capacity(*i);
                for _ in 0..*i {
                    self.eval_top(TailCallAvailability::Disallowed)?;
                    let StackItem::Value(val) = self.stack.pop().unwrap() else {
                        unreachable!()
                    };
                    fields.push(val);
                }
                match fields.into_iter().collect(){
                    Err(_) => todo!(),
                    Ok(fields) => {
                        let struct_ = self.runtime.allocate(Ok(DinObject::Struct(fields)))?;
                        self.stack.push(StackItem::Value(struct_));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::Variant(i) => {
                todo!()
            }
            Command::VariantOpt(i) => {
                todo!()
            }
        }
    }
}


pub struct SystemRuntimeFrame<'p, 's, 'r>{
    pub stack: DinoStack<'s>,
    parent: &'p RuntimeFrame<'s, 'r>,
    tca: TailCallAvailability,
}

impl<'p, 's, 'r> SystemRuntimeFrame<'p, 's, 'r>{
    fn from_parent(parent: &'p RuntimeFrame<'s, 'r>, stack: DinoStack<'s>, tca: TailCallAvailability) -> Self {
        Self{
            stack,
            parent,
            tca,
        }
    }

    fn from_system_parent(parent: &'p SystemRuntimeFrame<'p, 's, 'r>, stack: DinoStack<'s>, tca: TailCallAvailability) -> Self {
        Self::from_parent(parent.parent, stack, parent.tca & tca)
    }

    pub fn runtime(&self) -> &'r Runtime<'s> {
        self.parent.runtime
    }

    pub fn eval_pop(&mut self) -> DinoResult<'s> {
        if let ControlFlow::Break(ret) = self.eval_pop_tca(TailCallAvailability::Disallowed)?{
            Ok(ret)
        } else {
            unreachable!()
        }
    }

    pub fn eval_pop_tca(&mut self, tca: TailCallAvailability) -> SourceFnResult<'s> {
        loop {
            match self.stack.pop().unwrap(){
                StackItem::Value(val) => return Ok(ControlFlow::Break(val)),
                StackItem::Pending(Pending {func, arguments}) => {
                    match func.as_ref(){
                        DinObject::UserFn(..) => {
                            if tca.is_allowed() && self.tca.is_allowed() && self.parent.user_fn.as_ref().is_some_and(|t| Arc::ptr_eq(&func.0, &t.0)){
                                return Ok(ControlFlow::Continue(arguments));
                            }
                            let func = self.runtime().clone_ref(&func)?;
                            let mut new_frame = self.parent.child(func);
                            let val = new_frame.exec_fn(arguments)?;
                            self.stack.push(StackItem::Value(val));
                        }
                        DinObject::SourceFn(source_fn) => {
                            let mut new_frame = SystemRuntimeFrame::from_system_parent(self, arguments, tca);
                            let ret = source_fn(&mut new_frame)?;
                            match ret{
                                ControlFlow::Break(val) => {
                                    self.stack.push(StackItem::Value(val));
                                }
                                cont @ ControlFlow::Continue(..) => {
                                    return Ok(cont);
                                }
                            }
                        }
                        DinObject::Tail => {
                            // this is a similar case to user functions, but we use the frame's user_fn
                            let Some(user_fn) = self.parent.user_fn.as_ref() else {
                                panic!("tail call with no user function")
                            };
                            if tca.is_allowed() && self.tca.is_allowed(){
                                return Ok(ControlFlow::Continue(arguments));
                            }
                            let func = self.runtime().clone_ref(&user_fn)?;
                            let mut new_frame = self.parent.child(func);
                            let val = new_frame.exec_fn(arguments)?;
                            self.stack.push(StackItem::Value(val));
                        }
                        _ => {panic!("unexpected function: {:?}", func)}
                    }
                }
            }
        }
    }
}