use std::{
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Mutex},
};

use crate::{
    bytecode::{Command, SourceId},
    dinobj::{
        Allocatable, AllocatedRef, BindBack, DinObject, DinoResult, DinoStack, DinoValue, ExtendedObject, Pending,
        PendingFunctor, SourceFnResult, StackItem, TailCall, TailCallAvailability, UserFn, VariantObject, VariantTag,
    },
    errors::{AllocatedErrRef, RuntimeError, RuntimeViolation},
    lib_objects::optional::{self},
    lib_objects::sequence::Sequence,
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
    none: Option<AllocatedRef<'s>>,
    nil: Option<AllocatedRef<'s>>,
}

unsafe impl Send for SharedRuntime<'_> {}

pub(crate) const REPORT_MEMORY_USAGE: bool = false;
pub(crate) const INTERN_CONSTS: bool = true;

impl<'s> SharedRuntime<'s> {
    fn clone_ref(&mut self, obj: Result<&AllocatedRef<'s>, &AllocatedErrRef<'s>>) -> DinoResult<'s> {
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        if REPORT_MEMORY_USAGE {
            println!("cloning ref ({} bytes) to {:?}", AllocatedRef::SIZE, obj);
            println!("total allocated space: {} bytes", self.allocated_space);
        }
        Ok(match obj {
            Ok(obj) => Ok(unsafe { obj.clone() }),
            Err(e) => Err(unsafe { e.clone() }),
        })
    }

    fn clone_true(&mut self) -> DinoResult<'s> {
        if !INTERN_CONSTS {
            unreachable!()
        }
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        let r = self.true_.as_ref().unwrap();
        Ok(Ok(unsafe { r.clone() }))
    }

    fn clone_false(&mut self) -> DinoResult<'s> {
        if !INTERN_CONSTS {
            unreachable!()
        }
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        let r = self.false_.as_ref().unwrap();
        Ok(Ok(unsafe { r.clone() }))
    }

    fn clone_none(&mut self) -> DinoResult<'s> {
        if !INTERN_CONSTS {
            unreachable!()
        }
        self.allocated_space += AllocatedRef::SIZE;
        // todo check against max size
        let r = self.none.as_ref().unwrap();
        Ok(Ok(unsafe { r.clone() }))
    }
}

// todo in the future, this shouldn't be an arc, objects should instead store a pointer to the runtime
#[derive(Clone)]
pub struct Runtime<'s>(Arc<Mutex<SharedRuntime<'s>>>);

impl<'s> Runtime<'s> {
    pub fn allocate(&self, obj: Result<DinObject<'s>, RuntimeError>) -> DinoResult<'s> {
        let size = obj.as_ref().map_or_else(|e| e.allocated_size(), |o| o.allocated_size());
        {
            let mut rt = self.0.lock().unwrap();
            rt.allocated_space += size;
            // todo check against max size
            if REPORT_MEMORY_USAGE {
                println!("allocating {} bytes for {:?}", size, obj);
                println!("total allocated space: {} bytes", rt.allocated_space);
            }
        }
        let ptr = obj
            .map(|o| o.allocate(self.clone()))
            .map_err(|e| e.allocate(self.clone()));
        Ok(ptr)
    }

    pub fn allocate_ext<T: ExtendedObject + 's>(&self, obj: T) -> DinoResult<'s> {
        let ptr: *const dyn ExtendedObject = Box::into_raw(Box::new(obj));
        self.allocate(Ok(DinObject::Extended(ptr)))
    }

    pub fn new() -> Self {
        let shared_runtime = SharedRuntime {
            allocated_space: 0,
            true_: None,
            false_: None,
            none: None,
            nil: None,
        };
        let ret = Self(Arc::new(Mutex::new(shared_runtime)));
        let true_ = ret.allocate(Ok(DinObject::Bool(true))).unwrap().unwrap();
        let false_ = ret.allocate(Ok(DinObject::Bool(false))).unwrap().unwrap();
        let nil = ret.allocate(Ok(DinObject::Struct(vec![]))).unwrap().unwrap();
        let none = ret
            .allocate(Ok(DinObject::Variant(VariantObject::new(
                optional::tag::NONE,
                ret.clone_ok_ref(&nil).unwrap(),
            ))))
            .unwrap()
            .unwrap();

        if INTERN_CONSTS {
            let mut rt = ret.0.lock().unwrap();
            rt.true_ = Some(true_);
            rt.false_ = Some(false_);
            rt.nil = Some(nil);
            rt.none = Some(none);
        }

        ret
    }

    pub fn bool(&self, b: bool) -> DinoResult<'s> {
        if INTERN_CONSTS {
            let mut rt = self.0.lock().unwrap();
            if b {
                rt.clone_true()
            } else {
                rt.clone_false()
            }
        } else {
            self.allocate(Ok(DinObject::Bool(b)))
        }
    }

    pub fn none(&self) -> DinoResult<'s> {
        if INTERN_CONSTS {
            let mut rt = self.0.lock().unwrap();
            rt.clone_none()
        } else {
            self.allocate(Ok(DinObject::Variant(VariantObject::new(
                optional::tag::NONE,
                self.nil()?.unwrap(),
            ))))
        }
    }

    fn nil(&self) -> DinoResult<'s> {
        if INTERN_CONSTS {
            // currently, this will only be called to create a fesh None if INTERN_CONSTS is false
            unreachable!()
        }
        self.allocate(Ok(DinObject::Struct(vec![])))
    }

    #[deprecated]
    pub fn clone_ok_ref(&self, obj: &AllocatedRef<'s>) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        // todo deprecate this
        let mut rt = self.0.lock().unwrap();
        Ok(rt.clone_ref(Ok(obj))?.unwrap())
    }

    pub fn clone_ref(&self, obj: Result<&AllocatedRef<'s>, &AllocatedErrRef<'s>>) -> DinoResult<'s> {
        let mut rt = self.0.lock().unwrap();
        rt.clone_ref(obj)
    }

    pub fn clone_value(&self, obj: &DinoValue<'s>) -> Result<DinoValue<'s>, RuntimeViolation> {
        match obj {
            Ok(obj) => {
                let obj = self.clone_ok_ref(obj)?;
                Ok(Ok(obj))
            }
            Err(e) => todo!(),
        }
    }

    pub fn deallocate(&self, size: usize) {
        let mut rt = self.0.lock().unwrap();
        rt.allocated_space -= size;
        if REPORT_MEMORY_USAGE {
            println!("total allocated space: {} bytes", rt.allocated_space);
        }
    }
}

impl Default for Runtime<'_> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Sources<'s> {
    sources: HashMap<SourceId, Vec<DinoValue<'s>>>,
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

    pub fn add_source(&mut self, source_id: SourceId, source: Vec<DinoValue<'s>>) {
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
                    let Some(StackItem::Pending(Pending { functor, mut arguments })) = self.stack.pop() else {
                        unreachable!()
                    };
                    match functor {
                        PendingFunctor::Function(func) => {
                            let func_ref = if let DinObject::BindBack(bind_back) = func.as_ref() {
                                arguments.extend(
                                    bind_back
                                        .defaults
                                        .iter()
                                        .map(|d| Ok(StackItem::Value(Ok(self.runtime.clone_ok_ref(d)?))))
                                        .collect::<Result<Vec<_>, _>>()?,
                                );
                                &bind_back.func
                            } else {
                                &func
                            };
                            // note that tail-call-optimization is not allowed here, only from system functions
                            match func_ref.as_ref() {
                                DinObject::UserFn(_) => {
                                    // todo the frame could hold a ref to the user_fn instead of cloning the captures
                                    let func = self.runtime.clone_ok_ref(func_ref)?;
                                    let mut child_frame = self.child(func);
                                    let val = child_frame.exec_self_fn(arguments)?;
                                    self.stack.push(StackItem::Value(val));
                                }
                                DinObject::SourceFn(source_fn) => {
                                    let mut frame = SystemRuntimeFrame::from_parent(self, arguments, tca);
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
                                    let func = self.runtime.clone_ok_ref(user_fn)?;
                                    let mut new_frame = self.child(func);
                                    let val = new_frame.exec_self_fn(arguments)?;
                                    self.stack.push(StackItem::Value(val));
                                }
                                other => {
                                    unreachable!("attempted to call object {:?}", other)
                                    // err
                                }
                            }
                        }
                        PendingFunctor::VariantAccess(expected_tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            self.eval_top(TailCallAvailability::Disallowed)?;
                            let StackItem::Value(arg) = self.stack.pop().unwrap() else {
                                unreachable!()
                            };
                            match arg {
                                Ok(arg) => match arg.as_ref() {
                                    DinObject::Variant(variant) => {
                                        if variant.tag() == expected_tag {
                                            self.stack.push(StackItem::Value(Ok(self
                                                .runtime
                                                .clone_ok_ref(variant.obj())?)));
                                        } else {
                                            todo!("expected tag {:?}, got {:?}", expected_tag, variant.tag())
                                        }
                                    }
                                    _ => todo!(),
                                },
                                Err(e) => {
                                    todo!("handle error: {}", e)
                                }
                            }
                        }
                        PendingFunctor::VariantAccessSafe(expected_tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            self.eval_top(TailCallAvailability::Disallowed)?;
                            let StackItem::Value(arg) = self.stack.pop().unwrap() else {
                                unreachable!()
                            };
                            let val = match arg {
                                Ok(arg) => match arg.as_ref() {
                                    DinObject::Variant(variant) => {
                                        if variant.tag() == expected_tag {
                                            // todo we can make an optimization here, if the tag is already 0
                                            self.runtime.allocate(Ok(DinObject::Variant(VariantObject::new(
                                                optional::tag::SOME,
                                                self.runtime.clone_ok_ref(variant.obj())?,
                                            ))))?
                                        } else {
                                            self.runtime.none()?
                                        }
                                    }
                                    _ => todo!(),
                                },
                                Err(e) => {
                                    todo!("handle error: {}", e)
                                }
                            };
                            self.stack.push(StackItem::Value(val));
                        }
                        PendingFunctor::Attr(i) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            self.eval_top(TailCallAvailability::Disallowed)?;
                            let StackItem::Value(arg) = self.stack.pop().unwrap() else {
                                unreachable!()
                            };
                            match arg {
                                Ok(item) => match item.as_ref() {
                                    DinObject::Struct(fields) => {
                                        let Some(attr) = &fields.get(i) else {
                                            panic!("struct has no attribute at index {}, has fields: {:?}", i, fields)
                                        };
                                        self.stack.push(StackItem::Value(Ok(self.runtime.clone_ok_ref(attr)?)));
                                    }
                                    _ => todo!(),
                                },
                                _ => todo!(),
                            }
                        }
                        PendingFunctor::Struct => {
                            let mut fields = Vec::with_capacity(arguments.len());
                            for arg in arguments.into_iter().rev() {
                                self.stack.push(arg);
                                self.eval_top(TailCallAvailability::Disallowed)?;
                                let StackItem::Value(val) = self.stack.pop().unwrap() else {
                                    unreachable!()
                                };
                                let Ok(val) = val else { todo!() };
                                fields.push(val);
                            }
                            let struct_ = self.runtime.allocate(Ok(DinObject::Struct(fields)))?;
                            self.stack.push(StackItem::Value(struct_));
                        }
                        PendingFunctor::Variant(tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            self.eval_top(TailCallAvailability::Disallowed)?;
                            let StackItem::Value(arg) = self.stack.pop().unwrap() else {
                                unreachable!()
                            };
                            let Ok(arg) = arg else { todo!() };
                            let obj = self.runtime.allocate(Ok(DinObject::Variant(VariantObject::new(tag, arg))))?;
                            self.stack.push(StackItem::Value(obj));
                        }
                    }
                }
                None => {
                    todo!("the stack is empty!") // very error
                }
            }
        }
    }

    fn get_capture<'a>(&'a mut self, i: usize) -> Result<AllocatedRef<'s>, RuntimeViolation> {
        match self.user_fn.as_ref().unwrap().as_ref() {
            DinObject::UserFn(user_fn) => self.runtime.clone_ok_ref(&user_fn.captures[i]),
            _ => unreachable!(),
        }
    }

    pub fn exec_self_fn(&mut self, mut args: Vec<StackItem<'s>>) -> DinoResult<'s> {
        'tail: loop {
            let user_fn = self.user_fn.as_ref().unwrap();
            match user_fn.as_ref() {
                DinObject::UserFn(user_fn) => {
                    self.stack.extend(args);
                    for command in user_fn.commands.iter() {
                        if let ControlFlow::Continue(new_args) = self.execute(command)? {
                            args = new_args;
                            self.stack.clear();
                            // note: for now we also clear all the runtime cells, this is not necessary
                            self.cells = Self::empty_cells(self.cells.len());
                            continue 'tail;
                        }
                    }
                    let ret = self.stack.pop().unwrap();
                    let StackItem::Value(val) = ret else { todo!() };
                    break Ok(val);
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn execute<'a: 's>(
        &mut self,
        command: &'a Command<'s>,
    ) -> Result<ControlFlow<(), TailCall<'s>>, RuntimeViolation> {
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
                    Err(e) => {
                        todo!("handle error: {}", e) // exit the current frame
                    }
                }
            }
            Command::EvalTop => self.eval_top(TailCallAvailability::Allowed),
            Command::PushInt(i) => {
                let Ok(i) = (*i).try_into() else { todo!() };
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Int(i)))?));
                Ok(ControlFlow::Break(()))
            }
            Command::PushFloat(f) => {
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Float(*f)))?));
                Ok(ControlFlow::Break(()))
            }
            Command::PushBool(b) => {
                self.stack.push(StackItem::Value(self.runtime.bool(*b)?));
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
                        self.stack
                            .push(StackItem::Value(DinoValue::Ok(self.runtime.clone_ok_ref(val)?)));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::PushFromSource(pfs) => {
                let source_fn = self
                    .runtime
                    .clone_ref(self.get_sources().sources[pfs.source][pfs.id].as_ref())?;
                self.stack.push(StackItem::Value(source_fn));
                Ok(ControlFlow::Break(()))
            }
            Command::PushFromCapture(i) => {
                let mut val = self.get_capture(*i)?;
                if matches!(val.as_ref(), DinObject::Tail) {
                    // instead use the current frame's user_fn
                    let Some(user_fn) = self.user_fn.as_ref() else {
                        todo!("tail call with no user function")
                    };
                    val = self.runtime.clone_ok_ref(user_fn)?;
                }
                self.stack.push(StackItem::Value(DinoValue::Ok(val)));
                Ok(ControlFlow::Break(()))
            }
            Command::PushGlobal(i) => {
                let cell = &self.get_global_frame().cells[*i];
                match cell {
                    RuntimeCell::Uninitialized => todo!(), // err
                    RuntimeCell::Value(val) => {
                        let new_val = self.runtime.clone_ok_ref(val)?;
                        self.stack.push(StackItem::Value(DinoValue::Ok(new_val)));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::PushTail => {
                // todo tail can be a static/outsize of the arc-logic
                self.stack
                    .push(StackItem::Value(self.runtime.allocate(Ok(DinObject::Tail))?));
                Ok(ControlFlow::Break(()))
            }
            Command::MakeFunction(mfn) => {
                let captures = {
                    let mut captures = Vec::with_capacity(mfn.n_captures);
                    for _ in 0..mfn.n_captures {
                        self.eval_top(TailCallAvailability::Disallowed)?;
                        let StackItem::Value(Ok(val)) = self.stack.pop().unwrap() else {
                            todo!()
                        };
                        captures.push(val);
                    }
                    captures.reverse();
                    captures
                };
                let user_fn = UserFn::new(mfn.name.clone(), captures, mfn.n_cells, &mfn.commands);
                let value = self.runtime.allocate(Ok(DinObject::UserFn(user_fn)))?;
                self.stack.push(StackItem::Value(value));
                Ok(ControlFlow::Break(()))
            }
            Command::MakePending(n_args) => {
                self.eval_top(TailCallAvailability::Disallowed)?;
                let StackItem::Value(func) = self.stack.pop().unwrap() else {
                    unreachable!()
                };
                let Ok(func) = func else { todo!() };
                let arguments = self.stack.drain(self.stack.len() - n_args..).collect();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::Function(func),
                    arguments,
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::Attr(i) => {
                let arg = self.stack.pop().unwrap();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::Attr(*i),
                    arguments: vec![arg],
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::Struct(i) => {
                let pending_fields = self.stack.drain(self.stack.len() - *i..).collect();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::Struct,
                    arguments: pending_fields,
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::Variant(tag) => {
                let arg = self.stack.pop().unwrap();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::Variant(VariantTag::new(*tag)),
                    arguments: vec![arg],
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::VariantAccess(expected_tag) => {
                let arg = self.stack.pop().unwrap();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::VariantAccess(VariantTag::new(*expected_tag)),
                    arguments: vec![arg],
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::VariantAccessOpt(expected_tag) => {
                let arg = self.stack.pop().unwrap();
                self.stack.push(StackItem::Pending(Pending {
                    functor: PendingFunctor::VariantAccessSafe(VariantTag::new(*expected_tag)),
                    arguments: vec![arg],
                }));
                Ok(ControlFlow::Break(()))
            }
            Command::Array(i) => {
                // todo this should be pending too
                let mut items = Vec::with_capacity(*i);
                for _ in 0..*i {
                    self.eval_top(TailCallAvailability::Disallowed)?;
                    let StackItem::Value(val) = self.stack.pop().unwrap() else {
                        unreachable!()
                    };
                    items.push(val);
                }
                match items.into_iter().collect() {
                    Err(_) => todo!(),
                    Ok(items) => {
                        let array = self.runtime.allocate_ext(Sequence::new_array(items))?;
                        self.stack.push(StackItem::Value(array));
                        Ok(ControlFlow::Break(()))
                    }
                }
            }
            Command::BindBack(n_args) => {
                self.eval_top(TailCallAvailability::Disallowed)?;
                let StackItem::Value(func) = self.stack.pop().unwrap() else {
                    unreachable!()
                };
                let Ok(func) = func else { todo!() };
                let mut defaults = Vec::with_capacity(*n_args);
                for _ in 0..*n_args {
                    // TODO: i think that this eval_top is unessary, we could just pop the values
                    self.eval_top(TailCallAvailability::Disallowed)?;
                    let StackItem::Value(val) = self.stack.pop().unwrap() else {
                        unreachable!()
                    };
                    let Ok(val) = val else { todo!() };
                    defaults.push(val);
                }
                defaults.reverse();
                let bind_back = BindBack::new(func, defaults);
                let obj = self.runtime.allocate(Ok(DinObject::BindBack(bind_back)))?;
                self.stack.push(StackItem::Value(obj));
                Ok(ControlFlow::Break(()))
            }
        }
    }
}

pub struct SystemRuntimeFrame<'p, 's, 'r> {
    pub stack: DinoStack<'s>,
    parent: &'p RuntimeFrame<'s, 'r>,
    tca: TailCallAvailability,
}

impl<'p, 's, 'r> SystemRuntimeFrame<'p, 's, 'r> {
    pub(crate) fn from_parent(
        parent: &'p RuntimeFrame<'s, 'r>,
        stack: DinoStack<'s>,
        tca: TailCallAvailability,
    ) -> Self {
        Self { stack, parent, tca }
    }

    fn from_system_parent(
        parent: &'p SystemRuntimeFrame<'p, 's, 'r>,
        stack: DinoStack<'s>,
        tca: TailCallAvailability,
    ) -> Self {
        Self::from_parent(parent.parent, stack, parent.tca & tca)
    }

    pub fn runtime(&self) -> &'r Runtime<'s> {
        self.parent.runtime
    }

    pub fn eval_pop(&mut self) -> DinoResult<'s> {
        if let ControlFlow::Break(ret) = self.eval_pop_tca(TailCallAvailability::Disallowed)? {
            Ok(ret)
        } else {
            self.runtime().allocate(Err("stack is unexpectedly empty".into()))
        }
    }

    pub fn call(&self, func: &AllocatedRef<'s>, arguments: &[DinoValue<'s>]) -> DinoResult<'s> {
        let (func_ref, arguments) = if let DinObject::BindBack(bind_back) = func.as_ref() {
            (
                &bind_back.func,
                bind_back
                    .defaults
                    .iter()
                    .map(|d| Ok(StackItem::Value(Ok(self.runtime().clone_ok_ref(d)?))))
                    .chain(
                        arguments
                            .iter()
                            .map(|a| Ok(StackItem::Value(self.runtime().clone_value(a)?))),
                    )
                    .collect::<Result<Vec<_>, _>>()?,
            )
        } else {
            (
                func,
                arguments
                    .iter()
                    .map(|a| Ok(StackItem::Value(self.runtime().clone_value(a)?)))
                    .collect::<Result<Vec<_>, _>>()?,
            )
        };
        match func_ref.as_ref() {
            DinObject::UserFn(..) => {
                let func = self.runtime().clone_ok_ref(func_ref)?;
                let mut new_frame = self.parent.child(func);
                new_frame.exec_self_fn(arguments)
            }
            DinObject::SourceFn(source_fn) => {
                let mut new_frame =
                    SystemRuntimeFrame::from_system_parent(self, arguments, TailCallAvailability::Disallowed);
                let ret = source_fn(&mut new_frame)?;
                match ret {
                    ControlFlow::Break(val) => Ok(val),
                    ControlFlow::Continue(..) => {
                        unreachable!()
                    }
                }
            }
            DinObject::Tail => {
                // this is a similar case to user functions, but we use the frame's user_fn
                let Some(user_fn) = self.parent.user_fn.as_ref() else {
                    panic!("tail call with no user function")
                };
                let func = self.runtime().clone_ok_ref(user_fn)?;
                let mut new_frame = self.parent.child(func);
                new_frame.exec_self_fn(arguments)
            }
            _ => {
                panic!("unexpected function: {:?}", func_ref)
            }
        }
    }

    pub fn eval_pop_tca(&mut self, tca: TailCallAvailability) -> SourceFnResult<'s> {
        loop {
            match self.stack.pop().unwrap() {
                StackItem::Value(val) => return Ok(ControlFlow::Break(val)),
                StackItem::Pending(Pending { functor, mut arguments }) => {
                    match functor {
                        PendingFunctor::Function(func) => {
                            let func_ref = if let DinObject::BindBack(bind_back) = func.as_ref() {
                                arguments = bind_back
                                    .defaults
                                    .iter()
                                    .map(|d| Ok(StackItem::Value(Ok(self.runtime().clone_ok_ref(d)?))))
                                    .chain(arguments.into_iter().map(Ok))
                                    .collect::<Result<Vec<_>, _>>()?;
                                &bind_back.func
                            } else {
                                &func
                            };
                            match func_ref.as_ref() {
                                DinObject::UserFn(..) => {
                                    if tca.is_allowed()
                                        && self.tca.is_allowed()
                                        && self.parent.user_fn.as_ref().is_some_and(|t| Arc::ptr_eq(&func.0, &t.0))
                                    {
                                        return Ok(ControlFlow::Continue(arguments));
                                    }
                                    let func = self.runtime().clone_ok_ref(func_ref)?;
                                    let mut new_frame = self.parent.child(func);
                                    let val = new_frame.exec_self_fn(arguments)?;
                                    self.stack.push(StackItem::Value(val));
                                }
                                DinObject::SourceFn(source_fn) => {
                                    let mut new_frame = SystemRuntimeFrame::from_system_parent(self, arguments, tca);
                                    let ret = source_fn(&mut new_frame)?;
                                    match ret {
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
                                    if tca.is_allowed() && self.tca.is_allowed() {
                                        return Ok(ControlFlow::Continue(arguments));
                                    }
                                    let func = self.runtime().clone_ok_ref(user_fn)?;
                                    let mut new_frame = self.parent.child(func);
                                    let val = new_frame.exec_self_fn(arguments)?;
                                    self.stack.push(StackItem::Value(val));
                                }
                                _ => {
                                    panic!("unexpected function: {:?}", func_ref)
                                }
                            }
                        }
                        PendingFunctor::VariantAccess(expected_tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            let arg = self.eval_pop()?;
                            match arg {
                                Ok(arg) => match arg.as_ref() {
                                    DinObject::Variant(variant) => {
                                        if variant.tag() == expected_tag {
                                            self.stack.push(StackItem::Value(Ok(self
                                                .runtime()
                                                .clone_ok_ref(variant.obj())?)));
                                        } else {
                                            todo!("expected tag {:?}, got {:?}", expected_tag, variant.tag())
                                        }
                                    }
                                    _ => todo!(),
                                },
                                Err(e) => {
                                    todo!("handle error: {}", e)
                                }
                            }
                        }
                        PendingFunctor::VariantAccessSafe(expected_tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            let arg = self.eval_pop()?;
                            let val = match arg {
                                Ok(arg) => match arg.as_ref() {
                                    DinObject::Variant(variant) => {
                                        if variant.tag() == expected_tag {
                                            // todo we can make an optimization here, if the tag is already 0
                                            self.runtime().allocate(Ok(DinObject::Variant(VariantObject::new(
                                                optional::tag::SOME,
                                                self.runtime().clone_ok_ref(variant.obj())?,
                                            ))))?
                                        } else {
                                            self.runtime().none()?
                                        }
                                    }
                                    _ => todo!("expected variant, got {:?}", arg),
                                },
                                Err(e) => {
                                    todo!("handle error: {}", e)
                                }
                            };
                            self.stack.push(StackItem::Value(val));
                        }
                        PendingFunctor::Attr(i) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            let arg = self.eval_pop()?;
                            match arg {
                                Ok(item) => match item.as_ref() {
                                    DinObject::Struct(fields) => {
                                        let Some(attr) = &fields.get(i) else {
                                            panic!("struct has no attribute at index {}, has fields: {:?}", i, fields)
                                        };
                                        self.stack.push(StackItem::Value(Ok(self.runtime().clone_ok_ref(attr)?)));
                                    }
                                    _ => todo!(),
                                },
                                _ => todo!(),
                            }
                        }
                        PendingFunctor::Struct => {
                            let mut fields = Vec::with_capacity(arguments.len());
                            for arg in arguments.into_iter().rev() {
                                self.stack.push(arg);
                                let val = self.eval_pop()?;
                                match val {
                                    Ok(val) => fields.push(val),
                                    Err(e) => {
                                        todo!("handle error: {}", e)
                                    }
                                }
                            }
                            let obj = self.runtime().allocate(Ok(DinObject::Struct(fields)))?;
                            self.stack.push(StackItem::Value(obj));
                        }
                        PendingFunctor::Variant(tag) => {
                            debug_assert!(arguments.len() == 1);
                            self.stack.extend(arguments);
                            let Ok(arg) = self.eval_pop()? else {todo!()};
                            let obj = self.runtime().allocate(Ok(DinObject::Variant(VariantObject::new(
                                tag,
                                arg,
                            ))))?;
                            self.stack.push(StackItem::Value(obj));
                        }
                    }
                }
            }
        }
    }
}
