use std::sync::Arc;

use crate::{bytecode::Command, dinobj::{DinObject, DinoStack, DinoValue, Pending, SourceFn, StackItem, UserFn}};

#[derive(Debug, Clone)]
pub(crate) enum RuntimeCell<'s>{
    Uninitialized,
    Value(Arc<DinObject<'s>>),
}

pub(crate) struct Runtime<'s>{
    stack: DinoStack<'s>,
}

pub(crate) struct Sources<'s>{
    sources: Vec<Vec<Arc<DinObject<'s>>>>,
}

impl<'s> Sources<'s> {
    fn new() -> Self{
        Self{
            sources: Vec::new(),
        }
    }
    
}

pub(crate) struct RuntimeFrame<'s, 'r>{
    stack: DinoStack<'s>,

    captures: Vec<Arc<DinObject<'s>>>, // todo can we keep this a as a reference somehow?
    pub(crate) cells: Vec<RuntimeCell<'s>>,
    global_frame: Option<&'r RuntimeFrame<'s, 'r>>,
    sources: Option<Sources<'s>>,
}

impl <'s, 'r> RuntimeFrame<'s, 'r>{
    pub(crate) fn root(n_cells: usize, ) -> Self{
        Self{
            stack: Vec::new(),
            captures: Vec::new(),
            cells: vec![RuntimeCell::Uninitialized; n_cells],
            global_frame: None,
            sources: Some(Sources::new()),
        }
    }

    fn is_root(&self) -> bool{
        self.global_frame.is_none()
    }

    fn get_global_frame<'a>(&'a self) -> &'a RuntimeFrame<'s, 'r>{
        self.global_frame.unwrap_or(self)
    }

    fn get_sources(&self) -> &Sources<'s>{
        self.get_global_frame().sources.as_ref().unwrap()
    }

    pub(crate) fn child(&'r self, captures: Vec<Arc<DinObject<'s>>>, n_cells: usize) -> Self{
        Self{
            stack: Vec::new(),
            captures,
            cells: vec![RuntimeCell::Uninitialized; n_cells],
            global_frame: Some(self.get_global_frame()),
            sources: None,
        }
    }

    fn eval_top(&mut self)->Result<(), ()>{
        loop {
            match self.stack.last(){
                Some(StackItem::Value(_)) => {
                    return Ok(())
                }
                Some(StackItem::Pending(..)) => {
                    let Some(StackItem::Pending(pending)) = self.stack.pop() else {unreachable!()};
                    match pending.func.as_ref(){
                        DinObject::UserFn(user_fn) => {
                            let mut child_frame = self.child(user_fn.captures.clone(), user_fn.n_cells);
                            child_frame.stack.extend(pending.arguments.iter().cloned());
                            for command in user_fn.commands.iter(){
                                child_frame.execute(command)?;
                            }
                            let StackItem::Value(val) = child_frame.stack.pop().unwrap() else {todo!()};
                            self.stack.push(StackItem::Value(val));
                        }
                        DinObject::SourceFn(..) => {
                            todo!()
                        }
                        _ => {
                            todo!() // err
                        }
                    }
                }
                None => {
                    todo!() // very error
                }
            }
        }
    }

    pub(crate) fn execute(&mut self, command: &'s Command<'s>)->Result<(), ()>{
        match command{
            Command::PopToCell(i) => {
                debug_assert!(matches!(self.cells[*i], RuntimeCell::Uninitialized));
                self.eval_top()?;
                let StackItem::Value(val) = self.stack.pop().unwrap() else {todo!()};
                match val{
                    Ok(val) => {
                        self.cells[*i] = RuntimeCell::Value(val);
                        Ok(())
                    }
                    Err(_) => {
                        todo!() // exit the current frame
                    }
                }
            }
            Command::PushInt(i) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::Int(*i)))));
                Ok(())
            }
            Command::PushFloat(f) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::Float(*f)))));
                Ok(())
            }
            Command::PushBool(b) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::Bool(*b)))));
                Ok(())
            }
            Command::PushString(s) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::Str(s.clone())))));
                Ok(())
            }
            Command::PushFromCell(i) => {
                match &self.cells[*i]{
                    RuntimeCell::Uninitialized => todo!(), // err
                    RuntimeCell::Value(val) => {
                        self.stack.push(StackItem::Value(DinoValue::Ok(val.clone())));
                        Ok(())
                    }
                }
            }
            Command::PushFromSource(pfs) => {
                let source_fn = self.get_sources().sources[pfs.source][pfs.id].clone();
                self.stack.push(StackItem::Value(DinoValue::Ok(source_fn)));
                Ok(())
            }
            Command::PushFromCapture(i) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(self.captures[*i].clone())));
                Ok(())
            }
            Command::PushGlobal(i) => {
                let val = self.get_global_frame().cells[*i].clone();
                match val{
                    RuntimeCell::Uninitialized => todo!(), // err
                    RuntimeCell::Value(val) => {
                        self.stack.push(StackItem::Value(DinoValue::Ok(val)));
                        Ok(())
                    }
                }
            }
            Command::PushTail(i) => {
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::Tail))));
                Ok(())
            }
            Command::MakeFunction(mfn) => {
                let captures = {
                    let mut captures = Vec::with_capacity(mfn.n_captures);
                    for _ in 0..mfn.n_captures{
                        self.eval_top()?; // NOTE: i think this is actually never needed, aren't captures always retrieved from cells?
                        let StackItem::Value(Ok(val)) = self.stack.pop().unwrap() else {todo!()};
                        captures.push(val);
                    }
                    captures
                };
                let user_fn = UserFn{
                    captures,
                    n_cells: mfn.n_cells,
                    commands: &mfn.commands,
                };
                self.stack.push(StackItem::Value(DinoValue::Ok(Arc::new(DinObject::UserFn(user_fn)))));
                Ok(())
            }
            Command::MakePending(n_args) => {
                self.eval_top()?;
                let StackItem::Value(func) = self.stack.pop().unwrap() else {todo!()};
                let Ok(func) = func else {todo!()};
                let arguments = self.stack.drain(self.stack.len()-n_args..).collect();
                self.stack.push(StackItem::Pending(Pending{
                    func,
                    arguments,
                }));
                Ok(())
            }
            Command::Attr(i) => {
                todo!()
            }
            Command::Struct(i) => {
                todo!()
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