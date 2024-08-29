use std::{borrow::Cow, fmt::Debug, sync::Arc};

use crate::bytecode::Command;

#[derive(Debug)]
pub(crate) enum DinObject<'s> {
    Int(i64), // todo make a biginteger
    Float(f64),
    Bool(bool),
    Str(Cow<'s, str>), // todo use a fenced string
    Struct(Vec<Arc<DinObject<'s>>>),
    Variant(VariantObject<'s>),
    UserFn(UserFn<'s>),
    SourceFn(SourceFn),
    Extended(Box<dyn ExtendedObject<'s>>),
    Tail,
}

pub(crate) trait SourceFnTrait: Debug {
    fn call<'s>(&self, stack: DinoStack<'s>) -> DinObject<'s>;
}

pub(crate) type SourceFn = Box<dyn SourceFnTrait>;

#[derive(Debug)]
pub(crate) struct UserFn<'s> {
    pub(crate) captures: Vec<Arc<DinObject<'s>>>,
    pub(crate) n_cells: usize,
    pub(crate) commands: &'s Vec<Command<'s>>,
}

#[derive(Debug)]
pub(crate) struct VariantObject<'s> {
    tag: usize,
    obj: Arc<DinObject<'s>>,
}

pub(crate) trait ExtendedObject<'s>: Debug {
    fn allocated_size(&self) -> usize;
}

pub(crate) type DinoValue<'s> = Result<Arc<DinObject<'s>>, ()>;

#[derive(Debug, Clone)]
pub(crate) enum StackItem<'s> {
    Value(DinoValue<'s>),
    Pending(Pending<'s>),
}

pub(crate) type DinoStack<'s> = Vec<StackItem<'s>>;

#[derive(Debug, Clone)]
pub(crate) struct Pending<'s> {
    pub(crate) func: Arc<DinObject<'s>>,
    pub(crate) arguments: Vec<StackItem<'s>>,
}
