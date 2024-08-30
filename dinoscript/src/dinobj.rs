use derive_more::Debug;

use std::{borrow::Cow, sync::Arc};

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
    SourceFn(#[debug("<system_function>")]SourceFnFunc),
    Extended(Box<dyn ExtendedObject<'s>>),
    Tail,
}

pub(crate) type SourceFnFunc = Box<dyn for<'s> Fn(DinoStack<'s>) -> DinoResult<'s>>;

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
pub(crate) type DinoResult<'s> = Result<DinoValue<'s>, ()>;

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
