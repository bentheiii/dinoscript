use derive_more::Debug;

use std::{borrow::Cow, sync::Arc};

use crate::{bytecode::Command, maybe_owned::MaybeOwned};

#[derive(Debug)]
pub enum DinObject<'s> {
    Int(i64), // todo make a biginteger
    Float(f64),
    Bool(bool),
    Str(Cow<'s, str>), // todo use a fenced string
    Struct(Vec<Arc<DinObject<'s>>>),
    Variant(VariantObject<'s>),
    UserFn(UserFn<'s>),
    SourceFn(#[debug("<system_function>")] SourceFnFunc),
    Extended(Box<dyn ExtendedObject<'s>>),
    Tail,
}

pub type SourceFnFunc = Box<dyn for<'s> Fn(DinoStack<'s>) -> DinoResult<'s>>;

#[derive(Debug)]
pub struct UserFn<'s> {
    pub captures: Vec<Arc<DinObject<'s>>>,
    pub n_cells: usize,
    pub commands: &'s Vec<Command<'s>>,
}

impl<'s> UserFn<'s> {
    pub fn without_capture(n_cells: usize, commands: &'s Vec<Command<'s>>) -> Self {
        Self {
            captures: vec![],
            n_cells,
            commands,
        }
    }
}

#[derive(Debug)]
pub struct VariantObject<'s> {
    tag: usize,
    obj: Arc<DinObject<'s>>,
}

pub trait ExtendedObject<'s>: Debug {
    fn allocated_size(&self) -> usize;
}

pub type DinoValue<'s> = Result<Arc<DinObject<'s>>, ()>;
pub type DinoResult<'s> = Result<DinoValue<'s>, ()>;

#[derive(Debug, Clone)]
pub enum StackItem<'s> {
    Value(DinoValue<'s>),
    Pending(Pending<'s>),
}

pub type DinoStack<'s> = Vec<StackItem<'s>>;

#[derive(Debug, Clone)]
pub struct Pending<'s> {
    pub func: Arc<DinObject<'s>>,
    pub arguments: Vec<StackItem<'s>>,
}
