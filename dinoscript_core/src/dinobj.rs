use derive_more::Debug;

use std::{borrow::Cow, mem::size_of, ops::{BitAnd, ControlFlow, Deref}, sync::Arc};

use crate::{bytecode::Command, errors::{AllocatedRuntimeError, RuntimeError, RuntimeViolation}, maybe_owned::MaybeOwned, runtime::{Runtime, RuntimeFrame, SystemRuntimeFrame, REPORT_MEMORY_USAGE}};

#[derive(Debug)]
pub enum DinObject<'s> {
    Int(i64), // todo make a biginteger
    Float(f64),
    Bool(bool),
    Str(Cow<'s, str>), // todo use a fenced string
    Struct(Vec<AllocatedRef<'s>>),
    Variant(VariantObject<'s>),
    UserFn(UserFn<'s>),
    SourceFn(#[debug("<system_function>")] SourceFnFunc),
    Extended(Box<dyn ExtendedObject<'s>>),
    Tail,
}

impl<'s> DinObject<'s>{
    pub fn allocated_size(&self) -> usize {
        size_of::<Self>() + match self {
            Self::Extended(e) => e.allocated_size(),
            Self::Str(s) => s.len(),
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TailCallAvailability {
    Allowed,
    Disallowed,
}

impl TailCallAvailability {
    pub fn is_allowed(&self) -> bool {
        matches!(self, Self::Allowed)
    }
}

impl BitAnd for TailCallAvailability {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Allowed, Self::Allowed) => Self::Allowed,
            _ => Self::Disallowed,
        }
    }
}

pub type TailCall<'s> = Vec<StackItem<'s>>;
pub type SourceFnResult<'s> = Result<ControlFlow<DinoValue<'s>, TailCall<'s>>, RuntimeViolation>;

pub type SourceFnFunc = Box<dyn for<'s, 'r> Fn(&mut SystemRuntimeFrame<'_, 's, '_>) -> SourceFnResult<'s>>;

#[derive(Debug)]
pub struct UserFn<'s> {
    pub captures: Vec<AllocatedRef<'s>>,
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
    obj: AllocatedRef<'s>,
}

impl<'s> VariantObject<'s> {
    pub fn new(tag: usize, obj: AllocatedRef<'s>) -> Self {
        Self { tag, obj }
    }
}

pub trait ExtendedObject<'s>: Debug {
    fn allocated_size(&self) -> usize;
}


pub type DinoValue<'s> = Result<AllocatedRef<'s>, AllocatedRuntimeError>;
pub type DinoResult<'s> = Result<DinoValue<'s>, RuntimeViolation>;

#[derive(Debug)]
pub enum StackItem<'s> {
    Value(DinoValue<'s>),
    Pending(Pending<'s>),
}

pub type DinoStack<'s> = Vec<StackItem<'s>>;

#[derive(Debug)]
pub struct Pending<'s> {
    pub func: AllocatedRef<'s>,
    pub arguments: Vec<StackItem<'s>>,
}

#[derive(Debug)]
pub struct AllocatedObject<'s>{
    value: DinObject<'s>,
    size: usize,
    #[debug(skip)]
    runtime: Runtime<'s>,
}

impl<'s> AllocatedObject<'s>{
    pub fn new(value: DinObject<'s>, runtime: Runtime<'s>) -> Self {
        let size = value.allocated_size();
        Self{value, size, runtime}
    }
}

impl<'s> Drop for AllocatedObject<'s>{
    fn drop(&mut self) {
        if REPORT_MEMORY_USAGE {
            println!("Deallocating ({} bytes) {:?}", self.size, self.value);
        }
        self.runtime.deallocate(self.size);
    }
}

#[derive(Debug)]
pub struct AllocatedRef<'s>(pub Arc<AllocatedObject<'s>>);

impl<'s> AllocatedRef<'s>{
    pub const SIZE: usize = size_of::<AllocatedRef>();
    pub fn new(obj: Arc<AllocatedObject<'s>>) -> Self {
        Self(obj)
    }

    // this is purposefully not an implementation of Clone
    // so that we can control the allocation and deallocation
    pub(crate) unsafe fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'s> Drop for AllocatedRef<'s>{
    fn drop(&mut self) {
        if REPORT_MEMORY_USAGE {
            println!("Deallocating ref ({} bytes, {} refs remaining) {:?}", Self::SIZE, Arc::strong_count(&self.0)-1, self.0.value);
        }
        self.0.runtime.deallocate(Self::SIZE);
    }
}

impl<'s> Deref for AllocatedRef<'s>{
    type Target = DinObject<'s>;

    fn deref(&self) -> &Self::Target {
        &self.0.value
    }
}

impl<'s> AsRef<DinObject<'s>> for AllocatedRef<'s> {
    fn as_ref(&self) -> &DinObject<'s> {
        &self.0.as_ref().value
    }
}