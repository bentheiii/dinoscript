use derive_more::Debug;

use std::{
    borrow::Cow,
    mem::size_of,
    ops::{BitAnd, ControlFlow, Deref},
    sync::Arc,
};

use crate::{
    bytecode::Command,
    errors::{AllocatedErrRef, RuntimeViolation},
    runtime::{Runtime, SystemRuntimeFrame, REPORT_MEMORY_USAGE},
};

#[derive(Debug)]
pub enum DinObject<'s> {
    Int(i64), // todo make a biginteger
    /// is guanteed to never be a NaN/Inf
    Float(f64),
    Bool(bool),
    Str(Cow<'s, str>), // todo use a fenced string
    Struct(Vec<AllocatedRef<'s>>),
    Variant(VariantObject<'s>),
    UserFn(UserFn<'s>),
    SourceFn(#[debug("<system_function>")] SourceFnFunc),
    BindBack(BindBack<'s>),
    Extended(*const (dyn ExtendedObject + 's)),
    /// This is a special object, indicating that the frame's used function should be used as a value
    Tail,
}

unsafe impl Send for DinObject<'_> {}

impl<'s> DinObject<'s> {
    fn extra_allocated_size(&self) -> usize {
        match self {
            Self::Extended(e) => unsafe { (**e).allocated_size() },
            Self::Str(s) => s.len(),
            _ => 0,
        }
    }
}

impl Drop for DinObject<'_> {
    fn drop(&mut self) {
        if let Self::Extended(e) = self {
            unsafe {
                drop(Box::from_raw(*e as *mut (dyn ExtendedObject)));
            };
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
pub struct BindBack<'s> {
    pub func: AllocatedRef<'s>,
    pub defaults: Vec<AllocatedRef<'s>>,
}

impl<'s> BindBack<'s> {
    pub fn new(func: AllocatedRef<'s>, defaults: Vec<AllocatedRef<'s>>) -> Self {
        Self { func, defaults }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariantTag(usize);

impl VariantTag {
    pub const fn new(tag: usize) -> Self {
        Self(tag)
    }
}

#[derive(Debug)]
pub struct VariantObject<'s> {
    tag: VariantTag,
    obj: AllocatedRef<'s>,
}

impl<'s> VariantObject<'s> {
    pub fn new(tag: impl Into<VariantTag>, obj: AllocatedRef<'s>) -> Self {
        Self { tag: tag.into(), obj }
    }

    pub fn tag(&self) -> VariantTag {
        self.tag
    }

    pub fn obj(&self) -> &AllocatedRef<'s> {
        &self.obj
    }
}

pub trait ExtendedObject: Debug {
    fn type_name(&self) -> &'static str;
    fn allocated_size(&self) -> usize;
}

pub type DinoValue<'s> = Result<AllocatedRef<'s>, AllocatedErrRef<'s>>;
pub type DinoResult<'s> = Result<DinoValue<'s>, RuntimeViolation>;

#[derive(Debug)]
pub enum StackItem<'s> {
    Value(DinoValue<'s>),
    Pending(Pending<'s>),
}

pub type DinoStack<'s> = Vec<StackItem<'s>>;

#[derive(Debug)]
pub struct Pending<'s> {
    pub functor: PendingFunctor<'s>,
    pub arguments: Vec<StackItem<'s>>,
}

#[derive(Debug)]
pub enum PendingFunctor<'s> {
    Function(AllocatedRef<'s>),
    VariantAccess(VariantTag),
    VariantAccessSafe(VariantTag),
    Attr(usize),
}

#[derive(Debug)]
pub struct AllocatedObject<'s> {
    value: DinObject<'s>,
    size: usize, // todo we shouldn't store this
    #[debug(skip)]
    runtime: Runtime<'s>,
}

unsafe impl Send for AllocatedObject<'_> {}
unsafe impl Sync for AllocatedObject<'_> {}

impl<'s> AllocatedObject<'s> {
    pub fn new(value: DinObject<'s>, runtime: Runtime<'s>) -> Self {
        let size = value.obj_size();
        Self { value, size, runtime }
    }
}

impl<'s> Drop for AllocatedObject<'s> {
    fn drop(&mut self) {
        if REPORT_MEMORY_USAGE {
            println!("Deallocating ({} bytes) {:?}", self.size, self.value);
        }
        self.runtime.deallocate(self.size);
    }
}

#[derive(Debug)]
pub struct AllocatedRef<'s>(pub Arc<AllocatedObject<'s>>);

impl<'s> AllocatedRef<'s> {
    pub const SIZE: usize = size_of::<AllocatedRef>();
    pub fn new(obj: Arc<AllocatedObject<'s>>) -> Self {
        Self(obj)
    }

    // this is purposefully not an implementation of Clone
    // so that we can control the allocation and deallocation
    pub(crate) unsafe fn clone(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn runtime(&self) -> &Runtime<'s> {
        &self.0.runtime
    }
}

impl<'s> Drop for AllocatedRef<'s> {
    fn drop(&mut self) {
        if REPORT_MEMORY_USAGE {
            println!(
                "Deallocating ref ({} bytes, {} refs remaining) {:?}",
                Self::SIZE,
                Arc::strong_count(&self.0) - 1,
                self.0.value
            );
        }
        self.0.runtime.deallocate(Self::SIZE);
    }
}

impl<'s> Deref for AllocatedRef<'s> {
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

pub trait Allocatable<'s> {
    type Output: Sized;
    fn allocated_size(&self) -> usize {
        size_of::<Self::Output>() + self.obj_size()
    }

    /// The size of the object in bytes, excluding the size of the allocated object
    fn obj_size(&self) -> usize;
    fn allocate(self, runtime: Runtime<'s>) -> Self::Output;
}

impl<'s> Allocatable<'s> for DinObject<'s> {
    type Output = AllocatedRef<'s>;
    fn obj_size(&self) -> usize {
        size_of::<AllocatedObject<'s>>() + self.extra_allocated_size()
    }

    fn allocate(self, runtime: Runtime<'s>) -> Self::Output {
        AllocatedRef::new(Arc::new(AllocatedObject::new(self, runtime)))
    }
}
