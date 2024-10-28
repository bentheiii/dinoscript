use derive_more::Debug;
use std::{error::Error, fmt::Display, sync::Arc};

use crate::{dinobj::Allocatable, runtime::Runtime};

#[derive(Debug)]
pub enum RuntimeViolation{
    MalformedBytecode,
}

#[derive(Debug)]
pub enum RuntimeError {
    Borrowed(&'static str),
    Owned(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Borrowed(s) => write!(f, "{}", s),
            Self::Owned(s) => write!(f, "{}", s),
        }
    }
}

impl From<&'static str> for RuntimeError {
    fn from(s: &'static str) -> Self {
        Self::Borrowed(s)
    }
}

impl From<String> for RuntimeError {
    fn from(s: String) -> Self {
        Self::Owned(s)
    }
}

#[derive(Debug)]
pub struct AllocatedRuntimeError<'s> {
    err: RuntimeError,
    #[debug(skip)]
    runtime: Runtime<'s>,
}

impl<'s> Error for AllocatedRuntimeError<'s> {}

impl<'s> Display for AllocatedRuntimeError<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}
impl<'s> Drop for AllocatedRuntimeError<'s> {
    fn drop(&mut self) {
        self.runtime.deallocate(self.err.obj_size());
    }
}

#[derive(Debug)]
pub struct AllocatedErrRef<'s>(Arc<AllocatedRuntimeError<'s>>);

impl<'s> AllocatedErrRef<'s> {
    pub fn new(ptr: Arc<AllocatedRuntimeError<'s>>) -> Self {
        Self(ptr)
    }

    pub(crate) unsafe fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl Display for AllocatedErrRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.err)
    }
}

impl<'s> Drop for AllocatedErrRef<'s> {
    fn drop(&mut self) {
        self.0.runtime.deallocate(self.0.err.obj_size());
    }
}

impl<'s> Allocatable<'s> for RuntimeError {
    type Output = AllocatedErrRef<'s>;

    fn obj_size(&self) -> usize {
        std::mem::size_of::<AllocatedRuntimeError>()
            + match self {
                Self::Owned(s) => s.len(),
                _ => 0,
            }
    }

    fn allocate(self, runtime: Runtime<'s>) -> Self::Output {
        Self::Output::new(Arc::new(AllocatedRuntimeError { err: self, runtime }))
    }
}
