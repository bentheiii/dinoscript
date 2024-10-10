use std::{error::Error, fmt::{Display}, sync::Arc};
use derive_more::Debug;

use crate::{dinobj::Allocatable, runtime::Runtime};

#[derive(Debug)]
pub struct RuntimeViolation(());

#[derive(Debug)]
pub enum RuntimeError{
    Borrowed(&'static str),
    Owned{
        msg: Arc<str>,
        is_first_owner: bool,
    },
}

impl Display for RuntimeError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Borrowed(s) => write!(f, "{}", s),
            Self::Owned{msg, ..} => write!(f, "{}", msg),
        }
    }
}

impl From<&'static str> for RuntimeError{
    fn from(s: &'static str) -> Self {
        Self::Borrowed(s)
    }
}

impl From<String> for RuntimeError{
    fn from(s: String) -> Self {
        Self::Owned { msg: Arc::from(s), is_first_owner: true }
    }
}

#[derive(Debug)]
pub struct AllocatedRuntimeError<'s>{
    err: RuntimeError,
    #[debug(skip)]
    runtime: Runtime<'s>,
}

impl<'s> Error for AllocatedRuntimeError<'s>{}

impl<'s> Display for AllocatedRuntimeError<'s>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

impl<'s> Allocatable<'s> for RuntimeError{
    type Output = AllocatedRuntimeError<'s>;

    fn obj_size(&self) -> usize {
        std::mem::size_of::<Self::Output>() + match self {
            Self::Owned{ msg, is_first_owner:true} => msg.len(),
            _ => 0,
        }
    }
    
    fn allocate(self, runtime: Runtime<'s>) -> Self::Output {
        AllocatedRuntimeError{
            err: self,
            runtime,
        }
    }
}

impl<'s> Drop for AllocatedRuntimeError<'s>{
    fn drop(&mut self) {
        self.runtime.deallocate(self.err.obj_size());
    }
}