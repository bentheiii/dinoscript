use std::sync::Arc;
use derive_more::Debug;

use crate::{dinobj::Allocatable, runtime::Runtime};

pub type RuntimeViolation = ();
#[derive(Debug)]
pub enum RuntimeError{
    Borrowed(&'static str),
    Owned{
        msg: Arc<str>,
        is_first_owner: bool,
    },
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

impl RuntimeError{
    unsafe fn clone(&self) -> Self {
        match self {
            Self::Borrowed(s) => Self::Borrowed(s),
            Self::Owned{msg: ptr, ..} => Self::Owned{msg: ptr.clone(), is_first_owner: false},
        }
    }
}

#[derive(Debug)]
pub struct AllocatedRuntimeError<'s>{
    err: RuntimeError,
    #[debug(skip)]
    runtime: Runtime<'s>,
}

impl<'s> Allocatable<'s> for RuntimeError{
    type Output = AllocatedRuntimeError<'s>;

    fn obj_size(&self) -> usize {
        std::mem::size_of::<Self>() + match self {
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