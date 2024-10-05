use crate::dinobj::{AllocatedRef, ExtendedObject};

#[derive(Debug)]
pub enum Sequence<'s>{
    Array(Vec<AllocatedRef<'s>>),
}

impl<'s> Sequence<'s> {
    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Self::Array(array)
    }

    pub fn get(&self, index: usize) -> Option<&AllocatedRef<'s>> {
        match self {
            Self::Array(array) => array.get(index),
        }
    }
}

impl<'s> ExtendedObject for Sequence<'s> {
    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}