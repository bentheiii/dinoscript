use crate::dinobj::{AllocatedRef, ExtendedObject};

#[derive(Debug)]
pub struct Array<'s>(pub Vec<AllocatedRef<'s>>);

impl<'s> Array<'s> {
    pub fn new(array: Vec<AllocatedRef<'s>>) -> Self {
        Self(array)
    }
}

impl<'s> ExtendedObject for Array<'s> {
    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}