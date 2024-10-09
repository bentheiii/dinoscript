use crate::dinobj::{AllocatedRef, ExtendedObject};

#[derive(Debug)]
pub enum Sequence<'s>{
    Array(Vec<AllocatedRef<'s>>),
    Concat(Vec<AllocatedRef<'s>>),
}

impl<'s> Sequence<'s> {
    pub const EXPECTED_TYPE_NAME: &'static str = "Sequence";

    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Self::Array(array)
    }

    pub fn get(&self, index: usize) -> Option<&AllocatedRef<'s>> {
        match self {
            Self::Array(array) => array.get(index),
            Self::Concat(array) => todo!(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Array(array) => array.len(),
            Self::Concat(array) => todo!(),
        }
    }

    pub fn iter<'a>(&'a self)->Box<dyn Iterator<Item=&'a AllocatedRef<'s>> + 'a>{
        match self {
            Self::Array(array) => Box::new(array.iter()),
            Self::Concat(array) => todo!(),
        }
    }
}

impl<'s> ExtendedObject for Sequence<'s> {
    fn type_name(&self) -> &'static str {
        Self::EXPECTED_TYPE_NAME
    }
    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}