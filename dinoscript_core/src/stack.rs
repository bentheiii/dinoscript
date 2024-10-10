use crate::{as_ext, dinobj::{AllocatedRef, ExtendedObject}};

#[derive(Debug)]
pub enum Stack<'s>{
    Empty,
    Populated(PopulatedStackNode<'s>),
}

#[derive(Debug)]
struct PopulatedStackNode<'s>{
    top: AllocatedRef<'s>,
    // is guaranteed to be a valid stack object
    prev: AllocatedRef<'s>,
    len: usize,
}

impl<'s> Stack<'s>{
    pub const EXPECTED_TYPE_NAME: &'static str = "Stack";

    pub fn populated(top: AllocatedRef<'s>, prev: AllocatedRef<'s>) -> Self{
        let Some(prev_s) = as_ext!(prev, Stack) else {
            return Self::Empty;
        };
        let len = prev_s.len() + 1;
        Self::Populated(PopulatedStackNode{top, prev, len})
    }

    pub fn empty() -> Self{
        Self::Empty
    }

    pub fn len(&self) -> usize{
        match self {
            Self::Empty => 0,
            Self::Populated(node) => node.len,
        }
    }

    pub fn top(&self) -> Option<&AllocatedRef<'s>>{
        match self {
            Self::Empty => None,
            Self::Populated(node) => Some(&node.top),
        }
    }

    pub fn prev(&self) -> Option<&AllocatedRef<'s>>{
        match self {
            Self::Empty => None,
            Self::Populated(node) => Some(&node.prev),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &AllocatedRef<'s>>{
        struct StackIter<'s, 'a>{
            node: &'a Stack<'s>,
        }

        impl<'s, 'a> Iterator for StackIter<'s, 'a>{
            type Item = &'a AllocatedRef<'s>;

            fn next(&mut self) -> Option<Self::Item>{
                match self.node{
                    Stack::Empty => None,
                    Stack::Populated(node) => {
                        let prev = as_ext!(node.prev, Stack)?;
                        self.node = prev;
                        Some(&node.top)
                    }
                }
            }
        }

        impl<'s, 'a> ExactSizeIterator for StackIter<'s, 'a>{
            fn len(&self) -> usize{
                match self.node{
                    Stack::Empty => 0,
                    Stack::Populated(node) => node.len,
                }
            }
        }
        
        StackIter{node: self}
    }
}

impl<'s> ExtendedObject for Stack<'s>{
    fn type_name(&self) -> &'static str{
        Self::EXPECTED_TYPE_NAME
    }

    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}