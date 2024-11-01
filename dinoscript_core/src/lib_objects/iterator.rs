use crate::{as_ext, catch, dinobj::{AllocatedRef, DinoResult, ExtendedObject}, errors::RuntimeViolation, lib_objects::mapping::Mapping, runtime::SystemRuntimeFrame};

use super::sequence::Sequence;

#[derive(Debug)]
pub struct Iterable<'s>(IterableInner<'s>);

impl<'s> Iterable<'s> {
    pub const EXPECTED_TYPE_NAME: &'static str = "Iterable";

    pub fn new_mapping(mapping: AllocatedRef<'s>) -> Self {
        Self(IterableInner::Mapping(mapping))
    }

    pub fn new_sequence(sequence: AllocatedRef<'s>) -> Self {
        Self(IterableInner::Sequence(sequence))
    }

    pub fn iter<'f>(&self, frame: &'f SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, Box<dyn Iterator<Item=DinoResult<'s>> + 'f>> {
        self.0.iter(frame)
    }

    pub fn length_hint(&self) -> DinoResult<'s, LengthHint> {
        self.0.length_hint()
    }
}

#[derive(Debug)]
enum IterableInner<'s> {
    Mapping(
        // should be a mapping   
        AllocatedRef<'s>
    ),
    Sequence(
        // should be a sequence
        AllocatedRef<'s>
    ),
}

impl<'s> IterableInner<'s> {
    fn iter<'f>(&self, frame: &'f SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, Box<dyn Iterator<Item=DinoResult<'s>> + 'f>> {
        match self {
            Self::Mapping(mapping) => {
                let Some(mapping)  = as_ext!(mapping, Mapping) else {
                    return Err(RuntimeViolation::MalformedBytecode);
                };
                let iter = catch!(mapping.iter(frame)?);
                Ok(Ok(Box::new(iter)))
            },
            Self::Sequence(sequence) => {
                let Some(sequence)  = as_ext!(sequence, Sequence) else {
                    return Err(RuntimeViolation::MalformedBytecode);
                };
                let iter = sequence.iter(frame);
                Ok(Ok(iter))
            },
        }
    }

    fn length_hint(&self) -> DinoResult<'s, LengthHint> {
        match self {
            IterableInner::Mapping(mapping) => {
                let Some(mapping): Option<&Mapping<'s>>  = as_ext!(mapping, Mapping) else {
                    return Err(RuntimeViolation::MalformedBytecode);
                };
                Ok(Ok(LengthHint::UpTo(mapping.len())))
            },
            IterableInner::Sequence(sequence) => {
                let Some(sequence): Option<&Sequence<'s>>  = as_ext!(sequence, Sequence) else {
                    return Err(RuntimeViolation::MalformedBytecode);
                };
                Ok(Ok(LengthHint::UpTo(sequence.len())))
            },
        }
    }
}

#[derive(Debug)]
pub enum LengthHint {
    UpTo(usize),
    Unknown,
    Infinite,
}

impl<'s> ExtendedObject for Iterable<'s> {
    fn type_name(&self) -> &'static str {
        Self::EXPECTED_TYPE_NAME
    }
    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}