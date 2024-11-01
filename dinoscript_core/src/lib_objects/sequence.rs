use std::ops::Index;

use crate::catch;
use crate::dinobj::{AllocatedRef, DinoResult, ExtendedObject};
use crate::dinobj_utils::as_ext;
use crate::errors::RuntimeViolation;
use crate::runtime::{Runtime, SystemRuntimeFrame};
use itertools::Itertools;

use super::try_sort::try_sort;

#[derive(Debug)]
pub struct Sequence<'s>(SequenceInner<'s>);

impl<'s> Sequence<'s> {
    pub const EXPECTED_TYPE_NAME: &'static str = "Sequence";

    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Sequence(SequenceInner::new_array(array))
    }

    pub fn new_concat(runtime: &Runtime<'s>, seqs: Vec<AllocatedRef<'s>>) -> Result<Self, RuntimeViolation> {
        SequenceInner::new_concat(runtime, seqs).map(Self)
    }

    pub fn new_slice(
        runtime: &Runtime<'s>,
        source: AllocatedRef<'s>,
        start_idx: usize,
        end_idx: usize,
    ) -> Result<Self, RuntimeViolation> {
        SequenceInner::new_slice(runtime, source, start_idx, end_idx).map(Self)
    }

    pub fn new_map(inner: AllocatedRef<'s>, func: AllocatedRef<'s>) -> Self {
        Self(SequenceInner::new_map(inner, func))
    }
    
    pub fn get(&self, frame: &SystemRuntimeFrame<'_, 's, '_>, index: usize) -> DinoResult<'s> {
        self.0.get(frame, index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn idx_from_int(&self, idx: i64) -> NormalizedIdx {
        self.0.idx_from_int(idx)
    }

    pub fn iter<'a>(
        &'a self,
        frame: &'a SystemRuntimeFrame<'_, 's, '_>,
    ) -> Box<dyn Iterator<Item = DinoResult<'s>> + 'a> {
        self.0.iter(frame)
    }

    pub fn is_array(&self) -> bool {
        matches!(self.0, SequenceInner::Array(_))
    }
}

#[derive(Debug)]
enum SequenceInner<'s> {
    Array(
        // might be empty
        Vec<AllocatedRef<'s>>,
    ),
    Concat(SequenceConcat<'s>),
    Slice(SequenceSlice<'s>),
    Map(SequenceMap<'s>),
}

#[derive(Debug)]
struct SequenceConcat<'s>(
    // is guaranteed to have at least two elements
    Vec<SequenceConcatPart<'s>>,
);

#[derive(Debug)]
struct RelevantPart<'a, 's> {
    part: &'a SequenceConcatPart<'s>,
    start_idx: usize,
    part_index: usize,
}

impl<'a, 's> RelevantPart<'a, 's> {
    fn new(part: &'a SequenceConcatPart<'s>, prev_part_end_idx: usize, part_index: usize) -> Self {
        Self {
            part,
            start_idx: prev_part_end_idx,
            part_index,
        }
    }
}

impl<'s> SequenceConcat<'s> {
    const MAX_CONCAT_LIN_SEARCH: usize = 5;

    fn new(parts: Vec<SequenceConcatPart<'s>>) -> Self {
        debug_assert!(
            parts.len() >= 2,
            "concat should have at least two parts, got {}",
            parts.len()
        );
        Self(parts)
    }

    fn iter(&self) -> impl Iterator<Item = &SequenceConcatPart<'s>> {
        self.0.iter()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn last(&self) -> &SequenceConcatPart<'s> {
        self.0.last().unwrap()
    }

    fn find_relevant_part<'a>(&'a self, index: usize) -> RelevantPart<'a, 's> {
        if self[0].end_idx > index {
            RelevantPart::new(&self[0], 0, 0)
        } else if self.len() <= Self::MAX_CONCAT_LIN_SEARCH {
            // linear search
            let (idx, (prev_end, part)) = self
                .iter()
                .tuple_windows()
                .map(|(prev_part, part)| (prev_part.end_idx, part))
                .enumerate()
                .find(|(_, (_, part))| part.end_idx > index)
                .unwrap();
            RelevantPart::new(part, prev_end, idx + 1)
        } else {
            // binary search
            // we treat this as a binary search over the windowed tuples
            let mut left = 0;
            let mut right = self.len() - 2;
            loop {
                let mid = (left + right) / 2;
                let mid_part_prev = &self[mid];
                if mid_part_prev.end_idx > index {
                    right = mid - 1;
                    continue;
                }
                let mid_part = &self[mid + 1];
                if mid_part.end_idx <= index {
                    left = mid + 1;
                } else {
                    break RelevantPart::new(mid_part, mid_part_prev.end_idx, mid + 1);
                }
            }
        }
    }
}

impl<'s> Index<usize> for SequenceConcat<'s> {
    type Output = SequenceConcatPart<'s>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug)]
struct SequenceMap<'s> {
    // guarantees:
    // * to be a valid sequence
    // * to not be empty
    inner: AllocatedRef<'s>,
    // should:
    // * be a valid function
    // note that since it's impossible to guarantee that the function accepts exactly one argument,
    //  and we can't catch cases where it would accept less, but it's okay since that can only come from TBC
    func: AllocatedRef<'s>,
}

#[derive(Debug)]
struct SequenceConcatPart<'s> {
    // guarantees:
    // * to be a valid sequence
    // * to not be empty
    // * to not be a concat
    seq: AllocatedRef<'s>,
    end_idx: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NormalizedIdx {
    Negative,
    Positive(usize),
    Overflow,
}

fn normalize_idx(idx: i64, len: usize) -> NormalizedIdx {
    if idx < 0 {
        let Ok(subtrahend) = usize::try_from(-idx) else {
            return NormalizedIdx::Negative;
        };
        match len.checked_sub(subtrahend) {
            Some(n) => NormalizedIdx::Positive(n),
            None => NormalizedIdx::Negative,
        }
    } else {
        match idx.try_into() {
            Ok(n) => NormalizedIdx::Positive(n),
            Err(_) => NormalizedIdx::Overflow,
        }
    }
}

#[derive(Debug)]
struct SequenceSlice<'s> {
    // guarantees:
    // * to be a valid array
    // * length higher than the start index and higher than or equal to the end index
    // * to not be a slice itself
    // * either the start index is higher than 0 or the end index is lower than the length
    source: AllocatedRef<'s>,
    start_idx: usize,
    // is guaranteed to be higher than the start index
    end_idx: usize,
}

impl<'s> SequenceInner<'s> {
    fn empty() -> Self {
        Self::Array(Vec::new())
    }

    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Self::Array(array)
    }

    pub fn new_concat(runtime: &Runtime<'s>, seqs: Vec<AllocatedRef<'s>>) -> Result<Self, RuntimeViolation> {
        fn get_parts_iter<'a, 's>(part: &'a AllocatedRef<'s>) -> Box<dyn Iterator<Item = &'a AllocatedRef<'s>> + 'a> {
            let part_seq: &'a Sequence<'s> = match as_ext!(part, Sequence) {
                Some(seq) => seq,
                None => return Box::new(std::iter::empty()),
            };
            let part_seq = &part_seq.0;
            match part_seq {
                SequenceInner::Concat(parts) => Box::new(parts.iter().map(|part| &part.seq)),
                SequenceInner::Array(arr) if arr.is_empty() => Box::new(std::iter::empty()),
                _ => Box::new(std::iter::once(part)),
            }
        }
        let mut parts = Vec::with_capacity(seqs.len());
        let mut end_idx = 0;
        for part in seqs.iter().flat_map(|part| get_parts_iter(part)) {
            let part_seq: &Sequence<'s> = as_ext!(part, Sequence).unwrap();
            let part_seq = &part_seq.0;
            debug_assert!(!matches!(part_seq, Self::Concat(_)), "concat should not be nested");
            debug_assert!(!part_seq.is_empty(), "concat should not have empty parts");
            end_idx += part_seq.len();
            let part = runtime.clone_ok_ref(part)?;
            parts.push(SequenceConcatPart { seq: part, end_idx });
        }
        debug_assert!(parts.len() != 1, "concat should have at least two parts, or be empty");
        if parts.is_empty() {
            Ok(Self::empty())
        } else {
            Ok(Self::Concat(SequenceConcat::new(parts)))
        }
    }

    pub fn new_slice(
        runtime: &Runtime<'s>,
        source: AllocatedRef<'s>,
        start_idx: usize,
        mut end_idx: usize,
    ) -> Result<Self, RuntimeViolation> {
        let source_seq: &Sequence<'s> = match as_ext!(source, Sequence) {
            Some(seq) => seq,
            None => return Ok(Self::empty()),
        };
        let source_seq = &source_seq.0;
        let source_len = source_seq.len();
        if start_idx >= source_len || end_idx <= start_idx {
            return Ok(Self::empty());
        }
        if end_idx > source_len {
            end_idx = source_len;
        }
        debug_assert!(
            end_idx != source_len || start_idx != 0,
            "slice should not be the whole array, enforced by the caller"
        );
        match source_seq {
            Self::Slice(slc) => {
                let source = runtime.clone_ok_ref(&slc.source)?;
                Ok(Self::Slice(SequenceSlice {
                    source,
                    start_idx: slc.start_idx + start_idx,
                    end_idx: slc.start_idx + end_idx,
                }))
            }
            _ => Ok(Self::Slice(SequenceSlice {
                source,
                start_idx,
                end_idx,
            })),
        }
    }

    pub fn new_map(inner: AllocatedRef<'s>, func: AllocatedRef<'s>) -> Self {
        if cfg!(debug_assertions) {
            let seq: &Sequence<'s> = match as_ext!(inner, Sequence) {
                Some(seq) => seq,
                None => return Self::empty(),
            };
            let seq = &seq.0;
            debug_assert!(!seq.is_empty(), "map should not have an empty function");
        }
        Self::Map(SequenceMap { inner, func })
    }

    pub fn idx_from_int(&self, idx: i64) -> NormalizedIdx {
        let seq_len = self.len();
        normalize_idx(idx, seq_len)
    }

    pub fn get(&self, frame: &SystemRuntimeFrame<'_, 's, '_>, index: usize) -> DinoResult<'s> {
        match self {
            Self::Array(array) => {
                let Some(orig) = array.get(index) else {
                    return frame.runtime().allocate(Err("Index out of bounds".into()));
                };
                Ok(Ok(frame.runtime().clone_ok_ref(orig)?))
            }
            Self::Concat(parts) => {
                if index >= parts.last().end_idx {
                    return frame.runtime().allocate(Err("Index out of bounds".into()));
                }
                let relevant_part = parts.find_relevant_part(index);
                let part = match as_ext!(relevant_part.part.seq, Sequence) {
                    Some(seq) => &seq.0,
                    None => unreachable!(),
                };
                part.get(frame, index - relevant_part.start_idx)
            }
            Self::Slice(slc) => {
                if index >= slc.end_idx - slc.start_idx {
                    return frame.runtime().allocate(Err("Index out of bounds".into()));
                }
                let part = match as_ext!(slc.source, Sequence) {
                    Some(seq) => &seq.0,
                    None => return frame.runtime().allocate(Err("Invalid source".into())),
                };
                part.get(frame, index + slc.start_idx)
            }
            Self::Map(map) => {
                let arg = match as_ext!(map.inner, Sequence) {
                    Some(seq) => &seq.0,
                    None => return frame.runtime().allocate(Err("Invalid source".into())),
                };
                let arg = arg.get(frame, index)?;
                frame.call(&map.func, &[arg])
            }
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Array(array) => array.len(),
            Self::Concat(array) => array.last().end_idx,
            Self::Slice(slc) => slc.end_idx - slc.start_idx,
            Self::Map(map) => {
                let arg = match as_ext!(map.inner, Sequence) {
                    Some(seq) => &seq.0,
                    None => unreachable!("Invalid source"),
                };
                arg.len()
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        if let Self::Array(array) = self {
            array.is_empty()
        } else {
            false
        }
    }

    pub fn iter<'a>(
        &'a self,
        frame: &'a SystemRuntimeFrame<'_, 's, '_>,
    ) -> Box<dyn Iterator<Item = DinoResult<'s>> + 'a> {
        match self {
            Self::Array(array) => Box::new(array.iter().map(|r| Ok(Ok(frame.runtime().clone_ok_ref(r)?)))),
            Self::Concat(array) => {
                let mut iter = Vec::with_capacity(array.len());
                for part in array.iter() {
                    let part = &as_ext!(part.seq, Sequence).unwrap().0;
                    iter.push(part.iter(frame));
                }
                Box::new(iter.into_iter().flatten())
            }
            Self::Slice(slc) => {
                let inner = &as_ext!(slc.source, Sequence).unwrap().0;
                match inner {
                    Self::Array(array) => Box::new(
                        array
                            .iter()
                            .map(|r| Ok(Ok(frame.runtime().clone_ok_ref(r)?)))
                            .skip(slc.start_idx)
                            .take(slc.end_idx - slc.start_idx),
                    ),
                    Self::Concat(concat) => {
                        let rel_start_part = concat.find_relevant_part(slc.start_idx);
                        let rel_end_part = concat.find_relevant_part(slc.end_idx - 1);
                        if rel_start_part.part_index == rel_end_part.part_index {
                            // the slice is within the same part, we can just return a slice over that
                            let part_seq = as_ext!(rel_start_part.part.seq, Sequence).unwrap();
                            let part_seq = &part_seq.0;
                            Box::new(
                                part_seq
                                    .iter(frame)
                                    .take(slc.end_idx - rel_start_part.start_idx)
                                    .skip(slc.start_idx - rel_start_part.start_idx),
                            )
                        } else {
                            let mut iter = Vec::with_capacity(rel_end_part.part_index - rel_start_part.part_index + 1);
                            {
                                let first_part_seq = &as_ext!(rel_start_part.part.seq, Sequence).unwrap().0;
                                let first_iter: Box<dyn Iterator<Item = _>> = Box::new(
                                    first_part_seq
                                        .iter(frame)
                                        .skip(slc.start_idx - rel_start_part.start_idx),
                                );
                                iter.push(first_iter);
                            }
                            for part in concat
                                .0
                                .iter()
                                .take(rel_end_part.part_index)
                                .skip(rel_start_part.part_index + 1)
                            {
                                let part_seq = &as_ext!(part.seq, Sequence).unwrap().0;
                                iter.push(part_seq.iter(frame));
                            }
                            {
                                let last_part_seq = &as_ext!(rel_end_part.part.seq, Sequence).unwrap().0;
                                let last_iter: Box<dyn Iterator<Item = _>> =
                                    Box::new(last_part_seq.iter(frame).take(slc.end_idx - rel_end_part.start_idx));
                                iter.push(last_iter);
                            }
                            Box::new(iter.into_iter().flatten())
                        }
                    }
                    _ => Box::new(inner.iter(frame).take(slc.end_idx).skip(slc.start_idx)),
                }
            }
            Self::Map(map) => {
                let arg = match as_ext!(map.inner, Sequence) {
                    Some(seq) => &seq.0,
                    None => unreachable!("Invalid source"),
                };
                let arg = arg.iter(frame);
                Box::new(arg.map(|r| frame.call(&map.func, &[r?])))
            }
        }
    }
}

impl<'s> ExtendedObject for Sequence<'s> {
    fn type_name(&self) -> &'static str {
        Self::EXPECTED_TYPE_NAME
    }
    fn allocated_size(&self) -> usize {
        // todo is this correct?
        size_of::<Self>()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        as_prim,
        dinobj::{DinObject, TailCallAvailability},
        runtime::{Runtime, RuntimeFrame},
    };

    use super::*;

    fn array_from_vec(runtime: &Runtime<'static>, vec: Vec<i64>) -> AllocatedRef<'static> {
        let seq = Sequence::new_array(
            vec.into_iter()
                .map(|i| runtime.allocate(Ok(DinObject::Int(i))).unwrap().unwrap())
                .collect(),
        );
        runtime.allocate_ext(seq).unwrap().unwrap()
    }

    #[test]
    fn test_concat_get_linear() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);
        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let part3 = array_from_vec(&runtime, vec![4, 39, 24]);

        let seq = SequenceInner::Concat(SequenceConcat::new(vec![
            SequenceConcatPart { seq: part1, end_idx: 4 },
            SequenceConcatPart { seq: part2, end_idx: 6 },
            SequenceConcatPart { seq: part3, end_idx: 9 },
        ]));

        let get = |idx| {
            seq.get(&frame, idx)
                .unwrap()
                .map(|r| as_prim!(r, Int).cloned().unwrap())
        };

        assert_eq!(get(0).unwrap(), 3);
        assert_eq!(get(1).unwrap(), 1);
        assert_eq!(get(2).unwrap(), 0);
        assert_eq!(get(3).unwrap(), 9);
        assert_eq!(get(4).unwrap(), 5);
        assert_eq!(get(5).unwrap(), 92);
        assert_eq!(get(6).unwrap(), 4);
        assert_eq!(get(7).unwrap(), 39);
        assert_eq!(get(8).unwrap(), 24);
        assert!(get(9).is_err());
        assert!(get(100).is_err());
    }

    #[test]
    fn test_concat_get_binsearch() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);
        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let part3 = array_from_vec(&runtime, vec![4, 39, 24]);
        let part4 = array_from_vec(&runtime, vec![41, 2, 13]);
        let part5 = array_from_vec(&runtime, vec![7, 77, 51, 8]);
        let part6 = array_from_vec(&runtime, vec![6, 2, 99]);

        let seq = SequenceInner::Concat(SequenceConcat::new(vec![
            SequenceConcatPart { seq: part1, end_idx: 4 },
            SequenceConcatPart { seq: part2, end_idx: 6 },
            SequenceConcatPart { seq: part3, end_idx: 9 },
            SequenceConcatPart {
                seq: part4,
                end_idx: 12,
            },
            SequenceConcatPart {
                seq: part5,
                end_idx: 16,
            },
            SequenceConcatPart {
                seq: part6,
                end_idx: 19,
            },
        ]));

        let get = |idx| {
            seq.get(&frame, idx)
                .unwrap()
                .map(|r| as_prim!(r, Int).cloned().unwrap())
        };

        assert_eq!(get(0).unwrap(), 3);
        assert_eq!(get(1).unwrap(), 1);
        assert_eq!(get(2).unwrap(), 0);
        assert_eq!(get(3).unwrap(), 9);
        assert_eq!(get(4).unwrap(), 5);
        assert_eq!(get(5).unwrap(), 92);
        assert_eq!(get(6).unwrap(), 4);
        assert_eq!(get(7).unwrap(), 39);
        assert_eq!(get(8).unwrap(), 24);
        assert_eq!(get(9).unwrap(), 41);
        assert_eq!(get(10).unwrap(), 2);
        assert_eq!(get(11).unwrap(), 13);
        assert_eq!(get(12).unwrap(), 7);
        assert_eq!(get(13).unwrap(), 77);
        assert_eq!(get(14).unwrap(), 51);
        assert_eq!(get(15).unwrap(), 8);
        assert_eq!(get(16).unwrap(), 6);
        assert_eq!(get(17).unwrap(), 2);
        assert_eq!(get(18).unwrap(), 99);
        assert!(get(19).is_err());
    }

    #[test]
    fn test_make_concat() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);

        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let con1 = runtime
            .allocate_ext(Sequence::new_concat(&runtime, vec![part1, part2]).unwrap())
            .unwrap()
            .unwrap();
        let part3 = array_from_vec(&runtime, vec![3, 45, 16]);
        let part4 = array_from_vec(&runtime, vec![]);
        let part5 = array_from_vec(&runtime, vec![4, 39, 24]);

        let seq = Sequence::new_concat(&runtime, vec![con1, part3, part4, part5]).unwrap();
        assert_eq!(seq.len(), 12);
        let get = |idx| seq.get(&frame, idx).map(|r| *as_prim!(r.unwrap(), Int).unwrap());

        assert_eq!(get(0).unwrap(), 3);
        assert_eq!(get(5).unwrap(), 92);
        assert_eq!(get(6).unwrap(), 3);
        assert_eq!(get(8).unwrap(), 16);
        assert_eq!(get(10).unwrap(), 39);
        let SequenceInner::Concat(parts) = seq.0 else { panic!() };
        assert_eq!(parts.len(), 4);
    }
    #[test]
    fn test_make_concat_empty() {
        let runtime = Runtime::new();
        let part1 = array_from_vec(&runtime, vec![]);
        let part2 = array_from_vec(&runtime, vec![]);
        let part3 = array_from_vec(&runtime, vec![]);

        let seq = Sequence::new_concat(&runtime, vec![part1, part2, part3]).unwrap();
        assert_eq!(seq.len(), 0);

        let SequenceInner::Array(v) = seq.0 else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);

        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 2, 7).unwrap();
        assert_eq!(seq.len(), 5);
        let get = |idx| seq.get(&frame, idx).map(|r| *as_prim!(r.unwrap(), Int).unwrap());

        assert_eq!(get(0).unwrap(), 0);
        assert_eq!(get(1).unwrap(), 9);
        assert_eq!(get(2).unwrap(), 5);
        assert_eq!(get(3).unwrap(), 92);
        assert_eq!(get(4).unwrap(), 4);
    }

    #[test]
    fn test_make_slice_empty_eq_bounds() {
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 2, 2).unwrap();
        assert_eq!(seq.len(), 0);
        let SequenceInner::Array(v) = seq.0 else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice_empty_gt_bounds() {
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 7, 2).unwrap();
        assert_eq!(seq.len(), 0);
        let SequenceInner::Array(v) = seq.0 else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice_nested() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);

        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq0 = Sequence::new_slice(&runtime, source, 2, 7).unwrap();
        assert_eq!(seq0.len(), 5);
        let seq0 = runtime.allocate_ext(seq0).unwrap().unwrap();
        let seq1 = Sequence::new_slice(&runtime, seq0, 1, 4).unwrap();
        assert_eq!(seq1.len(), 3);
        let get = |idx| seq1.get(&frame, idx).map(|r| *as_prim!(r.unwrap(), Int).unwrap());

        assert_eq!(get(0).unwrap(), 9);
        assert_eq!(get(1).unwrap(), 5);
        assert_eq!(get(2).unwrap(), 92);

        let SequenceInner::Slice(slc) = seq1.0 else { panic!() };
        assert_eq!(slc.start_idx, 3);
        assert_eq!(slc.end_idx, 6);
        let inner_slc = as_ext!(slc.source, Sequence).unwrap();
        assert!(matches!(inner_slc.0, SequenceInner::Array(_)));
    }

    #[test]
    fn test_normalize_idx() {
        assert_eq!(normalize_idx(0, 5), NormalizedIdx::Positive(0));
        assert_eq!(normalize_idx(1, 5), NormalizedIdx::Positive(1));
        assert_eq!(normalize_idx(2, 5), NormalizedIdx::Positive(2));
        assert_eq!(normalize_idx(3, 5), NormalizedIdx::Positive(3));
        assert_eq!(normalize_idx(4, 5), NormalizedIdx::Positive(4));
        assert_eq!(normalize_idx(5, 5), NormalizedIdx::Positive(5));
        assert_eq!(normalize_idx(6, 5), NormalizedIdx::Positive(6));
        assert_eq!(normalize_idx(-1, 5), NormalizedIdx::Positive(4));
        assert_eq!(normalize_idx(-2, 5), NormalizedIdx::Positive(3));
        assert_eq!(normalize_idx(-3, 5), NormalizedIdx::Positive(2));
        assert_eq!(normalize_idx(-4, 5), NormalizedIdx::Positive(1));
        assert_eq!(normalize_idx(-5, 5), NormalizedIdx::Positive(0));
        assert_eq!(normalize_idx(-6, 5), NormalizedIdx::Negative);
    }

    #[test]
    fn test_iter_sliced_concat() {
        let runtime = Runtime::new();
        let root_frame = RuntimeFrame::root(0, &runtime);
        let frame = SystemRuntimeFrame::from_parent(&root_frame, Vec::new(), TailCallAvailability::Disallowed);

        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let part3 = array_from_vec(&runtime, vec![3, 45, 16]);
        let part4 = array_from_vec(&runtime, vec![4, 39, 24]);

        let inner = runtime
            .allocate_ext(Sequence::new_concat(&runtime, vec![part1, part2, part3, part4]).unwrap())
            .unwrap()
            .unwrap();

        let slc = |start, end| {
            SequenceInner::new_slice(&runtime, runtime.clone_ok_ref(&inner).unwrap(), start, end)
                .unwrap()
                .iter(&frame)
                .map(|r| *as_prim!(r.unwrap().unwrap(), Int).unwrap())
                .collect::<Vec<_>>()
        };

        let full = [3, 1, 0, 9, 5, 92, 3, 45, 16, 4, 39, 24];
        for start_ind in 0..full.len() {
            for end_ind in start_ind..=full.len() {
                if start_ind == 0 && end_ind == full.len() {
                    // not handled by slicing
                    continue;
                }
                assert_eq!(
                    slc(start_ind, end_ind),
                    full[start_ind..end_ind].to_vec(),
                    "start: {}, end: {}",
                    start_ind,
                    end_ind
                );
            }
        }
    }
}
