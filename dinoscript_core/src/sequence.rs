use std::ops::Index;

use crate::dinobj::{AllocatedRef, ExtendedObject};
use crate::dinobj_utils::as_ext;
use crate::errors::RuntimeViolation;
use crate::runtime::Runtime;
use itertools::Itertools;

#[derive(Debug)]
pub enum Sequence<'s>{
    Array(
        // might be empty
        Vec<AllocatedRef<'s>>
    ),
    Concat(
        // is guaranteed to have at least two elements
        SequenceConcat<'s>
    ),
    Slice(SequenceSlice<'s>),
}

#[derive(Debug)]
struct SequenceConcat<'s>(Vec<SequenceConcatPart<'s>>);

struct RelevantPart<'a, 's>{
    part: &'a SequenceConcatPart<'s>,
    prev_part_end_idx: usize,
    part_index: usize,
}

impl<'a, 's> RelevantPart<'a, 's>{
    fn new(part: &'a SequenceConcatPart<'s>, prev_part_end_idx: usize, part_index: usize)->Self{
        Self{part, prev_part_end_idx, part_index}
    }
}

impl<'s> SequenceConcat<'s>{
    const MAX_CONCAT_LIN_SEARCH: usize = 5;

    fn new(parts: Vec<SequenceConcatPart<'s>>)->Self{
        debug_assert!(parts.len() >= 2, "concat should have at least two parts");
        Self(parts)
    }

    fn iter(&self)->impl Iterator<Item=&SequenceConcatPart<'s>>{
        self.0.iter()
    }

    fn len(&self)->usize{
        self.0.len()
    }

    fn last(&self)->&SequenceConcatPart<'s>{
        self.0.last().unwrap()
    }

    fn find_relevant_part<'a>(&'a self, index: usize)->RelevantPart<'a, 's>{
        if self[0].end_idx > index{
            RelevantPart::new(&self[0], 0, 0)
        }
        else if self.len() <= Self::MAX_CONCAT_LIN_SEARCH{
            // linear search
            let (idx, (prev_end, part)) = self.iter().tuple_windows().map(|(prev_part, part)| (prev_part.end_idx, part)).enumerate().find(|(_,(_, part))| part.end_idx > index).unwrap();
            RelevantPart::new(part, prev_end, idx)
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
                    break RelevantPart::new(mid_part, mid_part_prev.end_idx, mid+1);
                }
            }
        }
    }
}

impl<'s> Index<usize> for SequenceConcat<'s>{
    type Output = SequenceConcatPart<'s>;

    fn index(&self, index: usize)->&Self::Output{
        &self.0[index]
    }
}

#[derive(Debug)]
struct SequenceConcatPart<'s>{
    // guarantees:
    // * to be a valid sequence
    // * to not be empty
    // * to not be a concat
    seq: AllocatedRef<'s>,
    end_idx: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NormalizedIdx{
    Negative,
    Positive(usize),
    Overflow
}

fn normalize_idx(idx: i64, len: usize) -> NormalizedIdx {
    if idx < 0 {
        let Ok(subtrahend) = usize::try_from(-idx) else { return NormalizedIdx::Negative };
        match len.checked_sub(subtrahend){
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
struct SequenceSlice<'s>{
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

impl<'s> Sequence<'s> {
    const MAX_CONCAT_LIN_SEARCH: usize = 5;
    pub const EXPECTED_TYPE_NAME: &'static str = "Sequence";

    fn empty() -> Self {
        Self::Array(Vec::new())
    }

    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Self::Array(array)
    }

    pub fn new_concat(runtime: &Runtime<'s>, seqs: Vec<AllocatedRef<'s>>) -> Result<Self, RuntimeViolation> {
        fn get_parts_iter<'a, 's>(part: &'a AllocatedRef<'s>)->Box<dyn Iterator<Item=&'a AllocatedRef<'s>> + 'a>{
            let part_seq: &'a Sequence<'s> = match as_ext!(part, Sequence) {
                Some(seq) => seq,
                None => return Box::new(std::iter::empty()),
            };
            match part_seq {
                Sequence::Concat(parts) => Box::new(parts.iter().map(|part| &part.seq)),
                Sequence::Array(arr) if arr.is_empty() => Box::new(std::iter::empty()),
                _ => Box::new(std::iter::once(part)),
            }
        }
        let mut parts = Vec::with_capacity(seqs.len());
        let mut end_idx = 0;
        for part in seqs.iter().flat_map(|part| get_parts_iter(part)) {
            let part_seq: &Sequence<'s> = as_ext!(part, Sequence).unwrap();
            debug_assert!(!matches!(part_seq, Self::Concat(_)), "concat should not be nested");
            debug_assert!(!part_seq.is_empty(), "concat should not have empty parts");
            end_idx += part_seq.len();
            let part = runtime.clone_ref(part)?;
            parts.push(SequenceConcatPart{seq: part, end_idx});
        }
        debug_assert!(parts.len() != 1, "concat should have at least two parts, or be empty");
        if parts.is_empty() {
            Ok(Self::empty())
        } else {
            Ok(Self::Concat(SequenceConcat::new(parts)))
        }
    }

    pub fn new_slice(runtime: &Runtime<'s>, source: AllocatedRef<'s>, start_idx: usize, mut end_idx: usize) -> Result<Self, RuntimeViolation> {
        let source_seq: &Sequence<'s> = match as_ext!(source, Sequence){
            Some(seq) => seq,
            None => return Ok(Self::empty()),
        };
        let source_len = source_seq.len();
        if start_idx >= source_len || end_idx <= start_idx {
            return Ok(Self::empty());
        }
        if end_idx > source_len {
            end_idx = source_len;
        }
        debug_assert!(end_idx != source_len || start_idx != 0, "slice should not be the whole array, enforced by the caller");
        match source_seq {
            Self::Slice(slc) => {
                let source = runtime.clone_ref(&slc.source)?;
                Ok(Self::Slice(SequenceSlice{source, start_idx: slc.start_idx + start_idx, end_idx: slc.start_idx + end_idx}))
            },
            _ =>Ok(Self::Slice(SequenceSlice{source, start_idx, end_idx}))
        }
    }

    pub fn idx_from_int(&self, idx: i64) -> NormalizedIdx{
        let seq_len = self.len();
        normalize_idx(idx, seq_len)
    }

    pub fn get(&self, index: usize) -> Option<&AllocatedRef<'s>> {
        match self {
            Self::Array(array) => array.get(index),
            Self::Concat(parts) => {
                if index >= parts.last().end_idx {
                    return None;
                }
                let relevant_part = parts.find_relevant_part(index);
                let part: &Sequence<'_> = as_ext!(relevant_part.part.seq, Sequence)?;
                part.get(index - relevant_part.prev_part_end_idx)
            },
            Self::Slice(slc) => {
                if index >= slc.end_idx-slc.start_idx {
                    return None;
                }
                let part: &Sequence<'_> = as_ext!(slc.source, Sequence)?;
                part.get(index + slc.start_idx)
            },
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Array(array) => array.len(),
            Self::Concat(array) => array.last().end_idx,
            Self::Slice(slc) => slc.end_idx - slc.start_idx,
        }
    }

    pub fn is_empty(&self) -> bool {
        if let Self::Array(array) = self {
            return array.is_empty();
        }
        return false;
    }

    pub fn iter<'a>(&'a self)->Box<dyn Iterator<Item=&'a AllocatedRef<'s>> + 'a>{
        match self {
            Self::Array(array) => Box::new(array.iter()),
            Self::Concat(array) => {
                let mut iter = Vec::new();
                for part in array.iter() {
                    let part: &Sequence<'_> = as_ext!(part.seq, Sequence).unwrap();
                    iter.push(part.iter());
                }
                Box::new(iter.into_iter().flatten())
            },
            Self::Slice(slc) => {
                let inner = as_ext!(slc.source, Sequence).unwrap();
                match inner{
                    Self::Array(array) => Box::new(array.iter().skip(slc.start_idx).take(slc.end_idx - slc.start_idx)),

                    _ => Box::new(
                        inner.iter().skip(slc.start_idx).take(slc.end_idx - slc.start_idx)
                    )
                }
            },
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

#[cfg(test)]
mod tests {
    use crate::{as_prim, dinobj::DinObject, runtime::Runtime};

    use super::*;

    fn array_from_vec(runtime: &Runtime<'static>, vec: Vec<i64>) -> AllocatedRef<'static> {
        let seq = Sequence::new_array(vec.into_iter().map(|i| runtime.allocate(Ok(DinObject::Int(i))).unwrap().unwrap()).collect());
        runtime.allocate_ext(seq).unwrap().unwrap()
    }
    
    #[test]
    fn test_concat_get_linear() {
        let runtime = Runtime::new();
        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let part3 = array_from_vec(&runtime, vec![4, 39, 24]);

        let seq = Sequence::Concat(SequenceConcat::new(vec![
            SequenceConcatPart{seq: part1, end_idx: 4},
            SequenceConcatPart{seq: part2, end_idx: 6},
            SequenceConcatPart{seq: part3, end_idx: 9},
        ]));

        let get = |idx| seq.get(idx).and_then(|r| as_prim!(r, Int).cloned());

        assert_eq!(get(0), Some(3));
        assert_eq!(get(1), Some(1));
        assert_eq!(get(2), Some(0));
        assert_eq!(get(3), Some(9));
        assert_eq!(get(4), Some(5));
        assert_eq!(get(5), Some(92));
        assert_eq!(get(6), Some(4));
        assert_eq!(get(7), Some(39));
        assert_eq!(get(8), Some(24));
        assert_eq!(get(9), None);
        assert_eq!(get(100), None);
    }

    #[test]
    fn test_concat_get_binsearch() {
        let runtime = Runtime::new();
        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let part3 = array_from_vec(&runtime, vec![4, 39, 24]);
        let part4 = array_from_vec(&runtime, vec![41, 2, 13]);
        let part5 = array_from_vec(&runtime, vec![7, 77, 51, 8]);
        let part6 = array_from_vec(&runtime, vec![6, 2, 99]);

        let seq = Sequence::Concat(SequenceConcat::new(vec![
            SequenceConcatPart{seq: part1, end_idx: 4},
            SequenceConcatPart{seq: part2, end_idx: 6},
            SequenceConcatPart{seq: part3, end_idx: 9},
            SequenceConcatPart{seq: part4, end_idx: 12},
            SequenceConcatPart{seq: part5, end_idx: 16},
            SequenceConcatPart{seq: part6, end_idx: 19},
        ]));

        let get = |idx| seq.get(idx).and_then(|r| as_prim!(r, Int).cloned());

        assert_eq!(get(0), Some(3));
        assert_eq!(get(1), Some(1));
        assert_eq!(get(2), Some(0));
        assert_eq!(get(3), Some(9));
        assert_eq!(get(4), Some(5));
        assert_eq!(get(5), Some(92));
        assert_eq!(get(6), Some(4));
        assert_eq!(get(7), Some(39));
        assert_eq!(get(8), Some(24));
        assert_eq!(get(9), Some(41));
        assert_eq!(get(10), Some(2));
        assert_eq!(get(11), Some(13));
        assert_eq!(get(12), Some(7));
        assert_eq!(get(13), Some(77));
        assert_eq!(get(14), Some(51));
        assert_eq!(get(15), Some(8));
        assert_eq!(get(16), Some(6));
        assert_eq!(get(17), Some(2));
        assert_eq!(get(18), Some(99));
        assert_eq!(get(19), None);
    }

    #[test]
    fn test_make_concat(){
        let runtime = Runtime::new();
        let part1 = array_from_vec(&runtime, vec![3, 1, 0, 9]);
        let part2 = array_from_vec(&runtime, vec![5, 92]);
        let con1 = runtime.allocate_ext(Sequence::new_concat(&runtime, vec![part1, part2]).unwrap()).unwrap().unwrap();
        let part3 = array_from_vec(&runtime, vec![3, 45, 16]);
        let part4 = array_from_vec(&runtime, vec![]);
        let part5 = array_from_vec(&runtime, vec![4, 39, 24]);

        let seq = Sequence::new_concat(&runtime, vec![con1, part3, part4, part5]).unwrap();
        assert_eq!(seq.len(), 12);
        let get = |idx| seq.get(idx).and_then(|r| as_prim!(r, Int).cloned());

        assert_eq!(get(0), Some(3));
        assert_eq!(get(5), Some(92));
        assert_eq!(get(6), Some(3));
        assert_eq!(get(8), Some(16));
        assert_eq!(get(10), Some(39));
        let Sequence::Concat(parts) = seq else { panic!() };
        assert_eq!(parts.len(), 4);
    }
    #[test]
    fn test_make_concat_empty(){
        let runtime = Runtime::new();
        let part1 = array_from_vec(&runtime, vec![]);
        let part2 = array_from_vec(&runtime, vec![]);
        let part3 = array_from_vec(&runtime, vec![]);

        let seq = Sequence::new_concat(&runtime, vec![part1, part2, part3]).unwrap();
        assert_eq!(seq.len(), 0);

        let Sequence::Array(v) = seq else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice(){
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 2, 7).unwrap();
        assert_eq!(seq.len(), 5);
        let get = |idx| seq.get(idx).and_then(|r| as_prim!(r, Int).cloned());

        assert_eq!(get(0), Some(0));
        assert_eq!(get(1), Some(9));
        assert_eq!(get(2), Some(5));
        assert_eq!(get(3), Some(92));
        assert_eq!(get(4), Some(4));
    }

    #[test]
    fn test_make_slice_empty_eq_bounds(){
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 2, 2).unwrap();
        assert_eq!(seq.len(), 0);
        let Sequence::Array(v) = seq else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice_empty_gt_bounds(){
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq = Sequence::new_slice(&runtime, source, 7, 2).unwrap();
        assert_eq!(seq.len(), 0);
        let Sequence::Array(v) = seq else { panic!() };
        assert!(v.is_empty());
    }

    #[test]
    fn test_make_slice_nested(){
        let runtime = Runtime::new();
        let source = array_from_vec(&runtime, vec![3, 1, 0, 9, 5, 92, 4, 39, 24]);
        let seq0 = Sequence::new_slice(&runtime, source, 2, 7).unwrap();
        assert_eq!(seq0.len(), 5);
        let seq0 = runtime.allocate_ext(seq0).unwrap().unwrap();
        let seq1 = Sequence::new_slice(&runtime, seq0, 1, 4).unwrap();
        assert_eq!(seq1.len(), 3);
        let get = |idx| seq1.get(idx).and_then(|r| as_prim!(r, Int).cloned());

        assert_eq!(get(0), Some(9));
        assert_eq!(get(1), Some(5));
        assert_eq!(get(2), Some(92));

        let Sequence::Slice(slc) = seq1 else { panic!() };
        assert_eq!(slc.start_idx, 3);
        assert_eq!(slc.end_idx, 6);
        let inner_slc = as_ext!(slc.source, Sequence).unwrap();
        assert!(matches!(inner_slc, Sequence::Array(_)));
    }

    #[test]
    fn test_normalize_idx(){
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
}
