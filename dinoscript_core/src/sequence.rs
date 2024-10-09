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
}

type SequenceConcat<'s> = Vec<SequenceConcatPart<'s>>;

#[derive(Debug)]
struct SequenceConcatPart<'s>{
    part: AllocatedRef<'s>,
    end_idx: usize,
}

const MAX_CONCAT_LIN_SEARCH: usize = 5;
    

impl<'s> Sequence<'s> {
    pub const EXPECTED_TYPE_NAME: &'static str = "Sequence";

    pub fn new_array(array: Vec<AllocatedRef<'s>>) -> Self {
        Self::Array(array)
    }

    pub fn new_concat(runtime: &Runtime<'s>, seqs: Vec<AllocatedRef<'s>>) -> Result<Self, RuntimeViolation> {
        fn get_parts_iter<'a, 's>(part: &'a AllocatedRef<'s>)->Box<dyn Iterator<Item=&'a AllocatedRef<'s>> + 'a>{
            let part_seq: &'a Sequence<'s> = as_ext!(part, Sequence).unwrap();
            match part_seq {
                Sequence::Concat(parts) => Box::new(parts.iter().map(|part| &part.part)),
                Sequence::Array(arr) if arr.is_empty() => Box::new(std::iter::empty()),
                _ => Box::new(std::iter::once(part)),
            }
        }
        let mut parts = Vec::with_capacity(seqs.len());
        let mut end_idx = 0;
        for part in seqs.iter().flat_map(|part| get_parts_iter(part)) {
            let part_seq: &Sequence<'s> = as_ext!(part, Sequence).unwrap();
            end_idx += part_seq.len();
            let part = runtime.clone_ref(part)?;
            parts.push(SequenceConcatPart{part, end_idx});
        }
        Ok(Self::Concat(parts))
    }

    pub fn get(&self, index: usize) -> Option<&AllocatedRef<'s>> {
        match self {
            Self::Array(array) => array.get(index),
            Self::Concat(parts) => {
                if index >= parts.last().unwrap().end_idx {
                    return None;
                }
                let (idx_offset, relevant_part) = if parts[0].end_idx > index{
                    (0, &parts[0])
                }
                else if parts.len() <= MAX_CONCAT_LIN_SEARCH{
                    // linear search
                    parts.iter().tuple_windows().map(|(prev_part, part)| (prev_part.end_idx, part)).find(|(_, part)| part.end_idx > index).unwrap()
                } else {
                    // binary search
                    // we treat this as a binary search over the windowed tuples
                    let mut left = 0;
                    let mut right = parts.len() - 2;
                    loop {
                        let mid = (left + right) / 2;
                        let mid_part_prev = &parts[mid];
                        if mid_part_prev.end_idx > index {
                            right = mid - 1;
                            continue;
                        }
                        let mid_part = &parts[mid + 1];
                        if mid_part.end_idx <= index {
                            left = mid + 1;
                        } else {
                            break (mid_part_prev.end_idx, mid_part);
                        }
                    }
                };
                let part: &Sequence<'_> = as_ext!(relevant_part.part, Sequence)?;
                part.get(index - idx_offset)
            },
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Array(array) => array.len(),
            Self::Concat(array) => array.last().unwrap().end_idx,
        }
    }

    pub fn iter<'a>(&'a self)->Box<dyn Iterator<Item=&'a AllocatedRef<'s>> + 'a>{
        match self {
            Self::Array(array) => Box::new(array.iter()),
            Self::Concat(array) => {
                let mut iter = Vec::new();
                for part in array {
                    let part: &Sequence<'_> = as_ext!(part.part, Sequence).unwrap();
                    iter.push(part.iter());
                }
                Box::new(iter.into_iter().flatten())
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

        let seq = Sequence::Concat(vec![
            SequenceConcatPart{part: part1, end_idx: 4},
            SequenceConcatPart{part: part2, end_idx: 6},
            SequenceConcatPart{part: part3, end_idx: 9},
        ]);

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

        let seq = Sequence::Concat(vec![
            SequenceConcatPart{part: part1, end_idx: 4},
            SequenceConcatPart{part: part2, end_idx: 6},
            SequenceConcatPart{part: part3, end_idx: 9},
            SequenceConcatPart{part: part4, end_idx: 12},
            SequenceConcatPart{part: part5, end_idx: 16},
            SequenceConcatPart{part: part6, end_idx: 19},
        ]);

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
}
