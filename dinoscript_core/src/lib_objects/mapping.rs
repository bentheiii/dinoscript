use std::collections::{hash_map::Entry, HashMap};

use crate::{as_ext, as_prim, catch, dinobj::{AllocatedRef, DinoResult, ExtendedObject}, errors::RuntimeViolation, runtime::{Runtime, SystemRuntimeFrame}};

#[derive(Debug)]
pub struct Mapping<'s>{
    eq_fn: AllocatedRef<'s>,
    hash_fn: AllocatedRef<'s>,
    // if an entry here exists, it is guaranteed to be non-empty
    buckets: Buckets<'s>,
    length: usize
}

fn get_hash<'s>(hash_fn: &AllocatedRef<'s>, key: AllocatedRef<'s>, frame: &SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, BucketKey> {
    let hash_ret = catch!(frame.call(hash_fn, &[Ok(key)])?);
    let Some(&hash) = as_prim!(hash_ret, Int) else {
        return Err(RuntimeViolation::MalformedBytecode);
    };
    // todo when we have bigintegers, we'll need some special logic when the hash is too big, for now, we can just use numeric casting
    Ok(Ok(hash as BucketKey))
}

impl<'s> Mapping<'s> {
    pub const EXPECTED_TYPE_NAME: &'static str = "Mapping";

    pub fn empty(eq_fn: AllocatedRef<'s>, hash_fn: AllocatedRef<'s>) -> Self {
        Self {
            eq_fn,
            hash_fn,
            buckets: Buckets::empty(),
            length: 0
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn with_update(&self, pairs: impl IntoIterator<Item=DinoResult<'s, (AllocatedRef<'s>, AllocatedRef<'s>)>>, frame: &SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, Self> {
        let mut new_buckets = self.buckets.clone(frame.runtime())?;
        let new_eq = frame.runtime().clone_ok_ref(&self.eq_fn)?;
        let new_hash = frame.runtime().clone_ok_ref(&self.hash_fn)?;
        let mut new_length = self.length;

        'outer: for pair_result in pairs {
            let (key, value) = catch!(pair_result?);
            let key_for_hash = frame.runtime().clone_ok_ref(&key)?;
            let hash = catch!(get_hash(&new_hash, key_for_hash, frame)?);
            let bucket = new_buckets.entry(hash);
            match bucket {
                Entry::Occupied(mut bucket) => {
                    let bucket = bucket.get_mut();
                    for (k, v) in bucket.iter_mut() {
                        let key_for_eq = frame.runtime().clone_ok_ref(&key)?;
                        let existing_key_for_eq = frame.runtime().clone_ok_ref(k)?;
                        let eq_ret = catch!(frame.call(&new_eq, &[Ok(key_for_eq), Ok(existing_key_for_eq)])?);
                        let Some(&eq) = as_prim!(eq_ret, Bool) else {
                            return Err(RuntimeViolation::MalformedBytecode);
                        };
                        if eq {
                            *v = value;
                            continue 'outer;
                        }
                    }
                    bucket.push((key, value));
                    new_length += 1;
                }
                Entry::Vacant(bucket) => {
                    bucket.insert(vec![(key, value)]);
                    new_length += 1;
                }
            }
        }
        
        Ok(Ok(Self {
            eq_fn: new_eq,
            hash_fn: new_hash,
            buckets: new_buckets,
            length: new_length
        }))
    }

    pub fn without_key(&self, key: AllocatedRef<'s>, frame: &SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, Option<Self>> {
        let key_for_hash = frame.runtime().clone_ok_ref(&key)?;
        let hash = catch!(get_hash(&self.hash_fn, key_for_hash, frame)?);
        let bucket = self.buckets.get(hash);
        if bucket.is_none() {
            return Ok(Ok(None));
        }
        let bucket = bucket.unwrap();
        let mut found_at = None;
        for (idx, (k, _)) in bucket.iter().enumerate() {
            let key_for_eq = frame.runtime().clone_ok_ref(&key)?;
            let existing_key_for_eq = frame.runtime().clone_ok_ref(k)?;
            let eq_ret = catch!(frame.call(&self.eq_fn, &[Ok(key_for_eq), Ok(existing_key_for_eq)])?);
            let Some(&eq) = as_prim!(eq_ret, Bool) else {
                return Err(RuntimeViolation::MalformedBytecode);
            };
            if eq {
                found_at = Some((hash, idx));
                break;
            }
        }

        if found_at.is_none() {
            return Ok(Ok(None));
        }

        let new_buckets = self.buckets.clone_except_for(frame.runtime(), found_at.unwrap())?;
        let new_eq = frame.runtime().clone_ok_ref(&self.eq_fn)?;
        let new_hash = frame.runtime().clone_ok_ref(&self.hash_fn)?;
        let new_length = self.length - 1;
        
        Ok(Ok(Some(Self {
            eq_fn: new_eq,
            hash_fn: new_hash,
            buckets: new_buckets,
            length: new_length
        })))
    }

    pub fn lookup(&self, key: AllocatedRef<'s>, frame: &SystemRuntimeFrame<'_, 's, '_>) -> DinoResult<'s, Option<AllocatedRef<'s>>> {
        let key_for_hash = frame.runtime().clone_ok_ref(&key)?;
        let hash = catch!(get_hash(&self.hash_fn, key_for_hash, frame)?);
        let bucket = self.buckets.get(hash);
        match bucket {
            Some(bucket) => {
                for (k, v) in bucket {
                    let key_for_eq = frame.runtime().clone_ok_ref(&key)?;
                    let existing_key_for_eq = frame.runtime().clone_ok_ref(k)?;
                    let eq_ret = catch!(frame.call(&self.eq_fn, &[Ok(key_for_eq), Ok(existing_key_for_eq)])?);
                    let Some(&eq) = as_prim!(eq_ret, Bool) else {
                        return Err(RuntimeViolation::MalformedBytecode);
                    };
                    if eq {
                        let v = frame.runtime().clone_ok_ref(v)?;
                        return Ok(Ok(Some(v)));
                    }
                }
                Ok(Ok(None))
            }
            None => Ok(Ok(None))
        }
    }
}



type BucketKey = u64;
type Bucket<'s> = Vec<(AllocatedRef<'s>, AllocatedRef<'s>)>;

// todo allow a faster hasher?
#[derive(Debug)]
struct Buckets<'s>(HashMap<BucketKey, Bucket<'s>>);

impl<'s> Buckets<'s> {
    fn empty() -> Self {
        Self(HashMap::new())
    }

    fn clone(&self, runtime: &Runtime<'s>) -> Result<Self, RuntimeViolation> {
        let mut new_buckets = HashMap::new();
        for (key, bucket) in &self.0 {
            let mut new_bucket = Vec::with_capacity(bucket.len());
            for (k, v) in bucket {
                new_bucket.push((runtime.clone_ok_ref(k)?, runtime.clone_ok_ref(v)?));
            }
            new_buckets.insert(*key, new_bucket);
        }
        Ok(Self(new_buckets))
    }

    fn clone_except_for(&self, runtime: &Runtime<'s>, except: (BucketKey, usize)) -> Result<Self, RuntimeViolation> {
        let mut new_buckets = HashMap::new();
        for (key, bucket) in &self.0 {
            let cap = if *key == except.0 {
                bucket.len() - 1
            } else {
                bucket.len()
            };
            let mut new_bucket = Vec::with_capacity(cap);
            for (i, (k, v)) in bucket.iter().enumerate() {
                if *key == except.0 && i == except.1 {
                    continue;
                }
                new_bucket.push((runtime.clone_ok_ref(k)?, runtime.clone_ok_ref(v)?));
            }
            new_buckets.insert(*key, new_bucket);
        }
        Ok(Self(new_buckets))
    }

    fn entry(&mut self, bucket_key: BucketKey) -> Entry<BucketKey, Bucket<'s>> {
        self.0.entry(bucket_key)
    }

    fn get(&self, bucket_key: BucketKey)-> Option<&Bucket<'s>> {
        self.0.get(&bucket_key)
    }
}

impl<'s> ExtendedObject for Mapping<'s> {
    fn type_name(&self) -> &'static str {
        Self::EXPECTED_TYPE_NAME
    }
    fn allocated_size(&self) -> usize {
        size_of::<Self>()
    }
}