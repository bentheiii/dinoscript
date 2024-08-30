use std::ops::Deref;

pub enum MaybeOwned<'a, T> {
    Borrowed(&'a T),
    Owned(T),
}

impl<'a, T> MaybeOwned<'a, T> {
    pub fn as_ref(&self) -> &T {
        match self {
            MaybeOwned::Borrowed(t) => t,
            MaybeOwned::Owned(t) => t,
        }
    }

    pub fn borrowed(&self) -> MaybeOwned<T> {
        match self {
            MaybeOwned::Borrowed(t) => MaybeOwned::Borrowed(t),
            MaybeOwned::Owned(t) => MaybeOwned::Borrowed(t),
        }
    }
}

impl<'a, T> Deref for MaybeOwned<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
