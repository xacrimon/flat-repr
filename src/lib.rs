pub use better_repr_derive::BetterRepr;

#[doc(hidden)]
#[derive(Copy, Clone)]
pub struct DynAlloc {
    pub offset: u16,
    pub len: u16,
}

use std::ops;

pub trait Plain {
    type FlatRepr: ?Sized;
    type RefRepr<'a>;

    #[doc(hidden)]
    fn to_flat(&self) -> Box<Self::FlatRepr>;
}

#[repr(transparent)]
pub struct Flat<T: Plain>(T::FlatRepr);

impl<T: Plain> Flat<T> {
    pub fn from_plain(repr: &T) -> Box<Self> {
        let repr: Box<T::FlatRepr> = repr.to_flat();
        unsafe { Box::from_raw(Box::into_raw(repr) as *mut Self) }
    }
}

impl<T: Plain> ops::Deref for Flat<T> {
    type Target = T::FlatRepr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Plain> ops::DerefMut for Flat<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(transparent)]
pub struct Ref<'a, T: Plain>(T::RefRepr<'a>);

impl<'a, T: Plain> ops::Deref for Ref<'a, T> {
    type Target = T::RefRepr<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T: Plain> ops::DerefMut for Ref<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
