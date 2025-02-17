pub use better_repr_derive::BetterRepr;

#[doc(hidden)]
#[derive(Copy, Clone)]
pub struct DynAlloc {
    pub offset: u16,
    pub len: u16,
}

use std::ops;

pub trait ReprFlat {
    type Ty: ?Sized;

    #[doc(hidden)]
    fn to_flat(&self) -> Box<Self::Ty>;
}

#[repr(transparent)]
pub struct Flat<T: ReprFlat>(T::Ty);

impl<T: ReprFlat> Flat<T> {
    pub fn from_plain(repr: &T) -> Box<Self> {
        let repr: Box<T::Ty> = repr.to_flat();
        unsafe { Box::from_raw(Box::into_raw(repr) as *mut Self) }
    }
}

impl<T: ReprFlat> ops::Deref for Flat<T> {
    type Target = T::Ty;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ReprFlat> ops::DerefMut for Flat<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait ReprRef {
    type Ty<'a>;
}

#[repr(transparent)]
pub struct Ref<'a, T: ReprRef>(T::Ty<'a>);

impl<'a, T: ReprRef> ops::Deref for Ref<'a, T> {
    type Target = T::Ty<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T: ReprRef> ops::DerefMut for Ref<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
