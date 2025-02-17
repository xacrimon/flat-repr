pub use better_repr_derive::BetterRepr;
use std::{ops, u16};

#[doc(hidden)]
#[derive(Copy, Clone, PartialEq)]
pub struct DynAlloc<Len = u16> {
    pub offset: u16,
    pub len: Len,
}

impl<Len> DynAlloc<Len>
where
    Len: AllocLength,
{
    pub const NONE: Self = Self {
        offset: u16::MAX,
        len: Len::UNIT,
    };

    pub fn next_offset(
        next_offset: &mut usize,
        len: usize,
        elem_size: usize,
        align: usize,
    ) -> Self {
        while *next_offset % align != 0 {
            *next_offset += 1;
        }

        let offset = *next_offset;
        *next_offset += len * elem_size;

        Self {
            offset: offset as u16,
            len: Len::from_usize(len),
        }
    }
}

pub trait AllocLength {
    const UNIT: Self;

    fn from_usize(len: usize) -> Self;
}

impl AllocLength for () {
    const UNIT: Self = ();

    fn from_usize(_len: usize) -> Self {
        ()
    }
}

impl AllocLength for u16 {
    const UNIT: Self = 0;

    fn from_usize(len: usize) -> Self {
        len as u16
    }
}

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
