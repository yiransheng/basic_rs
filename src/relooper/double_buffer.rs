use std::ops::{Deref, DerefMut};

pub(super) struct DblBuffer<T> {
    swapped: bool,
    first: T,
    second: T,
}
impl<T> DblBuffer<T>
where
    T: Default,
{
    pub(super) fn new() -> Self {
        DblBuffer {
            swapped: false,
            first: T::default(),
            second: T::default(),
        }
    }
    pub(super) fn swap(&mut self) {
        self.swapped = !self.swapped;
    }
}
impl<T> Deref for DblBuffer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        if self.swapped {
            &self.first
        } else {
            &self.second
        }
    }
}
impl<T> DerefMut for DblBuffer<T> {
    fn deref_mut(&mut self) -> &mut T {
        if self.swapped {
            &mut self.first
        } else {
            &mut self.second
        }
    }
}
