use std::marker::PhantomData;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum State<T> {
    Matched(T),
    Fail,
    Intermediate(u8),
}

pub trait Dfa {
    type Output;

    fn match_str(&mut self, s: &str) -> Option<(Self::Output, usize)>;

    fn map<T, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> T,
    {
        Map { inner: self, f }
    }

    fn alternative<T>(self) -> Alt<Self, T>
    where
        Self: Sized,
        T: Dfa + Default,
        T::Output: Into<Self::Output>,
    {
        Alt {
            left: self,
            right: PhantomData,
        }
    }
}

pub struct Map<D, F> {
    inner: D,
    f: F,
}

impl<D, T, F> Dfa for Map<D, F>
where
    D: Dfa,
    F: Fn(D::Output) -> T,
{
    type Output = T;

    fn match_str(&mut self, s: &str) -> Option<(Self::Output, usize)> {
        self.inner.match_str(s).map(|(x, c)| ((self.f)(x), c))
    }
}

pub struct Alt<T, U> {
    left: T,
    right: PhantomData<U>,
}

impl<T, U> Dfa for Alt<T, U>
where
    T: Dfa,
    U: Dfa + Default,
    U::Output: Into<T::Output>,
{
    type Output = T::Output;

    fn match_str(&mut self, s: &str) -> Option<(Self::Output, usize)> {
        match self.left.match_str(s) {
            Some((x, c)) => return Some((x, c)),
            _ => {}
        }

        let mut right = U::default();
        right.match_str(s).map(|(x, c)| (x.into(), c))
    }
}
