pub trait Subscript {
    fn to_usize(self, arr: &Array<Self>) -> Option<usize>
    where
        Self: Sized;
}

impl Subscript for u8 {
    fn to_usize(self, arr: &Array<Self>) -> Option<usize> {
        if self < arr.bound {
            Some(self as usize)
        } else {
            None
        }
    }
}

impl Subscript for [u8; 2] {
    fn to_usize(self, arr: &Array<Self>) -> Option<usize> {
        let [m, n] = arr.bound;
        let [i, j] = self;
        if i < m && j < n {
            Some((i * n + j) as usize)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Error {
    OutOfBound,
    UnInitialized,
    RedefineDim,
}

pub struct Array<I> {
    values: Vec<Option<f64>>,
    bound: I,
}

impl<I> Array<I> {
    pub fn new(bound: I) -> Self {
        Array {
            bound,
            values: Vec::new(),
        }
    }
}

impl<I: Subscript> Array<I> {
    pub fn get(&self, i: I) -> Result<f64, Error> {
        let index = i.to_usize(self).ok_or(Error::OutOfBound)?;
        let v = self
            .values
            .get(index)
            .and_then(|x| *x)
            .ok_or(Error::UnInitialized)?;
        Ok(v)
    }
    pub fn set(&mut self, i: I, x: f64) -> Result<(), Error> {
        let index = i.to_usize(self).ok_or(Error::OutOfBound)?;
        let v = self.values.get_mut(index);
        if let Some(v) = v {
            *v = Some(x);
            Ok(())
        } else {
            self.grow_to(index);
            self.values[index] = Some(x);
            Ok(())
        }
    }

    pub fn set_bound(&mut self, bound: I) -> Result<(), Error> {
        if self.values.is_empty() {
            self.bound = bound;
            Ok(())
        } else {
            Err(Error::RedefineDim)
        }
    }

    #[inline]
    fn grow_to(&mut self, index: usize) {
        while self.values.len() <= index + 1 {
            self.values.push(None);
        }
    }
}
