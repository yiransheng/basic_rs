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

pub struct Array<I> {
    values: Vec<f64>,
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
    pub fn get(&self, i: I) -> Option<f64> {
        let index = i.to_usize(self)?;
        let v = self.values.get(index)?;
        Some(*v)
    }
    pub fn set(&mut self, i: I, x: f64) -> Option<()> {
        let index = i.to_usize(self)?;
        let v = self.values.get_mut(index)?;
        *v = x;
        Some(())
    }
}
