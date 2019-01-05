pub struct DataStack<T> {
    stack: Vec<T>,
    index: usize,
}

impl<T: Clone> DataStack<T> {
    pub fn new(values: Vec<T>) -> Self {
        let index = values.len();
        DataStack {
            stack: values,
            index,
        }
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if self.index > 0 {
            self.index -= 1;
            let x = self.stack[self.index].clone();
            Some(x)
        } else {
            None
        }
    }
    pub fn reset(&mut self) {
        self.index = self.stack.len();
    }
}
