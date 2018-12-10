pub struct DataStack<T> {
    stack: Vec<T>,
    index: usize,
}

impl<T: Copy> DataStack<T> {
    pub fn new() -> Self {
        DataStack {
            stack: vec![],
            index: 0,
        }
    }

    pub fn push_back(&mut self, x: T) {
        self.stack.push(x);
    }
    pub fn pop_front(&mut self) -> Option<T> {
        let x = self.stack.get(self.index).cloned();
        if x.is_some() {
            self.index += 1;
        }
        x
    }
    pub fn reset(&mut self) {
        self.index = 0;
    }
}
