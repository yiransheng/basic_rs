use std::mem;

trait Stride: Copy {
    fn to_offset(self, index: Self) -> usize;
}
impl Stride for i32 {
    fn to_offset(self, index: Self) -> usize {
        (self * index) as usize
    }
}
impl Stride for [i32; 2] {
    fn to_offset(self, index: Self) -> usize {
        let [row_s, col_s] = self;
        let [i, j] = index;
        (row_s * i + col_s * j) as usize
    }
}

pub struct Array<S> {
    stride: S,
    data: *mut f64,
}

impl<S: Stride> Array<S> {
    fn load(&self, index: S) -> f64 {
        let i = self.stride.to_offset(index);
        unsafe { *self.data.offset(i as isize) }
    }
    fn store(&mut self, index: S, val: f64) {
        let i = self.stride.to_offset(index);
        unsafe {
            *self.data.offset(i as isize) = val;
        }
    }
}

fn alloc_array<S: Stride + Sized>(
    next_ptr: *mut *mut u8,
    size: usize,
    stride: S,
) -> *mut Array<S> {
    //TODO: use  align_offset instead of hardcoding
    let meta_size = 16; // enough for Array<S>
    let data_size = mem::size_of::<f64>() * size;

    unsafe {
        let ptr: *mut u8 = *next_ptr;
        let data_ptr = ptr.offset(meta_size as isize);

        let arr_ptr = ptr as *mut Array<S>;
        let mut data_ptr = data_ptr as *mut f64;

        *arr_ptr = Array {
            stride,
            data: data_ptr,
        };
        for i in 0..size {
            *data_ptr = 0.0;
            data_ptr = data_ptr.offset(1);
        }

        *next_ptr = ptr.offset((meta_size + data_size) as isize);

        arr_ptr
    }
}

#[no_mangle]
pub extern "C" fn alloc1d(
    next_ptr: *mut *mut u8,
    size: i32,
) -> *mut Array<i32> {
    alloc_array(next_ptr, size as usize, 1)
}

#[no_mangle]
pub extern "C" fn alloc2d(
    next_ptr: *mut *mut u8,
    row: i32,
    col: i32,
) -> *mut Array<[i32; 2]> {
    let size = (row * col) as usize;
    alloc_array(next_ptr, size, [1, row])
}

#[no_mangle]
pub extern "C" fn load1d(ptr: *mut Array<i32>, index: i32) -> f64 {
    unsafe { (*ptr).load(index) }
}
#[no_mangle]
pub extern "C" fn store1d(ptr: *mut Array<i32>, index: i32, val: f64) {
    unsafe {
        (*ptr).store(index, val);
    }
}
#[no_mangle]
pub extern "C" fn load2d(ptr: *mut Array<[i32; 2]>, row: i32, col: i32) -> f64 {
    unsafe { (*ptr).load([row, col]) }
}
#[no_mangle]
pub extern "C" fn store2d(
    ptr: *mut Array<[i32; 2]>,
    row: i32,
    col: i32,
    val: f64,
) {
    unsafe {
        (*ptr).store([row, col], val);
    }
}
