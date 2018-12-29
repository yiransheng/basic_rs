use std::mem;

trait Stride: Copy {
    fn to_offset(self, index: Self) -> usize;
}
impl Stride for u8 {
    fn to_offset(self, index: Self) -> usize {
        (self * index) as usize
    }
}
impl Stride for [u8; 2] {
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

static mut NEXT_PTR: i32 = 0;

#[no_mangle]
pub extern "C" fn free_all() {
    unsafe {
        NEXT_PTR = 0;
    }
}

#[no_mangle]
pub extern "C" fn alloc1d(size: u32) -> *mut Array<u8> {
    let meta_size = 8; // 8bytes for Array<u8> to satisfy f64 alignment
    debug_assert!(meta_size >= mem::size_of::<Array<u8>>());

    let data_size = mem::size_of::<f64>() * (size as usize);
    unsafe {
        let ptr = NEXT_PTR as *mut Array<u8>;
        let data_ptr = (NEXT_PTR as *mut u8).offset(meta_size as isize);
        let mut data_ptr = data_ptr as *mut f64;
        *ptr = Array {
            stride: 1,
            data: data_ptr,
        };
        for i in 0..size {
            *data_ptr = 0.0;
            data_ptr = data_ptr.offset(1);
        }
        NEXT_PTR = NEXT_PTR + (meta_size as i32) + (data_size as i32);
        ptr
    }
}

#[no_mangle]
pub extern "C" fn alloc2d(row_size: u32, col_size: u32) -> *mut Array<[u8; 2]> {
    let meta_size = 8; // for alignment
    debug_assert!(meta_size >= mem::size_of::<Array<[u8; 2]>>());
    let data_size = mem::size_of::<f64>() * ((row_size * col_size) as usize);

    unsafe {
        let ptr = NEXT_PTR as *mut Array<[u8; 2]>;
        let data_ptr = (NEXT_PTR as *mut u8).offset(meta_size as isize);
        let mut data_ptr = data_ptr as *mut f64;
        *ptr = Array {
            stride: [1, row_size as u8],
            data: data_ptr,
        };
        for i in 0..(row_size * col_size) {
            *data_ptr = 0.0;
            data_ptr = data_ptr.offset(1);
        }
        NEXT_PTR = NEXT_PTR + (meta_size as i32) + (data_size as i32);
        ptr
    }
}

#[no_mangle]
pub extern "C" fn load1d(ptr: *mut Array<u8>, index: u8) -> f64 {
    unsafe { (*ptr).load(index) }
}
#[no_mangle]
pub extern "C" fn store1d(ptr: *mut Array<u8>, index: u8, val: f64) {
    unsafe {
        (*ptr).store(index, val);
    }
}
#[no_mangle]
pub extern "C" fn load2d(ptr: *mut Array<[u8; 2]>, row: u8, col: u8) -> f64 {
    unsafe { (*ptr).load([row, col]) }
}
#[no_mangle]
pub extern "C" fn store2d(
    ptr: *mut Array<[u8; 2]>,
    row: u8,
    col: u8,
    val: f64,
) {
    unsafe {
        (*ptr).store([row, col], val);
    }
}
