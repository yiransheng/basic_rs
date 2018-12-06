use nanbox::*;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Copy, Clone)]
pub struct True;

#[derive(Copy, Clone)]
pub struct False;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, FromPrimitive, ToPrimitive)]
pub struct FuncId(u8);

impl FuncId {
    #[inline(always)]
    pub fn raw(self) -> u8 {
        self.0
    }
}

pub struct FuncIdGen {
    id: u8,
}

impl FuncIdGen {
    pub fn new() -> Self {
        FuncIdGen { id: 0 }
    }
    pub fn next_id(&mut self) -> FuncId {
        assert!(self.id < u8::max_value());

        let r = FuncId(self.id);
        self.id += 1;
        r
    }
}

impl NanBoxable for True {
    unsafe fn from_nan_box(_n: NanBox) -> Self {
        True
    }

    fn into_nan_box(self) -> NanBox {
        1u64.into_nan_box()
    }
}

impl NanBoxable for False {
    unsafe fn from_nan_box(_n: NanBox) -> Self {
        False
    }

    fn into_nan_box(self) -> NanBox {
        0u64.into_nan_box()
    }
}

impl NanBoxable for FuncId {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        FuncId(u8::from_nan_box(n))
    }

    fn into_nan_box(self) -> NanBox {
        self.0.into_nan_box()
    }
}

#[rustfmt::skip]
make_nanbox!{
    #[derive(Copy, Clone)]
    pub unsafe enum Value, Variant {
        Number(f64),
        True(True),
        False(False),
        Function(FuncId)
    }
}

impl Value {
    pub fn true_value() -> Self {
        Value::from(Variant::True(True))
    }
    pub fn false_value() -> Self {
        Value::from(Variant::False(False))
    }
}
