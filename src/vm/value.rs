use nanbox::*;

#[derive(Copy, Clone)]
pub struct True;

#[derive(Copy, Clone)]
pub struct False;

#[derive(Copy, Clone)]
pub struct _NoData {}

impl NanBoxable for _NoData {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        _NoData {}
    }

    fn into_nan_box(self) -> NanBox {
        0u64.into_nan_box()
    }
}

impl NanBoxable for True {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        True
    }

    fn into_nan_box(self) -> NanBox {
        1u64.into_nan_box()
    }
}

impl NanBoxable for False {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        False
    }

    fn into_nan_box(self) -> NanBox {
        0u64.into_nan_box()
    }
}

#[rustfmt::skip]
make_nanbox!{
    #[derive(Copy, Clone)]
    pub unsafe enum Value, Variant {
	Number(f64),
	True(True),
	False(False),
        // Sentinel written by DATA statement
        // When READ should exit program
        // without error
	NoData(_NoData)
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
