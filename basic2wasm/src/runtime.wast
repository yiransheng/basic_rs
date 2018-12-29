(module
  (type $t0 (func (param i32) (result i32)))
  (type $t1 (func (param i32 i32 i32) (result i32)))
  (type $t2 (func (param i32 i32) (result i32)))
  (type $t3 (func (param i32 i32) (result f64)))
  (type $t4 (func (param i32 i32 f64)))
  (type $t5 (func (param i32 i32 i32) (result f64)))
  (type $t6 (func (param i32 i32 i32 f64)))
  (func $alloc1d (export "alloc1d") (type $t0) (param $p0 i32) (result i32)
    (local $l0 i32) (local $l1 i32) (local $l2 i32)
    i32.const 0
    i32.load offset=1024
    tee_local $l0
    i32.const 1
    i32.store8 offset=4
    get_local $l0
    get_local $l0
    i32.const 8
    i32.add
    tee_local $l1
    i32.store
    get_local $p0
    i32.const 3
    i32.shl
    set_local $l2
    block $B0
      get_local $p0
      i32.eqz
      br_if $B0
      get_local $l1
      i32.const 0
      get_local $l2
      call $memset
      drop
    end
    i32.const 0
    get_local $l2
    i32.const 0
    i32.load offset=1024
    i32.add
    i32.const 8
    i32.add
    i32.store offset=1024
    get_local $l0)
  (func $alloc2d (export "alloc2d") (type $t2) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l0 i32) (local $l1 i32)
    i32.const 0
    i32.load offset=1024
    tee_local $l0
    i32.const 1
    i32.store8 offset=4
    get_local $l0
    i32.const 5
    i32.add
    get_local $p0
    i32.store8
    get_local $l0
    get_local $l0
    i32.const 8
    i32.add
    tee_local $l1
    i32.store
    get_local $p1
    get_local $p0
    i32.mul
    tee_local $p1
    i32.const 3
    i32.shl
    set_local $p0
    block $B0
      get_local $p1
      i32.eqz
      br_if $B0
      get_local $l1
      i32.const 0
      get_local $p0
      call $memset
      drop
    end
    i32.const 0
    get_local $p0
    i32.const 0
    i32.load offset=1024
    i32.add
    i32.const 8
    i32.add
    i32.store offset=1024
    get_local $l0)
  (func $load1d (export "load1d") (type $t3) (param $p0 i32) (param $p1 i32) (result f64)
    get_local $p0
    i32.load
    get_local $p0
    i32.load8_u offset=4
    get_local $p1
    i32.mul
    i32.const 255
    i32.and
    i32.const 3
    i32.shl
    i32.add
    f64.load)
  (func $store1d (export "store1d") (type $t4) (param $p0 i32) (param $p1 i32) (param $p2 f64)
    get_local $p0
    i32.load
    get_local $p0
    i32.load8_u offset=4
    get_local $p1
    i32.mul
    i32.const 255
    i32.and
    i32.const 3
    i32.shl
    i32.add
    get_local $p2
    f64.store)
  (func $load2d (export "load2d") (type $t5) (param $p0 i32) (param $p1 i32) (param $p2 i32) (result f64)
    get_local $p0
    i32.load
    get_local $p0
    i32.load16_u offset=4 align=1
    tee_local $p0
    get_local $p1
    i32.mul
    get_local $p0
    i32.const 8
    i32.shr_u
    get_local $p2
    i32.mul
    i32.add
    i32.const 255
    i32.and
    i32.const 3
    i32.shl
    i32.add
    f64.load)
  (func $store2d (export "store2d") (type $t6) (param $p0 i32) (param $p1 i32) (param $p2 i32) (param $p3 f64)
    get_local $p0
    i32.load
    get_local $p0
    i32.load16_u offset=4 align=1
    tee_local $p0
    get_local $p1
    i32.mul
    get_local $p0
    i32.const 8
    i32.shr_u
    get_local $p2
    i32.mul
    i32.add
    i32.const 255
    i32.and
    i32.const 3
    i32.shl
    i32.add
    get_local $p3
    f64.store)
  (func $memset (type $t1) (param $p0 i32) (param $p1 i32) (param $p2 i32) (result i32)
    (local $l0 i32)
    block $B0
      get_local $p2
      i32.eqz
      br_if $B0
      get_local $p0
      set_local $l0
      loop $L1
        get_local $l0
        get_local $p1
        i32.store8
        get_local $l0
        i32.const 1
        i32.add
        set_local $l0
        get_local $p2
        i32.const -1
        i32.add
        tee_local $p2
        br_if $L1
      end
    end
    get_local $p0)
  (table $T0 6 anyfunc)
  (memory $memory (export "memory") 17)
  (elem (i32.const 0) $alloc1d)
  (elem (i32.const 1) $alloc2d)
  (elem (i32.const 2) $load1d)
  (elem (i32.const 3) $load2d)
  (elem (i32.const 4) $store1d)
  (elem (i32.const 5) $store2d)
  (data (i32.const 1024) "\00\00\00\00\00"))
