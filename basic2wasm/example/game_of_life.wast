(module
 (import "env" "print" (func $print (param f64)))
 (import "env" "printLabel" (func $printLabel (param i32 i32)))
 (import "env" "printNewline" (func $printNewline))
 (import "env" "printAdvance3" (func $printAdvance3))
 (type $0 (func (param i32 i32) (result i32)))
 (type $1 (func (param i32 i32 i32) (result i32)))
 (type $2 (func (param i32 i32) (result f64)))
 (type $3 (func (param i32 i32 f64)))
 (type $4 (func (param i32 i32 i32) (result f64)))
 (type $5 (func (param i32 i32 i32 f64)))
 (type $7 (func (param f64)))
 (type $8 (func (param i32 i32)))
 (type $9 (func))
 (type $read (func (result f64)))
 (memory $0 1 1)
 (data (i32.const 6) "\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00$@\00\00\00\00\00\00$@")
 (data (i32.const 96) "GEN .O")
 (table 6 anyfunc)
 (elem (i32.const 0) $alloc1d)
 (elem (i32.const 1) $alloc2d)
 (elem (i32.const 2) $load1d)
 (elem (i32.const 3) $load2d)
 (elem (i32.const 4) $store1d)
 (elem (i32.const 5) $store2d)
 (global $data_end (mut i32) (i32.const 96))
 (global $I (mut f64) (f64.const 0))
 (global $N (mut f64) (f64.const 0))
 (global $M (mut f64) (f64.const 0))
 (global $G (mut f64) (f64.const 0))
 (global $Y (mut f64) (f64.const 0))
 (global $X (mut f64) (f64.const 0))
 (global $array_M (mut i32) (i32.const 0))
 (global $array_B (mut i32) (i32.const 0))
 (global $array_L (mut i32) (i32.const 0))
 (global $array_A (mut i32) (i32.const 0))
 (export "alloc1d" (func $alloc1d))
 (export "alloc2d" (func $alloc2d))
 (export "load1d" (func $load1d))
 (export "store1d" (func $store1d))
 (export "load2d" (func $load2d))
 (export "store2d" (func $store2d))
 (export "memory" (memory $0))
 (export "data" (memory $0))
 (export "main" (func $main))
 (func $alloc1d (; 4 ;) (; has Stack IR ;) (type $0) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (i32.store
   (tee_local $2
    (i32.load
     (get_local $0)
    )
   )
   (i32.const 1)
  )
  (i32.store offset=4
   (get_local $2)
   (tee_local $3
    (i32.add
     (get_local $2)
     (i32.const 16)
    )
   )
  )
  (if
   (get_local $1)
   (loop $label$2
    (i64.store
     (get_local $3)
     (i64.const 0)
    )
    (set_local $3
     (i32.add
      (get_local $3)
      (i32.const 8)
     )
    )
    (br_if $label$2
     (i32.lt_u
      (tee_local $4
       (i32.add
        (get_local $4)
        (i32.const 1)
       )
      )
      (get_local $1)
     )
    )
   )
  )
  (i32.store
   (get_local $0)
   (i32.add
    (i32.add
     (get_local $2)
     (i32.shl
      (get_local $1)
      (i32.const 3)
     )
    )
    (i32.const 16)
   )
  )
  (get_local $2)
 )
 (func $alloc2d (; 5 ;) (; has Stack IR ;) (type $1) (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (i32.store offset=8
   (tee_local $3
    (i32.load
     (get_local $0)
    )
   )
   (tee_local $4
    (i32.add
     (get_local $3)
     (i32.const 16)
    )
   )
  )
  (i64.store align=4
   (get_local $3)
   (i64.or
    (i64.shl
     (i64.extend_u/i32
      (get_local $1)
     )
     (i64.const 32)
    )
    (i64.const 1)
   )
  )
  (set_local $5
   (i32.shl
    (tee_local $2
     (i32.mul
      (get_local $2)
      (get_local $1)
     )
    )
    (i32.const 3)
   )
  )
  (if
   (get_local $2)
   (block
    (set_local $1
     (i32.const 0)
    )
    (loop $label$2
     (i64.store
      (get_local $4)
      (i64.const 0)
     )
     (set_local $4
      (i32.add
       (get_local $4)
       (i32.const 8)
      )
     )
     (br_if $label$2
      (i32.lt_u
       (tee_local $1
        (i32.add
         (get_local $1)
         (i32.const 1)
        )
       )
       (get_local $2)
      )
     )
    )
   )
  )
  (i32.store
   (get_local $0)
   (i32.add
    (i32.add
     (get_local $3)
     (get_local $5)
    )
    (i32.const 16)
   )
  )
  (get_local $3)
 )
 (func $load1d (; 6 ;) (; has Stack IR ;) (type $2) (param $0 i32) (param $1 i32) (result f64)
  (f64.load
   (i32.add
    (i32.load offset=4
     (get_local $0)
    )
    (i32.shl
     (i32.mul
      (i32.load
       (get_local $0)
      )
      (get_local $1)
     )
     (i32.const 3)
    )
   )
  )
 )
 (func $store1d (; 7 ;) (; has Stack IR ;) (type $3) (param $0 i32) (param $1 i32) (param $2 f64)
  (f64.store
   (i32.add
    (i32.load offset=4
     (get_local $0)
    )
    (i32.shl
     (i32.mul
      (i32.load
       (get_local $0)
      )
      (get_local $1)
     )
     (i32.const 3)
    )
   )
   (get_local $2)
  )
 )
 (func $load2d (; 8 ;) (; has Stack IR ;) (type $4) (param $0 i32) (param $1 i32) (param $2 i32) (result f64)
  (f64.load
   (i32.add
    (i32.load offset=8
     (get_local $0)
    )
    (i32.shl
     (i32.add
      (i32.mul
       (i32.load
        (i32.add
         (get_local $0)
         (i32.const 4)
        )
       )
       (get_local $2)
      )
      (i32.mul
       (i32.load
        (get_local $0)
       )
       (get_local $1)
      )
     )
     (i32.const 3)
    )
   )
  )
 )
 (func $store2d (; 9 ;) (; has Stack IR ;) (type $5) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64)
  (f64.store
   (i32.add
    (i32.load offset=8
     (get_local $0)
    )
    (i32.shl
     (i32.add
      (i32.mul
       (i32.load
        (i32.add
         (get_local $0)
         (i32.const 4)
        )
       )
       (get_local $2)
      )
      (i32.mul
       (i32.load
        (get_local $0)
       )
       (get_local $1)
      )
     )
     (i32.const 3)
    )
   )
   (get_local $3)
  )
 )
 (func $read (; 10 ;) (; has Stack IR ;) (type $read) (result f64)
  (if (result f64)
   (get_global $data_end)
   (block (result f64)
    (set_global $data_end
     (i32.sub
      (get_global $data_end)
      (i32.const 8)
     )
    )
    (f64.load
     (get_global $data_end)
    )
   )
   (unreachable)
  )
 )
 (func $fn$0 (; 11 ;) (; has Stack IR ;) (type $9)
  (local $0 f64)
  (local $1 f64)
  (call $printLabel
   (i32.const 96)
   (i32.const 4)
  )
  (call $print
   (get_global $I)
  )
  (call $printNewline)
  (set_local $0
   (get_global $M)
  )
  (set_global $Y
   (f64.const 1)
  )
  (block $block$10$break
   (loop $shape$2$continue
    (br_if $block$10$break
     (f64.gt
      (f64.sub
       (get_global $Y)
       (get_local $0)
      )
      (f64.const 0)
     )
    )
    (set_local $1
     (get_global $M)
    )
    (set_global $X
     (f64.const 1)
    )
    (block $block$8$break
     (loop $shape$5$continue
      (br_if $block$8$break
       (f64.gt
        (f64.sub
         (get_global $X)
         (get_local $1)
        )
        (f64.const 0)
       )
      )
      (if
       (f64.eq
        (call_indirect (type $4)
         (get_global $array_A)
         (i32.trunc_u/f64
          (get_global $X)
         )
         (i32.trunc_u/f64
          (get_global $Y)
         )
         (i32.const 3)
        )
        (f64.const 1)
       )
       (call $printLabel
        (i32.const 101)
        (i32.const 1)
       )
       (call $printLabel
        (i32.const 100)
        (i32.const 1)
       )
      )
      (call $printAdvance3)
      (set_global $X
       (f64.add
        (get_global $X)
        (f64.const 1)
       )
      )
      (br $shape$5$continue)
     )
    )
    (call $printNewline)
    (set_global $Y
     (f64.add
      (get_global $Y)
      (f64.const 1)
     )
    )
    (br $shape$2$continue)
   )
  )
 )
 (func $fn$1 (; 12 ;) (; has Stack IR ;) (type $9)
  (local $0 f64)
  (local $1 f64)
  (set_local $0
   (get_global $M)
  )
  (set_global $Y
   (f64.const 1)
  )
  (block $block$7$break
   (loop $shape$2$continue
    (br_if $block$7$break
     (f64.gt
      (f64.sub
       (get_global $Y)
       (get_local $0)
      )
      (f64.const 0)
     )
    )
    (set_local $1
     (get_global $M)
    )
    (set_global $X
     (f64.const 1)
    )
    (block $block$6$break
     (loop $shape$5$continue
      (br_if $block$6$break
       (f64.gt
        (f64.sub
         (get_global $X)
         (get_local $1)
        )
        (f64.const 0)
       )
      )
      (set_global $N
       (f64.add
        (f64.add
         (f64.add
          (f64.add
           (f64.add
            (f64.add
             (f64.add
              (call_indirect (type $4)
               (get_global $array_A)
               (i32.trunc_u/f64
                (f64.sub
                 (get_global $X)
                 (f64.const 1)
                )
               )
               (i32.trunc_u/f64
                (get_global $Y)
               )
               (i32.const 3)
              )
              (call_indirect (type $4)
               (get_global $array_A)
               (i32.trunc_u/f64
                (f64.add
                 (get_global $X)
                 (f64.const 1)
                )
               )
               (i32.trunc_u/f64
                (get_global $Y)
               )
               (i32.const 3)
              )
             )
             (call_indirect (type $4)
              (get_global $array_A)
              (i32.trunc_u/f64
               (get_global $X)
              )
              (i32.trunc_u/f64
               (f64.sub
                (get_global $Y)
                (f64.const 1)
               )
              )
              (i32.const 3)
             )
            )
            (call_indirect (type $4)
             (get_global $array_A)
             (i32.trunc_u/f64
              (get_global $X)
             )
             (i32.trunc_u/f64
              (f64.add
               (get_global $Y)
               (f64.const 1)
              )
             )
             (i32.const 3)
            )
           )
           (call_indirect (type $4)
            (get_global $array_A)
            (i32.trunc_u/f64
             (f64.sub
              (get_global $X)
              (f64.const 1)
             )
            )
            (i32.trunc_u/f64
             (f64.sub
              (get_global $Y)
              (f64.const 1)
             )
            )
            (i32.const 3)
           )
          )
          (call_indirect (type $4)
           (get_global $array_A)
           (i32.trunc_u/f64
            (f64.add
             (get_global $X)
             (f64.const 1)
            )
           )
           (i32.trunc_u/f64
            (f64.add
             (get_global $Y)
             (f64.const 1)
            )
           )
           (i32.const 3)
          )
         )
         (call_indirect (type $4)
          (get_global $array_A)
          (i32.trunc_u/f64
           (f64.sub
            (get_global $X)
            (f64.const 1)
           )
          )
          (i32.trunc_u/f64
           (f64.add
            (get_global $Y)
            (f64.const 1)
           )
          )
          (i32.const 3)
         )
        )
        (call_indirect (type $4)
         (get_global $array_A)
         (i32.trunc_u/f64
          (f64.add
           (get_global $X)
           (f64.const 1)
          )
         )
         (i32.trunc_u/f64
          (f64.sub
           (get_global $Y)
           (f64.const 1)
          )
         )
         (i32.const 3)
        )
       )
      )
      (call_indirect (type $5)
       (get_global $array_B)
       (i32.trunc_u/f64
        (get_global $X)
       )
       (i32.trunc_u/f64
        (get_global $Y)
       )
       (call_indirect (type $4)
        (get_global $array_L)
        (i32.trunc_u/f64
         (call_indirect (type $4)
          (get_global $array_A)
          (i32.trunc_u/f64
           (get_global $X)
          )
          (i32.trunc_u/f64
           (get_global $Y)
          )
          (i32.const 3)
         )
        )
        (i32.trunc_u/f64
         (get_global $N)
        )
        (i32.const 3)
       )
       (i32.const 5)
      )
      (set_global $X
       (f64.add
        (get_global $X)
        (f64.const 1)
       )
      )
      (br $shape$5$continue)
     )
    )
    (set_global $Y
     (f64.add
      (get_global $Y)
      (f64.const 1)
     )
    )
    (br $shape$2$continue)
   )
  )
 )
 (func $fn$2 (; 13 ;) (; has Stack IR ;) (type $9)
  (local $0 f64)
  (local $1 f64)
  (set_local $0
   (get_global $M)
  )
  (set_global $Y
   (f64.const 1)
  )
  (block $block$7$break
   (loop $shape$2$continue
    (br_if $block$7$break
     (f64.gt
      (f64.sub
       (get_global $Y)
       (get_local $0)
      )
      (f64.const 0)
     )
    )
    (set_local $1
     (get_global $M)
    )
    (set_global $X
     (f64.const 1)
    )
    (block $block$6$break
     (loop $shape$5$continue
      (br_if $block$6$break
       (f64.gt
        (f64.sub
         (get_global $X)
         (get_local $1)
        )
        (f64.const 0)
       )
      )
      (call_indirect (type $5)
       (get_global $array_A)
       (i32.trunc_u/f64
        (get_global $X)
       )
       (i32.trunc_u/f64
        (get_global $Y)
       )
       (call_indirect (type $4)
        (get_global $array_B)
        (i32.trunc_u/f64
         (get_global $X)
        )
        (i32.trunc_u/f64
         (get_global $Y)
        )
        (i32.const 3)
       )
       (i32.const 5)
      )
      (set_global $X
       (f64.add
        (get_global $X)
        (f64.const 1)
       )
      )
      (br $shape$5$continue)
     )
    )
    (set_global $Y
     (f64.add
      (get_global $Y)
      (f64.const 1)
     )
    )
    (br $shape$2$continue)
   )
  )
 )
 (func $main (; 14 ;) (; has Stack IR ;) (type $9)
  (local $0 f64)
  (set_global $data_end
   (i32.const 96)
  )
  (i32.store
   (i32.const 104)
   (i32.const 112)
  )
  (set_global $array_A
   (call_indirect (type $1)
    (i32.const 104)
    (i32.const 20)
    (i32.const 20)
    (i32.const 1)
   )
  )
  (set_global $array_B
   (call_indirect (type $1)
    (i32.const 104)
    (i32.const 30)
    (i32.const 30)
    (i32.const 1)
   )
  )
  (set_global $array_M
   (call_indirect (type $1)
    (i32.const 104)
    (i32.const 10)
    (i32.const 10)
    (i32.const 1)
   )
  )
  (set_global $array_L
   (call_indirect (type $1)
    (i32.const 104)
    (i32.const 50)
    (i32.const 50)
    (i32.const 1)
   )
  )
  (set_global $G
   (call $read)
  )
  (set_global $M
   (call $read)
  )
  (call_indirect (type $5)
   (get_global $array_L)
   (i32.const 0)
   (i32.const 3)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_L)
   (i32.const 1)
   (i32.const 3)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_L)
   (i32.const 1)
   (i32.const 2)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 3)
   (i32.const 4)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 3)
   (i32.const 5)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 3)
   (i32.const 6)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 6)
   (i32.const 5)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 6)
   (i32.const 6)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 7)
   (i32.const 5)
   (call $read)
   (i32.const 5)
  )
  (call_indirect (type $5)
   (get_global $array_A)
   (i32.const 7)
   (i32.const 6)
   (call $read)
   (i32.const 5)
  )
  (set_global $I
   (f64.const 0)
  )
  (call $fn$0)
  (set_local $0
   (get_global $G)
  )
  (set_global $I
   (f64.const 1)
  )
  (block $block$5$break
   (loop $shape$3$continue
    (br_if $block$5$break
     (f64.gt
      (f64.sub
       (get_global $I)
       (get_local $0)
      )
      (f64.const 0)
     )
    )
    (call $fn$1)
    (call $fn$2)
    (call $fn$0)
    (set_global $I
     (f64.add
      (get_global $I)
      (f64.const 1)
     )
    )
    (br $shape$3$continue)
   )
  )
 )
)
