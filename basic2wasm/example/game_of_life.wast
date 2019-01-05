(module
 (type $1 (func (param f64)))
 (type $2 (func (param i32 i32)))
 (type $3 (func))
 (type $read (func (result f64)))
 (type $load2d (func (param i32 i32 i32) (result f64)))
 (type $store2d (func (param i32 i32 i32 f64)))
 (memory $0 1 1)
 (data (i32.const 6) "\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00\f0?\00\00\00\00\00\00$@\00\00\00\00\00\00$@")
 (data (i32.const 96) "GEN .O")
 (import "env" "print" (func $print (param f64)))
 (import "env" "printLabel" (func $printLabel (param i32 i32)))
 (import "env" "printNewline" (func $printNewline))
 (import "env" "printAdvance3" (func $printAdvance3))
 (global $data_end (mut i32) (i32.const 96))
 (global $I (mut f64) (f64.const 0))
 (global $array_B (mut i32) (i32.const 0))
 (global $M (mut f64) (f64.const 0))
 (global $array_A (mut i32) (i32.const 0))
 (global $Y (mut f64) (f64.const 0))
 (global $X (mut f64) (f64.const 0))
 (global $array_L (mut i32) (i32.const 0))
 (export "data" (memory $0))
 (export "main" (func $main))
 (func $read (; 4 ;) (; has Stack IR ;) (type $read) (result f64)
  (if (result f64)
   (i32.gt_s
    (get_global $data_end)
    (i32.const 0)
   )
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
 (func $alloc2d (; 5 ;) (; has Stack IR ;) (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (i32.store offset=8
   (tee_local $2
    (i32.load
     (i32.const 104)
    )
   )
   (tee_local $3
    (i32.add
     (get_local $2)
     (i32.const 16)
    )
   )
  )
  (i64.store align=4
   (get_local $2)
   (i64.or
    (i64.shl
     (i64.extend_u/i32
      (get_local $0)
     )
     (i64.const 32)
    )
    (i64.const 1)
   )
  )
  (set_local $4
   (i32.shl
    (tee_local $1
     (i32.mul
      (get_local $0)
      (get_local $1)
     )
    )
    (i32.const 3)
   )
  )
  (if
   (get_local $1)
   (block
    (set_local $0
     (i32.const 0)
    )
    (loop $$label$2
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
     (br_if $$label$2
      (i32.lt_u
       (tee_local $0
        (i32.add
         (get_local $0)
         (i32.const 1)
        )
       )
       (get_local $1)
      )
     )
    )
   )
  )
  (i32.store
   (i32.const 104)
   (i32.add
    (i32.add
     (get_local $2)
     (get_local $4)
    )
    (i32.const 16)
   )
  )
  (get_local $2)
 )
 (func $load2d (; 6 ;) (; has Stack IR ;) (type $load2d) (param $0 i32) (param $1 i32) (param $2 i32) (result f64)
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
 (func $store2d (; 7 ;) (; has Stack IR ;) (type $store2d) (param $0 i32) (param $1 i32) (param $2 i32) (param $3 f64)
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
 (func $fn$0 (; 8 ;) (; has Stack IR ;) (type $3)
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
        (call $load2d
         (get_global $array_A)
         (i32.trunc_u/f64
          (get_global $X)
         )
         (i32.trunc_u/f64
          (get_global $Y)
         )
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
 (func $fn$1 (; 9 ;) (; has Stack IR ;) (type $3)
  (local $0 f64)
  (local $1 f64)
  (local $2 f64)
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
      (set_local $2
       (f64.add
        (f64.add
         (f64.add
          (f64.add
           (f64.add
            (f64.add
             (f64.add
              (call $load2d
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
              )
              (call $load2d
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
              )
             )
             (call $load2d
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
             )
            )
            (call $load2d
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
            )
           )
           (call $load2d
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
           )
          )
          (call $load2d
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
          )
         )
         (call $load2d
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
         )
        )
        (call $load2d
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
        )
       )
      )
      (call $store2d
       (get_global $array_B)
       (i32.trunc_u/f64
        (get_global $X)
       )
       (i32.trunc_u/f64
        (get_global $Y)
       )
       (call $load2d
        (get_global $array_L)
        (i32.trunc_u/f64
         (call $load2d
          (get_global $array_A)
          (i32.trunc_u/f64
           (get_global $X)
          )
          (i32.trunc_u/f64
           (get_global $Y)
          )
         )
        )
        (i32.trunc_u/f64
         (get_local $2)
        )
       )
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
 (func $fn$2 (; 10 ;) (; has Stack IR ;) (type $3)
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
      (call $store2d
       (get_global $array_A)
       (i32.trunc_u/f64
        (get_global $X)
       )
       (i32.trunc_u/f64
        (get_global $Y)
       )
       (call $load2d
        (get_global $array_B)
        (i32.trunc_u/f64
         (get_global $X)
        )
        (i32.trunc_u/f64
         (get_global $Y)
        )
       )
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
 (func $main (; 11 ;) (; has Stack IR ;) (type $3)
  (local $0 f64)
  (set_global $data_end
   (i32.const 96)
  )
  (i32.store
   (i32.const 104)
   (i32.const 112)
  )
  (set_global $array_A
   (call $alloc2d
    (i32.const 20)
    (i32.const 20)
   )
  )
  (set_global $array_L
   (call $alloc2d
    (i32.const 10)
    (i32.const 10)
   )
  )
  (set_global $array_B
   (call $alloc2d
    (i32.const 30)
    (i32.const 30)
   )
  )
  (set_local $0
   (call $read)
  )
  (set_global $M
   (call $read)
  )
  (call $store2d
   (get_global $array_L)
   (i32.const 0)
   (i32.const 3)
   (call $read)
  )
  (call $store2d
   (get_global $array_L)
   (i32.const 1)
   (i32.const 3)
   (call $read)
  )
  (call $store2d
   (get_global $array_L)
   (i32.const 1)
   (i32.const 2)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 3)
   (i32.const 4)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 3)
   (i32.const 5)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 3)
   (i32.const 6)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 6)
   (i32.const 5)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 6)
   (i32.const 6)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 7)
   (i32.const 5)
   (call $read)
  )
  (call $store2d
   (get_global $array_A)
   (i32.const 7)
   (i32.const 6)
   (call $read)
  )
  (set_global $I
   (f64.const 0)
  )
  (call $fn$0)
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
