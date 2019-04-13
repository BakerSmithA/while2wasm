(module
  (global $sp (mut i32) (i32.const 0))
  (memory $memory 1)
  (func $main (result i32)  (local $0 i32) (local $1 i32)
    get_global $sp
    i32.const 8
    i32.add
    set_global $sp
    get_global $sp
    i32.const 0
    i32.sub
    i32.const 6
    i32.store offset=0
    get_global $sp
    i32.const 4
    i32.sub
    i32.const 1
    i32.store offset=0
    get_global $sp
    i32.const 0
    i32.sub
    get_global $sp
    i32.const 4
    i32.sub
    call $0
    get_global $sp
    i32.const 4
    i32.sub
    i32.load offset=0
    return
    get_global $sp
    i32.const 8
    i32.sub
    set_global $sp
  )
  (func $0 (param $0 i32) (param $1 i32) 
    get_global $sp
    i32.const 0
    i32.sub
    i32.load offset=0
    i32.const 1
    i32.eq
    i32.const 0
    i32.eq
    if
      get_global $sp
      i32.const 4
      i32.sub
      get_global $sp
      i32.const 4
      i32.sub
      i32.load offset=0
      get_global $sp
      i32.const 0
      i32.sub
      i32.load offset=0
      i32.mul
      i32.store offset=0
      get_global $sp
      i32.const 0
      i32.sub
      get_global $sp
      i32.const 0
      i32.sub
      i32.load offset=0
      i32.const 1
      i32.sub
      i32.store offset=0
      get_global $sp
      i32.const 0
      i32.sub
      get_global $sp
      i32.const 4
      i32.sub
      call $0
    else
      nop
    end
  )
  (export "main" (func $main))
  (export "memory" (memory $memory))
)