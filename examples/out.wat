(module
  (global $sp (mut i32) (i32.const 0))
  (memory $memory 1)
  (func $main (result i32)  (local $1 i32) (local $3 i32)
    get_local $1
    call $0
    get_local $3
    call $1
    get_local $1
    return
  )
  (func $1 (param $3 i32) (local $2 i32)
    i32.const 3
    set_local $2
    i32.const 100
    set_local $3
  )
  (func $0 (param $1 i32) (local $0 i32)
    i32.const 2
    set_local $0
    get_local $0
    set_local $1
  )
  (export "main" (func $main))
  (export "memory" (memory $memory))
)