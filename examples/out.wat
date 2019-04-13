(module
  (global $sp (mut i32) (i32.const 0))
  (memory $memory 1)
  (func $main (result i32)  (local $0 i32)
    get_local $0
    call $0
    get_local $0
    return
  )
  (func $0 (param $0 i32) 
    i32.const 1
    set_local $0
  )
  (export "main" (func $main))
  (export "memory" (memory $memory))
)