(module
  (global $sp (mut i32) (i32.const 0))
  (memory $memory 1)
  (func $main (result i32)  (local $0 i32)
    i32.const 1
    i32.const 1
    i32.eq
    if
      i32.const 1
      set_local $0
    else
      i32.const 2
      set_local $0
    end
    get_local $0
    return
  )
  (export "main" (func $main))
  (export "memory" (memory $memory))
)