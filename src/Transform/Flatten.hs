
-- Removes blocks containing local variables, transforming local variable
-- declarations into variable assgniments. This assumes all variables have
-- unique names.
--
-- Also produces a mapping from procedure names to the variables they capture.
-- This is used to decide what arguments functions have in outputted WebAssembly.

module Transform.Flatten where
