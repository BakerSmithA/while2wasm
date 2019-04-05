
-- Annotates variables in AST with whether they are local or foreign at
-- to different scopes. Used to tell whether a variable should be local or
-- param in WebAssembly.
--
-- Also produces a set of variables which are 'dirty', i.e. modified in
-- two different scopes. Used to tell whether a variable should be stored
-- as a pointer to memory on the stack, or a simple value in outputted
-- WebAssembly.

module Transform.Capture where
