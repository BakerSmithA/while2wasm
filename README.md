# while2wasm
While to WASM compiler using scoped effect handlers combined with Datatypes a la Carte and Extensible effects. The thesis this work is based on can be found in [DSLs.pdf](https://github.com/BakerSmithA/while2wasm/blob/master/doc/DSLs.pdf).

# Install

Clone the repo, and then run the following commands. The compiled output can be found in `dist/build/w2w`.

```
> cd while2wasm
> cabal sandbox init
> cabal install
> cabal build
```

To compile a While program simply run the `w2w` executable with a path to a While program and the path output WASM to. 
