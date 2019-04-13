#!/usr/bin/env bash

w2w $1 out.wat
wat2wasm out.wat -o out.wasm
node --expose-wasm runner.js
