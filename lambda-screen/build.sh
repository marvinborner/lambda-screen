#!/bin/env sh

. ~/.ghc-wasm/env
wasm32-wasi-cabal build
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$(wasm32-wasi-cabal list-bin exe:lambda-screen)" -o build/wasm_jsffi.js
cp "$(wasm32-wasi-cabal list-bin exe:lambda-screen)" build/lambda_screen.wasm
