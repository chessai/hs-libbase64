# hs-libbase64-bindings

Haskell bindings to the C [libbase64](https://github.com/aklomp/libbase64) SIMD
base64 encoding/decoding library.

This library is intended to only be a wrapper. It provides nothing in the way of
a "nice" API or abstractions to make working with it "easy". Instead, it is
meant to be a building block for other libraries to take up that more
opinionated work.
