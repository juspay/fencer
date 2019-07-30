# Fencer

Fencer is a port of <https://github.com/lyft/ratelimit> into Haskell. The
key difference is that Fencer does not use Redis. The API, rule matching
logic, etc, are compatible with `lyft/ratelimit` as far as possible.

## Building

Instructions pending.

## Developing

Install [Nix](https://nixos.org/nix/). Then run:

```
$ nix-shell
$ cabal new-build
```

You can use [`nix-cabal`](https://github.com/monadfix/nix-cabal) as a
`cabal` replacement to get seamless nix-shell support in your editor/IDE.

### Regenerating API from Protobuf

`lib/Ratelimit/Proto.hs` has been generated from a proto3 file used in
`lyft/ratelimit`. This file, with minor modifications, is vendored in
`proto/rls.proto`. To regenerate the Haskell code from the proto3 file, use
the following command:

```
# compile-proto-file is weird and you can't get it to generate a module with
# a normal name, so we have to resort to sed shenanigans
(cd proto; compile-proto-file --proto rls.proto --out ../lib/Ratelimit/) \
  && mv lib/Ratelimit/Rls.hs lib/Ratelimit/Proto.hs \
  && sed -i 's/module Rls/module Ratelimit.Proto/' lib/Ratelimit/Proto.hs \
  && sed -i 's/Rls\./Ratelimit\.Proto\./g' lib/Ratelimit/Proto.hs
```
