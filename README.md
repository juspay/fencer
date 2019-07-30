# Fencer

Fencer is a port of <https://github.com/lyft/ratelimit> into Haskell. The
key difference is that Fencer does not use Redis. The API, rule matching
logic, etc, are compatible with `lyft/ratelimit` as far as possible.

## Roadmap

Done:

* In-memory counting
* Rule matching
* `lyft/ratelimit`-compatible gRPC interface

Left to do:

* Garbage collect old counters
* Configuration loading and reloading
* API for setting configuration
* Doctests
* Tests
* Benchmarks

## Building

Instructions pending.

## Developing

Install [Nix](https://nixos.org/nix/). On macOS and Linux, this can be done
with:

```
$ curl https://nixos.org/nix/install | sh
```

Once you have Nix, enter the Nix shell and build the project with `cabal`:

```
$ nix-shell
$ cabal v2-build
```

You can use [`nix-cabal`](https://github.com/monadfix/nix-cabal) as a
`cabal` replacement to get seamless nix-shell support in your editor/IDE.

### Regenerating API from Protobuf

`lib/Fencer/Proto.hs` has been generated from a proto3 file used in
`lyft/ratelimit`. This file, with minor modifications, is vendored in
`proto/rls.proto`. To regenerate the Haskell code from the proto3 file, use
the following command:

```
# compile-proto-file is weird and you can't get it to generate a module with
# a normal name, so we have to resort to sed shenanigans
(cd proto; compile-proto-file --proto rls.proto --out ../lib/Fencer/) \
  && mv lib/Fencer/Rls.hs lib/Fencer/Proto.hs \
  && sed -i 's/module Rls/module Fencer.Proto/' lib/Fencer/Proto.hs \
  && sed -i 's/Rls\./Fencer\.Proto\./g' lib/Fencer/Proto.hs
```
