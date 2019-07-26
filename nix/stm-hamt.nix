{ mkDerivation, async, base, criterion, deferred-folds, focus, free
, hashable, list-t, mwc-random, mwc-random-monad, primitive
, primitive-extras, QuickCheck, quickcheck-instances, rebase
, rerebase, stdenv, tasty, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "stm-hamt";
  # We intentionally use 1.2.0.3 instead of 1.2.0.4 because 1.2.0.4 would
  # cause us to recompile the world due to the old version of 'primitive'
  # shipped by nixpkgs.
  version = "1.2.0.3";
  sha256 = "2389eae079a7c80013c1ac028ee4750cdfec1f192f521561f3078f80c4c4d72f";
  libraryHaskellDepends = [
    base deferred-folds focus hashable list-t primitive
    primitive-extras transformers
  ];
  testHaskellDepends = [
    deferred-folds focus QuickCheck quickcheck-instances rerebase tasty
    tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    async criterion focus free list-t mwc-random mwc-random-monad
    rebase
  ];
  homepage = "https://github.com/nikita-volkov/stm-hamt";
  description = "STM-specialised Hash Array Mapped Trie";
  license = stdenv.lib.licenses.mit;
}
