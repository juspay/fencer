{ mkDerivation, aeson, aeson-qq, base, base-prelude, bytestring
, containers, deepseq, directory, filepath, focus, fsnotify
, grpc-haskell, hashable, list-t, monad-loops, named, proto3-suite
, proto3-wire, stdenv, stm, stm-containers, tasty, tasty-discover
, tasty-hunit, text, time, tinylog, transformers
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "fencer";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-prelude bytestring containers deepseq directory
    filepath focus fsnotify grpc-haskell hashable list-t monad-loops
    named proto3-suite proto3-wire stm stm-containers text time tinylog
    transformers unordered-containers vector yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson aeson-qq base base-prelude tasty tasty-discover tasty-hunit
    text unordered-containers vector
  ];
  testToolDepends = [ tasty-discover ];
  homepage = "http://github.com/juspay/fencer";
  description = "Port of lyft/ratelimit";
  license = stdenv.lib.licenses.bsd3;
}
