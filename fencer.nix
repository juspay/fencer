{ mkDerivation, stdenv
, base, stm, vector, text, containers, bytestring, deepseq, proto3-wire, proto3-suite
, hashable, base-prelude, grpc-haskell, stm-containers, focus, named, monad-loops
, unordered-containers, time, aeson, yaml, directory, filepath, fsnotify, tinylog
, transformers, list-t
, tasty, tasty-hunit, tasty-discover
, configureFlags ? [], enableSharedExecutables ? true, enableSharedLibraries ? true
}:
mkDerivation {
  pname = "fencer";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  inherit enableSharedExecutables enableSharedLibraries configureFlags;
  libraryHaskellDepends = [
    base stm vector text containers bytestring deepseq proto3-wire proto3-suite
    hashable base-prelude grpc-haskell stm-containers focus named monad-loops
    unordered-containers time aeson yaml directory filepath fsnotify tinylog
    transformers list-t
  ];
  executableHaskellDepends = [
    base
  ];
  description = "Port of lyft/ratelimit";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
  testHaskellDepends = [
    aeson base tasty tasty-hunit tasty-discover text unordered-containers vector
  ];
}
