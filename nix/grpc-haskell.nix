{ mkDerivation, fetchFromGitHub
, async, base, bytestring, clock, containers
, criterion, grpc-haskell-core, managed, pipes, proto3-suite
, proto3-wire, QuickCheck, random, safe, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time, transformers, turtle, unix
}:
mkDerivation {
  pname = "grpc-haskell";
  version = "0.0.0.0";
  src = fetchFromGitHub {
    owner = "awakesecurity";
    repo = "gRPC-haskell";
    rev = "c83eacd1f30f20b0661fb651ad4234faf19c8160";
    sha256 = "1l95rjrnsiy23gqmjxirqc4yn38nl94whq25dd6lgj44pgikd72n";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring grpc-haskell-core managed proto3-suite
    proto3-wire
  ];
  testHaskellDepends = [
    async base bytestring clock containers managed pipes proto3-suite
    QuickCheck safe tasty tasty-hunit tasty-quickcheck text time
    transformers turtle unix
  ];
  benchmarkHaskellDepends = [
    async base bytestring criterion proto3-suite random
  ];
  homepage = "https://github.com/awakenetworks/gRPC-haskell";
  description = "Haskell implementation of gRPC layered on shared C library";
  license = stdenv.lib.licenses.asl20;
}
