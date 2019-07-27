{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, binary, bytestring, cereal, containers
, contravariant, deepseq, doctest, fetchgit, filepath, foldl
, hashable, haskell-src, insert-ordered-containers, lens, mtl
, neat-interpolation, optparse-applicative, optparse-generic
, parsec, parsers, pretty, pretty-show, proto3-wire, QuickCheck
, quickcheck-instances, range-set-list, safe, semigroups, stdenv
, swagger2, system-filepath, tasty, tasty-hunit, tasty-quickcheck
, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite.git";
    sha256 = "0j9pnq3rvrvclrcjm25k47pvxr348amz5jlc1fg5zi5953xh7ivs";
    rev = "3859acd327296e45025587c4f552b7112ee0164b";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring binary
    bytestring cereal containers contravariant deepseq filepath foldl
    hashable haskell-src insert-ordered-containers lens mtl
    neat-interpolation parsec parsers pretty pretty-show proto3-wire
    QuickCheck quickcheck-instances safe semigroups swagger2
    system-filepath text transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers mtl optparse-applicative optparse-generic
    proto3-wire range-set-list system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq doctest mtl pretty-show proto3-wire QuickCheck
    semigroups swagger2 tasty tasty-hunit tasty-quickcheck text
    transformers turtle vector
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
