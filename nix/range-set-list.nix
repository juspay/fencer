{ mkDerivation, base, containers, deepseq, hashable, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "range-set-list";
  version = "0.1.3";
  sha256 = "e51b393d2c09e3c2b0c21523389a48ce8e6090413abdfff1c623815c76cc96df";
  revision = "1";
  editedCabalFile = "00ddj7if8lcrqf5c882m4slm15sdwcghz7d2fz222c7jcw1ahvdr";
  libraryHaskellDepends = [ base containers deepseq hashable ];
  testHaskellDepends = [
    base containers deepseq hashable tasty tasty-quickcheck
  ];
  homepage = "https://github.com/phadej/range-set-list#readme";
  description = "Memory efficient sets with ranges of elements";
  license = stdenv.lib.licenses.mit;
}
