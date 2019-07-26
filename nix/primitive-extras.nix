{ mkDerivation, base, bytestring, cereal, deferred-folds, focus
, foldl, list-t, primitive, profunctors, QuickCheck
, quickcheck-instances, rerebase, stdenv, tasty, tasty-hunit
, tasty-quickcheck, vector
}:
mkDerivation {
  pname = "primitive-extras";
  version = "0.7.1.1";
  sha256 = "b57dd914bbe8f1ab6a4bb7d7eaa965d44dee6f0d4b9281bea0b5ebdcf07ecec1";
  revision = "1";
  editedCabalFile = "10z7fnz907s7ar15lk3kq62p11bbsksdb0nmg5y7ii0n97mqni96";
  libraryHaskellDepends = [
    base bytestring cereal deferred-folds focus foldl list-t primitive
    profunctors vector
  ];
  testHaskellDepends = [
    cereal deferred-folds focus primitive QuickCheck
    quickcheck-instances rerebase tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/metrix-ai/primitive-extras";
  description = "Extras for the \"primitive\" library";
  license = stdenv.lib.licenses.mit;
}
