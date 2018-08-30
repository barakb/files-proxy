{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, deepseq, directory
, dlist, filepath, generic-deriving, ghc-prim, hashable
, hashable-time, integer-logarithms, QuickCheck
, quickcheck-instances, scientific, stdenv, tagged, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, time, time-locale-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.0.0";
  sha256 = "916a208dbb2d46f4dda2d7162a9960e42b7b70526be4af06cd34cba526865710";
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers deepseq dlist
    ghc-prim hashable scientific tagged template-haskell text
    th-abstraction time time-locale-compat unordered-containers
    uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers directory dlist filepath generic-deriving
    ghc-prim hashable hashable-time integer-logarithms QuickCheck
    quickcheck-instances scientific tagged tasty tasty-hunit
    tasty-quickcheck template-haskell text time time-locale-compat
    unordered-containers uuid-types vector
  ];
  doCheck = false;
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
