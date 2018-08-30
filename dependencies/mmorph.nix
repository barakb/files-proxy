{ mkDerivation, base, mtl, stdenv, transformers
, transformers-compat
}:
mkDerivation {
  pname = "mmorph";
  version = "1.1.2";
  sha256 = "c90afd7996c94be2b9a5796a7b94918d198c53b0c1d7a3eaf2982293560c5fbe";
  libraryHaskellDepends = [
    base mtl transformers transformers-compat
  ];
  doCheck = false;
  description = "Monad morphisms";
  license = stdenv.lib.licenses.bsd3;
}
