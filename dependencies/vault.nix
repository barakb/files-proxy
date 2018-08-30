{ mkDerivation, base, containers, hashable, semigroups, stdenv
, unordered-containers
}:
mkDerivation {
  pname = "vault";
  version = "0.3.1.2";
  sha256 = "9e00e52ec0b054cfb9b1e44d8ce2eefb499cc1dcd4bcdd0d434b370d635e551c";
  libraryHaskellDepends = [
    base containers hashable semigroups unordered-containers
  ];
  doCheck = false;
  homepage = "https://github.com/HeinrichApfelmus/vault";
  description = "a persistent store for values of arbitrary types";
  license = stdenv.lib.licenses.bsd3;
}
