{ compiler ? "ghc822" } :
let
#  compiler = "ghc843";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

            project0 = haskellPackagesNew.callPackage ./default.nix { };

            servant = haskellPackagesNew.callPackage ./dependencies/servant.nix { };

            servant-server = haskellPackagesNew.callPackage ./dependencies/servant-server.nix { };
           };
          };
        };
      };
    };
  };


  pkgs = import <nixpkgs> { inherit config; };

in
  { project0 =  pkgs.haskell.packages.${compiler}.project0;  
  }
