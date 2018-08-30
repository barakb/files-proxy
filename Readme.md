## Workflow

*  cabal2nix --no-check cabal://servant-0.11 > servant.nix
*  cabal2nix --no-check cabal://servant-server-0.11 > servant-server.nix
*  cabal2nix --no-check cabal://aeson-1.4.0.0 > aeson.nix
*  cabal2nix --no-check cabal://base-compat-0.9.3 > base-compat.nix
*  cabal2nix --no-check cabal://http-api-data-0.3.8.1 > http-api-data.nix
*  cabal2nix --no-check cabal://mmorph-1.1.2 > mmorph.nix
*  cabal2nix --no-check cabal://text-1.2.3.0 > text.nix
*  cabal2nix --no-check cabal://vault-0.3.1.2 > vault.nix
   

0. Update cabal dependencies
1. cabal2nix . > default.nix
2. nix-shell 
