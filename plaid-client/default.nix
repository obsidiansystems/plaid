{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
let
  haskellPackages = nixpkgs.haskell.packages.${compiler};
in
  haskellPackages.callCabal2nix "plaid-client" ./. {
    plaid-core = import ../plaid-core {};
  }
