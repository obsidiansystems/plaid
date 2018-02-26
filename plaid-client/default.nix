{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
, withHoogle ? false }:
let
  haskellPackages = nixpkgs.haskell.packages.${compiler}.override {
    overrides = _: su: {
      ghcWithPackages = if withHoogle then su.ghcWithHoogle else su.ghcWithPackages;
    };
  };
in
  haskellPackages.callCabal2nix "plaid-client" ./. {
    plaid-core = import ../plaid-core {};
  }
