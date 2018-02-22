{ mkDerivation, aeson, base, casing, containers, req, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "plaid";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base casing containers req template-haskell text
  ];
  description = "API client for plaid.com";
  license = stdenv.lib.licenses.bsd3;
}
