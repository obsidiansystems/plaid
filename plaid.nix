{ mkDerivation, aeson, base, casing, containers, data-default-class
, req, scientific, stdenv, template-haskell, text, time, wreq
}:
mkDerivation {
  pname = "plaid";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base casing containers data-default-class req scientific
    template-haskell text time wreq
  ];
  description = "API client for plaid.com";
  license = stdenv.lib.licenses.bsd3;
}
