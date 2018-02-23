{ mkDerivation, aeson, base, casing, containers, data-default-class
, req, stdenv, template-haskell, text, wreq
}:
mkDerivation {
  pname = "plaid";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base casing containers data-default-class req
    template-haskell text wreq
  ];
  description = "API client for plaid.com";
  license = stdenv.lib.licenses.bsd3;
}
