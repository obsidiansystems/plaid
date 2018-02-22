{ mkDerivation, base, containers, req, stdenv, text }:
mkDerivation {
  pname = "plaid";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers req text ];
  description = "API client for plaid.com";
  license = stdenv.lib.licenses.bsd3;
}
