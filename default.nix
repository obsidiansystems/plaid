let
  reflex-platform = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner  = "reflex-frp";
    repo   = "reflex-platform";
    rev    = "318eb23267471f0cfbb0c8be3efa26430f8627d4";
    sha256 = "0yn0lwa6z4jk09ng67m5wdfwah07a71zxbdswxm8qi7mgj0pim32";
  }) {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    req = pkgs.fetchFromGitHub {
      owner  = "mrkkrp";
      repo   = "req";
      rev    = "1.0.0";
      sha256 = "1zh5xa3nqx4r5wzv37h2kcqb2jgirgvx0m0ycwmmvnqps17jdnj7";
    };


    plaid-core = ./plaid-core;
    plaid-client = ./plaid-client;
    reflex-dom-plaid = ./reflex-dom-plaid;
    reflex-dom-plaid-demo = ./reflex-dom-plaid-demo;
  };

  shells = {
    ghc = ["plaid-core" "plaid-client" "reflex-dom-plaid" "reflex-dom-plaid-demo"];
    ghcjs = ["plaid-core" "reflex-dom-plaid" "reflex-dom-plaid-demo"];
  };
})
