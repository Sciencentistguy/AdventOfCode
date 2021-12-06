#TODO: shut up the `trace` outputs whenever haskell.nix is invoked

let
  sources = {
    haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") { };
  };
  haskellNix = import sources.haskellNix { };
  # Pin nixpkgs to the haskell.nix unstable pin
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  project = import ./default.nix;
in
project.shellFor {
  name = "aoc-2020-hs";
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    #stack = "latest";
    ormolu = "0.3.1.0";
  };

  withHoogle = false;
  exactDeps = true;

  nativeBuildInputs = [
    pkgs.stack
    pkgs.hpack
  ];

  TOKEN = (builtins.readFile ./tokenfile);
}
