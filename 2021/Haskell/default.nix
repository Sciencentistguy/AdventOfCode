let
  sources = {
    haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") { };
  };
  haskellNix = import sources.haskellNix { };
  # Pin nixpkgs to the haskell.nix unstable pin
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.stackProject {
  name = "aoc-2021-hs";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "aoc-2021-hs";
    src = ./.;
  };
}
