let
  # Pinned on 2021-11-04 at 01:48 UTC, to nixpkgs-unstable
  nixpkgs_url = "https://github.com/NixOS/nixpkgs/archive/550dab224a26ec25e20e82c0c8bfc764e01b772e.tar.gz";
  # aliases in shellHooks don't work with zsh-nix-shell. See https://github.com/chisui/zsh-nix-shell/issues/6
  zdotdir_url = "https://gist.githubusercontent.com/chisui/bba90fccc930f614743dc259fbadae6d/raw/4108222addc1d646c1b0a6d12130083e2219ad28/zdotdir.nix";
  shell_hooks = ''
    alias stack="stack --nix"
  '';
in
{ pkgs ? import (fetchTarball nixpkgs_url) { }
, zdotdir ? import (builtins.fetchurl { url = zdotdir_url; }) { inherit pkgs; }
}: pkgs.mkShell rec {

  name = "aoc-2020-hs";

  nativeBuildInputs = [
    pkgs.stack
    pkgs.ormolu
    pkgs.haskell-language-server
    pkgs.hlint
  ];

  buildInputs = [
    pkgs.zlib
  ];

  shellHooks = shell_hooks;

  # For zsh
  shellHook = zdotdir {
    zshenv = shell_hooks;
  };

  # Needed for stack to find zlib
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
  TOKEN = (builtins.readFile ./tokenfile);
}
