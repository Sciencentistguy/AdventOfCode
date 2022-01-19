let
  isDarwin = (import <nixpkgs> { }).stdenv.isDarwin;
  # Pinned on 2022-01-19 to nixpkgs-unstable
  # Using https://github.com/Sciencentistguy/nixpkgs-fork/commit/7b25a24936921a94d2639868972e1bebd0bb672c for darwin to work around ghc bug
  # See: https://github.com/NixOS/nixpkgs/pull/154046
  nixpkgs_url =
    if isDarwin then "https://github.com/Sciencentistguy/nixpkgs-fork/archive/7b25a24936921a94d2639868972e1bebd0bb672c.tar.gz"
    else "https://github.com/NixOS/nixpkgs/archive/e5a50e8f2995ff359a170d52cc40adbcfdd92ba4.tar.gz";
  # aliases in shellHooks don't work with zsh-nix-shell. See https://github.com/chisui/zsh-nix-shell/issues/6
  zdotdir_url = "https://gist.githubusercontent.com/chisui/bba90fccc930f614743dc259fbadae6d/raw/4108222addc1d646c1b0a6d12130083e2219ad28/zdotdir.nix";
  shell_hooks = ''
    alias stack="stack --nix"
  '';
in
{ name, token }:
(
  { pkgs ? import (fetchTarball nixpkgs_url) { }
  , zdotdir ? import (builtins.fetchurl { url = zdotdir_url; }) { inherit pkgs; }
  }: pkgs.mkShell rec {
    inherit name;

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
    TOKEN = token;
  }
)
