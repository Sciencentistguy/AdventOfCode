let
  isDarwin = (import <nixpkgs> { }).stdenv.isDarwin;
  # Pin nixpkgs to nixpkgs-unstable (2022-01-19)
  # Using https://github.com/Sciencentistguy/nixpkgs-fork/commit/7b25a24936921a94d2639868972e1bebd0bb672c for darwin to work around a ghc bug
  # See: https://github.com/NixOS/nixpkgs/pull/154046
  nixpkgs_url =
    if isDarwin then "https://github.com/Sciencentistguy/nixpkgs-fork/archive/7b25a24936921a94d2639868972e1bebd0bb672c.tar.gz"
    else "https://github.com/NixOS/nixpkgs/archive/e5a50e8f2995ff359a170d52cc40adbcfdd92ba4.tar.gz";
in
{ name, token }:
(
  { pkgs ? import (fetchTarball nixpkgs_url) { }
    #, zdotdir ? import (builtins.fetchurl { url = zdotdir_url; }) { inherit pkgs; }
  }:
  let
    # Haskell language server by default builds with support for ghc 8.8.4, which does not support aarch64-darwin.
    # TODO: remove this once it is no longer needed
    haskell-language-server =
      if isDarwin
      then pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; }
      else pkgs.haskell-language-server;
  in
  pkgs.mkShell rec {
    inherit name;

    nativeBuildInputs = [
      pkgs.stack
      pkgs.ormolu
      haskell-language-server
      pkgs.hlint
    ];

    buildInputs = [
      pkgs.zlib
    ];

    # Needed for stack to find zlib
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
    TOKEN = token;
  }
)
