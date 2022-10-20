let
  isDarwin = (import <nixpkgs> {}).stdenv.isDarwin;
  nixpkgs_url = "https://github.com/NixOS/nixpkgs/archive/d0f9857448e77df50d1e0b518ba0e835b797532a.tar.gz";
in
  {
    name,
    token,
  }: (
    {
      pkgs ? import (fetchTarball nixpkgs_url) {},
      #, zdotdir ? import (builtins.fetchurl { url = zdotdir_url; }) { inherit pkgs; }
    }: let
      # Haskell language server by default builds with support for ghc 8.8.4, which does not support aarch64-darwin.
      # TODO: remove this once it is no longer needed
      haskell-language-server =
        if isDarwin
        then pkgs.haskell-language-server.override {supportedGhcVersions = ["8107"];}
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
