{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.haskell = pkgs.mkShell rec {
        name = "aoc-haskell";
        nativeBuildInputs = with pkgs; [
          stack
          ormolu
          haskell-language-server
          hlint
          zlib
        ];

        # Needed for stack to find zlib
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeBuildInputs;
      };
      devShells.rust = pkgs.mkShell {
        name = "aoc-rust";
        nativeBuildInputs = with pkgs; [
          rustc
          cargo
          clippy
          rustfmt
          cargo-edit
          rustPlatform.bindgenHook
          pkg-config
          openssl
        ];
      };
    });
}
