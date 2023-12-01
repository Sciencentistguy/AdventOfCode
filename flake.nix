{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
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
      fenix = self.inputs.fenix.packages.${system};
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
        nativeBuildInputs = with pkgs;
          [
            (
              fenix.toolchainOf {
                channel = "nightly";
                date = "2023-11-29";
                sha256 = "sha256-NGdi7CZp3m6s4P4KMFoVfQmeKsWhLnioYoHcF66dBzk=";
              }
            )
            .toolchain
            pkg-config
            openssl
            cargo-criterion
          ]
          ++ lib.optionals (pkgs.stdenv.isDarwin) ([
              iconv
            ]
            ++ (with pkgs.darwin.apple_sdk.frameworks; [
              SystemConfiguration
            ]));
      };
    });
}
