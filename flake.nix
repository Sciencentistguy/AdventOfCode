{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    fenix = {
      url = "github:Sciencentistguy/fenix/fix-vscode-call";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
    haskell-flake = {
      url = "github:srid/haskell-flake";
    };
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-darwin"];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {
        self',
        system,
        lib,
        config,
        pkgs,
        ...
      }: let
        fenix = inputs.fenix.packages.${system};
      in {
        haskellProjects.aoc = {
          projectFlakeName = "aoc-haskell";
          basePackages = pkgs.haskell.packages.ghc9101;
          packages = {
            # the lastest release of hlint does not work with ghc 9.10.1 - pinning to a commit that does
            hlint.source = pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "hlint";
              rev = "ad9a1f8485eb34d40a59cc0bd1457d03fbffd50d";
              sha256 = "sha256-IqGhE9pAaK6CtVTPrsOpWvZMkvICng8pwv6KSGVAg9I=";
            };
          };
        };

        devShells = rec {
          default = pkgs.mkShell {
            name = "aoc";
            inputsFrom = [
              config.haskellProjects.aoc.outputs.devShell
            ];
            nativeBuildInputs = with pkgs;
              [
                fenix.complete.toolchain
                pkg-config
                openssl
                cargo-criterion
                cargo-flamegraph
                just
              ]
              ++ lib.optionals (pkgs.stdenv.isDarwin) ([
                  iconv
                ]
                ++ (with pkgs.darwin.apple_sdk.frameworks; [
                  SystemConfiguration
                ]));
          };
          haskell = pkgs.mkShell {
            name = "aoc-haskell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
          };
          rust = pkgs.mkShell {
            name = "aoc-rust";
            nativeBuildInputs = with pkgs;
              [
                fenix.complete.toolchain
                pkg-config
                openssl
                cargo-criterion
                cargo-flamegraph
              ]
              ++ lib.optionals (pkgs.stdenv.isDarwin) ([
                  iconv
                ]
                ++ (with pkgs.darwin.apple_sdk.frameworks; [
                  SystemConfiguration
                ]));
          };
          nix = pkgs.mkShell {
            name = "aoc-nix";
            nativeBuildInputs = with pkgs; [just];
          };
        };
      };
    };
}
