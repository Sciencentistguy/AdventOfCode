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
        haskellProjects.default = {
          projectFlakeName = "aoc-haskell";
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`, e.g. from Hackage
          # packages = {
          # aeson.source = "1.5.0.0"; # Hackage version
          # };

          # # my-haskell-package development shell configuration
          # devShell = {
          # hlsCheck.enable = false;
          # };

          # # What should haskell-flake add to flake outputs?
          # autoWire = ["packages" "apps" "checks"]; # Wire all but the devShell
        };

        devShells = {
          haskell = pkgs.mkShell {
            name = "my-haskell-package custom development shell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            nativeBuildInputs = with pkgs; [
              # other development tools.
            ];
          };
          rust = pkgs.mkShell {
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
