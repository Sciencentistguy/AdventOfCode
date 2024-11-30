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
        haskellProjects.default = {projectFlakeName = "aoc-haskell";};

        devShells = {
          haskell = pkgs.mkShell {
            name = "aoc-haskell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            # nativeBuildInputs = with pkgs; [
                # other development tools.
            # ];
          };
          rust = pkgs.mkShell {
            name = "aoc-rust";
            nativeBuildInputs = with pkgs;
              [
                (
                  fenix.toolchainOf {
                    channel = "nightly";
                    date = "2024-11-28";
                    sha256 = "sha256-lmQQppk1opfsDa+37lYNHvOwC5CXgIInS7pAnLoMSKM=";
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
