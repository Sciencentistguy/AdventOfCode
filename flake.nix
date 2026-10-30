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
        libclangPath = "${pkgs.libclang.lib}/lib";
      in {
        haskellProjects.aoc = {
          projectFlakeName = "aoc-haskell";
          basePackages = pkgs.haskell.packages.ghc9122;
        };

        devShells = rec {
          default = pkgs.mkShell {
            name = "aoc";
            LIBCLANG_PATH = libclangPath;
            LD_LIBRARY_PATH = lib.makeLibraryPath [pkgs.z3];
            inputsFrom = [
              haskell
              rust
            ];
          };
          haskell = pkgs.mkShell {
            name = "aoc-haskell";
            inputsFrom = [
              config.haskellProjects.aoc.outputs.devShell
            ];
          };
          rust = pkgs.mkShell {
            name = "aoc-rust";
            LIBCLANG_PATH = libclangPath;
            buildInputs = [pkgs.z3.dev];
            nativeBuildInputs = with pkgs;
              [
                fenix.complete.toolchain
                pkg-config
                openssl
                cargo-criterion
                cargo-flamegraph
                (cargo-show-asm.overrideAttrs (old: {cargoBuildFlags = old.cargoBuildFlags or [] ++ ["--features=disasm"];}))
                hyperfine
                libclang
                z3.dev
              ]
              ++ lib.optionals (pkgs.stdenv.isDarwin) [
                iconv
              ]
              ++ lib.optionals (pkgs.stdenv.isLinux) [
                perf
              ];
            LD_LIBRARY_PATH = lib.makeLibraryPath [pkgs.z3];
          };
          nix = pkgs.mkShell {
            name = "aoc-nix";
            nativeBuildInputs = with pkgs; [just];
          };
        };
      };
    };
}
