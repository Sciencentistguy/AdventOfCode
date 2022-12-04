{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    flake-utils,
    fenix,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      toolchain = fenix.packages.${system}.toolchainOf {
        date = "2022-11-28";
        sha256 = "sha256-bWDPQLhuWCAxyzaAYywJmPktYh81TOSV3+cQDgj3xVE=";
      };

      craneLib = (crane.mkLib pkgs).overrideToolchain (toolchain.withComponents ["rustc" "cargo"]);

      aoc-2022 = craneLib.buildPackage {
        src = craneLib.cleanCargoSource ./.;

        buildInputs =
          []
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.libiconv
          ];
      };
    in {
      checks = {
        inherit aoc-2022;
      };

      packages.default = aoc-2022;

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.checks;

        nativeBuildInputs = with pkgs; [
          toolchain.toolchain
          toolchain.rust-src
          cargo-flamegraph
        ];
      };
    });
}
