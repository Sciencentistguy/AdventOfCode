{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = {
        self',
        pkgs,
        ...
      }: {
        haskellProjects.default = {
          packages.aoc.root = ./.; # This value is detected based on .cabal files
          overrides = self: super: {};
          devShell = {
            enable = true; # Enabled by default
            tools = hp: {
              inherit (hp) ormolu;
              # fourmolu = hp.ormolu;
              ghcid = null;
            };
            hlsCheck.enable = true;
          };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.aoc;
      };
    };
}
