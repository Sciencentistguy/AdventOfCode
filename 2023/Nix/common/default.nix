let
  lock = builtins.fromJSON (builtins.readFile ../../../flake.lock);
  rev = lock.nodes.nixpkgs.locked.rev;
  nixpkgs = fetchTarball {
    url = "https://api.github.com/repos/NixOS/nixpkgs/tarball/${rev}";
    sha256 = lock.nodes.nixpkgs.locked.narHash;
  };
in rec {
  pkgs = import nixpkgs {};
  inherit (pkgs) lib;
}
