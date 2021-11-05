with import <nixpkgs> { };

let self = callPackage ./default.nix { };
in
mkShell {
  nativeBuildInputs = [
    self
  ];
}
