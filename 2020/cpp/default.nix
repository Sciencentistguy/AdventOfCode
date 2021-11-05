{ pkgs ? import <nixpkgs> { }
, cpkgs ? import (fetchTarball "https://github.com/sciencentistguy/nixpkgs/archive/e601d30ad4fe5e65bd49f52414b92dbef47aa87f.tar.gz") { inherit pkgs; }
}:
with pkgs; stdenv.mkDerivation {
  name = "aoc-2020-cpp";

  src = ./.;

  nativeBuildInputs = [
    cmake
    ninja
    fmt
    range-v3
    glm

    cpkgs.ctre
    cpkgs.robin-hood-hashing
  ];

}
