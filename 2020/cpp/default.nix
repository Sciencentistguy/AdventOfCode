{ pkgs ? import <nixpkgs> { }
, cpkgs ? import (fetchTarball "https://github.com/sciencentistguy/nixpkgs/archive/e601d30ad4fe5e65bd49f52414b92dbef47aa87f.tar.gz") { inherit pkgs; }
, stdenv ? pkgs.gcc11Stdenv
}:

stdenv.mkDerivation {
  name = "aoc-2020-cpp";

  src = pkgs.lib.cleanSource ./.;

  nativeBuildInputs = with pkgs; [
    cmake
    ninja
    fmt
    range-v3
    glm

    cpkgs.ctre
    cpkgs.robin-hood-hashing
  ];

  hardeningDisable = pkgs.lib.optionals (stdenv.isAarch64 && stdenv.isDarwin) [ "stackprotector" ];
}
