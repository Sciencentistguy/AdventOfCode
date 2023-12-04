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

  # isDigit :: char -> bool
  isDigit = x: (lib.lists.findSingle (a: a == x) "!!" "!!" ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]) != "!!";

  # sum :: [Int] -> Int
  sum = builtins.foldl' (a: b: a + b) 0;

  # product :: [Int] -> Int
  product = builtins.foldl' (a: b: a * b) 1;

  # dropNulls :: [a?] -> [a]
  dropNulls = builtins.filter (x: x != null);

  # contains :: a -> [a] -> bool
  contains = as: a: (lib.lists.findSingle (x: x == a) null a as) != null;

# pow :: int -> int -> int
pow = n : i :
          if i == 1 then n
          else if i == 0 then 1
          else n * pow n (i - 1);
}
