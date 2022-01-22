import ../../haskell.nix {
  name = "aoc-2015-hs";
  token = (builtins.readFile ./tokenfile);
}
