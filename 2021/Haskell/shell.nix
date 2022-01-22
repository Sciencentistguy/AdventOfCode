import ../../haskell.nix {
  name = "aoc-2021-hs";
  token = (builtins.readFile ./tokenfile);
}
