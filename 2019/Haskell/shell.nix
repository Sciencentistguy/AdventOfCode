import ../../haskell-nix {
  name = "aoc-2019-hs";
  token = (builtins.readFile ./tokenfile);
}
