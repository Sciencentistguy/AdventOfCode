import ../../haskell-nix {
  name = "aoc-2020-hs";
  token = (builtins.readFile ./tokenfile);
}
