with import ../common; rec {
  # input :: string
  input = builtins.readFile ~/.aoc/2023/day01.txt;

  # lines :: [[char]] (note: char is just a 1-character string)
  lines = lib.lists.init (lib.strings.splitString "\n" input);

  # isDigit :: char -> bool

  # solve :: [string] -> Int
  solve = splitted: let
    lines = map lib.strings.stringToCharacters splitted;
    isDigit = x: (lib.lists.findSingle (a: a == x) "!!" "!!" ["1" "2" "3" "4" "5" "6" "7" "8" "9"]) != "!!";

    # first_digits :: [char]
    first_digits = map (lib.lists.findFirst isDigit "!!") lines;
    # last_digits :: [char]
    last_digits = map (lib.lists.findFirst isDigit "!!") (map lib.lists.reverseList lines);
    # calibration_values :: [string]
    calibration_values = lib.lists.zipListsWith (a: b: a + b) first_digits last_digits;
    result = builtins.foldl' builtins.add 0 (map lib.strings.toInt calibration_values);
  in
    result;

  go = builtins.replaceStrings ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] ["o1e" "t2o" "t3ree" "f4ur" "f5ve" "s6x" "s7ven" "e8ght" "n9ne"];

  # lib.fix save me ... lib.fix ... save me lib.fix
  reallyGo = x: go (go (go x));

  part1 = solve lines;
  part2 = solve (map reallyGo lines);
}
