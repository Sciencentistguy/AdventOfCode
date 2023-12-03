with import ../common; rec {
  # input :: string
  input = builtins.readFile ~/.aoc/2023/day02.txt;

  lines = lib.lists.init (lib.strings.splitString "\n" input);
  lines' =
    lib.lists.imap1 (i: l: {
      idx = i;
      toP = let
        idx = lib.lists.findFirstIndex (x: x == ":") null l;
      in
        lib.strings.concatStrings (lib.lists.drop (idx + 2) l);
    })
    (map lib.strings.stringToCharacters lines);

  # parsed :: [{idx :: int, red :: int, green :: int, blue :: int}]
  parsed =
    map (
      {
        idx,
        toP,
      }: let
        x = lib.lists.flatten (map (lib.strings.splitString ",") (lib.strings.splitString ";" toP));
        x' = map (lib.strings.removePrefix " ") x; # remove leading whitespace
        pairs = map (lib.strings.splitString " ") x'; # split into of pairs ["num", "colour"]
        attrsets =
          # parse it into (name, colour) attrsets
          map (l: {
            num = lib.strings.toInt (builtins.head l);
            colour = builtins.elemAt l 1;
          })
          pairs;
      in
        (builtins.foldl' ({
            red,
            green,
            blue,
          }: {
            colour,
            num,
          }:
            if colour == "red"
            then {
              red = lib.trivial.max red num;
              inherit green blue;
            }
            else if colour == "green"
            then {
              green = lib.trivial.max green num;
              inherit red blue;
            }
            else if colour == "blue"
            then {
              blue = lib.trivial.max blue num;
              inherit red green;
            }
            else throw ("parsing error: invalid colour: " + colour))
          {
            red = 0;
            green = 0;
            blue = 0;
          }
          attrsets)
        // {
          inherit idx;
        }
    )
    lines';

  part1 = let
    RED_COUNT = 12;
    GREEN_COUNT = 13;
    BLUE_COUNT = 14;
    valid_games =
      builtins.filter
      ({
        red,
        green,
        blue,
        ...
      }:
        red <= RED_COUNT && green <= GREEN_COUNT && blue <= BLUE_COUNT)
      parsed;
  in
    sum (map ({idx, ...}: idx) valid_games);

  part2 = sum (map ({
    red,
    green,
    blue,
    ...
  }:
    red * green * blue)
  parsed);
}
