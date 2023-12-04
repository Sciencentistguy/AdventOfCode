with import ../common; rec {
  # input :: string
  input = builtins.readFile ~/.aoc/2023/day04.txt;
  testInput = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";

  lines = lib.lists.init (lib.strings.splitString "\n" input);

  cards =
    map (
      line: let
        line' = lib.strings.stringToCharacters line;
        idx = lib.lists.findFirstIndex (x: x == ":") null line';
        line'' = lib.lists.drop (idx + 2) line';
        line''' = lib.strings.concatStrings line'';
        sides = lib.strings.splitString "|" line''';

        winning_side = builtins.head sides;
        scratch_side = builtins.elemAt sides 1;

        pSide = side:
          dropNulls (map (
              s: let
                s' = lib.strings.removePrefix " " (lib.strings.removeSuffix " " s);
                attempt = builtins.tryEval (lib.strings.toInt s');
              in
                if attempt.success
                then attempt.value
                else null
            )
            (lib.strings.splitString " " side));
      in {
        winning_numbers = pSide winning_side;
        scratch_numbers = pSide scratch_side;
      }
    )
    lines;

  num_matches = game: builtins.length (builtins.filter (x: builtins.elem x game.winning_numbers) game.scratch_numbers);
  value = game: let
    x = num_matches game;
  in
    if x == 0
    then 0
    else pow 2 (x - 1);

  part1 = sum (map value cards);
  part2 = let
    # type CardState = { an attrset where keys are card indexes, and values are counts }

    # sum_card_states CardState -> CardState
    sum_card_states = a: b: lib.attrsets.zipAttrsWith (name: vals: builtins.foldl' builtins.add 0 vals) [a b];

    # card_state_of :: int -> int -> int -> CardState
    card_state_of = from: to: init:
      builtins.listToAttrs (map (x: {
          name = toString x;
          value = init;
        })
        (lib.lists.range from to));

    initial_state = card_state_of 0 ((builtins.length cards) - 1) 1;
    enumerated = lib.lists.imap0 (i: card: {inherit i card;}) cards;
  in
    sum (lib.attrsets.mapAttrsToList (_: v: v)
      (builtins.foldl' (acc: {
        i,
        card,
      }: let
        value = num_matches card;
        new_cards = card_state_of (i + 1) (i + value) acc."${toString i}";
      in
        sum_card_states acc new_cards)
      initial_state
      enumerated));
}
