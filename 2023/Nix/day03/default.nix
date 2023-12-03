with import ../common; rec {
  # input :: string
  input = builtins.readFile ~/.aoc/2023/day03.txt;
  testInput = ''
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
  '';

  lines = lib.lists.init (lib.strings.splitString "\n" input);

  schematic = {
    numbers = lib.lists.flatten (
      lib.lists.imap0 (y: line: let
        enumerated = lib.lists.imap0 (x: c: {inherit x c;}) (lib.strings.stringToCharacters line);
        acced =
          builtins.foldl' (
            acc @ {
              positions,
              values,
              in_number,
            }: {
              x,
              c,
            }:
              if in_number
              then
                if isDigit c
                then
                  acc
                  // {
                    values = let
                      x' = builtins.head values;
                      x's = lib.lists.tail values;
                    in
                      [(x' ++ [c])] ++ x's;
                  }
                else
                  acc
                  // {
                    positions = let
                      x' = builtins.head positions;
                      x's = lib.lists.tail positions;
                    in
                      [(x' // {end = x;})] ++ x's;
                    in_number = false;
                  }
              else if isDigit c
              then {
                positions =
                  [{start = x;}]
                  ++ positions;
                in_number = true;
                values = [[c]] ++ values;
              }
              else acc
          )
          {
            positions = [];
            values = [];
            in_number = false;
          }
          enumerated;
        # sorted :: [{start, end}]
        zipped =
          lib.lists.zipListsWith (value: {
            start,
            end ? builtins.length enumerated,
          }: {
            inherit value;
            x_start = start;
            x_end = end;
          })
          acced.values
          acced.positions;
        parsed = map (x: x // {value = lib.strings.toInt (lib.strings.concatStrings x.value);}) zipped;
        sorted = lib.lists.sort (a: b: a.x_start < b.x_start) parsed;
        yd = map (x: x // {inherit y;}) sorted;
      in
        yd)
      lines
    );

    symbols = builtins.filter (a: a != null) (lib.lists.flatten (lib.lists.imap0 (
        y: line:
          lib.lists.imap0 (
            x: c:
              if !(isDigit c) && c != "."
              then {
                inherit x y;
                symbol = c;
              }
              else null
          ) (lib.strings.stringToCharacters line)
      )
      lines));
  };

  part1 = sum (dropNulls
    (map (
        number: let
          x_start = number.x_start - 1;
          x_end = number.x_end;
          y_start = number.y - 1;
          y_end = number.y + 1;
          aa =
            lib.lists.findFirst
            (symbol: y_start <= symbol.y && symbol.y <= y_end && x_start <= symbol.x && symbol.x <= x_end)
            null
            schematic.symbols;
        in
          if aa == null
          then null
          else number.value
      )
      schematic.numbers));
  part2 = sum (dropNulls (map (
      {
        symbol,
        x,
        y,
      }:
        if symbol != "*"
        then null
        else let
          numbers = dropNulls (map (
              number: let
                x_start = number.x_start - 1;
                x_end = number.x_end;
                y_start = number.y - 1;
                y_end = number.y + 1;
              in
                if y_start <= y && y <= y_end && x_start <= x && x <= x_end
                then number.value
                else null
            )
            schematic.numbers);
        in
          if builtins.length numbers == 2
          then product numbers
          else null
    )
    schematic.symbols));
}
