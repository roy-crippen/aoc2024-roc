module [red, green, blue, Solution]

Solution : {
    day : U64,
    input_str : Str,
    part1 : Str -> [Err Str, Ok U64],
    part2 : Str -> [Err Str, Ok U64],
    expected_part1 : U64,
    expected_part2 : U64,
}

blue : Str -> Str
blue = |str| "\u(001b)[0;34m${str}\u(001b)[0m"

green : Str -> Str
green = |str| "\u(001b)[0;32m${str}\u(001b)[0m"

red : Str -> Str
red = |str| "\u(001b)[0;31m${str}\u(001b)[0m"

