module [unwrap, blue, green, orange, red, yellow, Solution, k_combos_without_reps]

unwrap : [Err *, Ok a], Str -> a
unwrap = |result, message|
    when result is
        Ok(x) -> x
        Err(_) -> crash(message)

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
green = |str| "\u(001b)[38;5;46m${str}\u(001b)[0m"

red : Str -> Str
red = |str| "\u(001b)[0;31m${str}\u(001b)[0m"

yellow : Str -> Str
yellow = |str| "\u(001b)[38;5;226m${str}\u(001b)[0m"

orange : Str -> Str
orange = |str| "\u(001b)[38;5;208m${str}\u(001b)[0m"

k_combos_without_reps : List a, U64 -> List (List a)
k_combos_without_reps = |xs, k|
    if k == 0 or k > List.len xs then
        []
    else if k == 1 then
        List.map xs |x| [x]
    else
        List.walk_with_index xs [] |acc, elem, index|
            rest = List.drop_first xs (index + 1)
            sub_combos = k_combos_without_reps rest (k - 1)
            combos_with_elem = List.map sub_combos |sub| List.prepend sub elem
            List.concat acc combos_with_elem

#
expect k_combos_without_reps([1, 2, 3], 2) == [[1, 2], [1, 3], [2, 3]]
expect k_combos_without_reps([1, 2, 3, 4], 3) == [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
