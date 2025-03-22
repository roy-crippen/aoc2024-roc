module [zip, unzip, window_by_2, unwrap, blue, green, orange, red, yellow, Solution, k_combos_without_reps]

zip : List a, List b -> List (a, b)
zip = |a, b| List.map2(a, b, |x, y| (x, y))

expect zip [1, 2, 3] [4, 5, 6] == [(1, 4), (2, 5), (3, 6)]

unzip : List (a, b) -> (List a, List b)
unzip = |xs| unzip_loop xs [] []

unzip_loop : List (a, b), List a, List b -> (List a, List b)
unzip_loop = |xs, one, other|
    when xs is
        [] -> (one, other)
        [(first_one, first_other), .. as rest] ->
            unzip_loop rest (List.append one first_one) (List.append other first_other)

expect unzip [(1, 4), (2, 5), (3, 6)] == ([1, 2, 3], [4, 5, 6])
expect unzip [(1, 'a'), (2, 'b'), (3, 'c')] == ([1, 2, 3], ['a', 'b', 'c'])

window_by_2 : List a -> List (a, a)
window_by_2 = |xs|
    cs = List.drop_last xs 1
    ds = List.drop_first xs 1
    zip cs ds

expect window_by_2 [1, 2, 3, 4] == [(1, 2), (2, 3), (3, 4)]

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
