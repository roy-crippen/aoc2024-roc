module [solution_day_01]

import Util exposing [Solution]
import "../data/day_01.txt" as input_str : Str

solution_day_01 : Solution
solution_day_01 = {
    day: 1,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 2086478

expected_part2 : U64
expected_part2 = 24941624

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    in_str
    |> parse
    |> Result.map_err |_e| "failed to parse input string"
    |> try
    |> unzip_lists
    |> sort_lists
    |> diffs
    |> List.sum
    |> Ok

expect part1 example_str == Ok 11
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> parse
    |> Result.map_err |_e| "failed to parse input string"
    |> try
    |> unzip_lists
    |> freq
    |> Ok

expect part2 example_str == Ok 31
expect part2 input_str == Ok expected_part2

parse : Str -> Result (List (U64, U64)) [InvalidInput, InvalidNumStr]
parse = |in_str|
    in_str
    |> Str.trim
    |> Str.split_on "\n"
    |> List.map_try |line|
        when Str.split_on line "   " is
            [s1, s2] ->
                x = Str.to_u64? s1
                y = Str.to_u64? s2
                Ok (x, y)

            _ -> Err InvalidInput
    |> try
    |> Ok

unzip_lists : List (U64, U64) -> (List U64, List U64)
unzip_lists = |vs| (List.map vs .0, List.map vs .1)

sort_lists : (List U64, List U64) -> (List U64, List U64)
sort_lists = |(xs, ys)| (List.sort_asc xs, List.sort_asc ys)

diffs : (List U64, List U64) -> List U64
diffs = |(xs, ys)| List.map2 xs ys Num.abs_diff

freq : (List U64, List U64) -> U64
freq = |(xs, ys)|
    xs
    |> List.map(|x| ys |> List.walk(0, |acc, y| if x == y then acc + x else acc))
    |> List.sum

expect
    expected = Ok ([(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)])
    got = parse example_str
    expected == got

example_str : Str
example_str =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """
