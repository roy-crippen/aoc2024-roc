module [solution_day_02]

import Util exposing [Solution]
import "../data/day_02.txt" as input_str : Str

solution_day_02 : Solution
solution_day_02 = {
    day: 2,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 390

expected_part2 : U64
expected_part2 = 439

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    in_str
    |> parse
    |> Result.map_err |_e| "failed to parse input string"
    |> try
    |> count_safe
    |> Ok

expect part1 example_str == Ok 2
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

# expect part2 example_str == Ok 4
# expect part2 input_str == Ok expected_part2

count_safe : List (List I16) -> U64
count_safe = |xss|
    xss
    |> List.map |xs| if is_safe xs then 1 else 0
    |> List.sum

parse : Str -> Result (List (List I16)) [InvalidNumStr]
parse = |in_str|
    in_str
    |> Str.trim
    |> Str.split_on "\n"
    |> List.map_try |line|
        Str.split_on line " "
        |> List.map_try |s| Str.to_i16 s
        |> try
        |> Ok

is_safe_desc : List I16 -> (Bool, I16)
is_safe_desc = |ys|
    when List.first ys is
        Ok first ->
            List.walk_with_index_until
                ys
                (Bool.true, first)
                |(_failed, prev), y, idx|
                    if
                        idx == 0 or (y < prev and prev - y < 4)
                    then
                        Continue (Bool.true, y)
                    else
                        Break (Bool.false, y)

        _ -> (Bool.true, 0)

is_safe_asc : List I16 -> (Bool, I16)
is_safe_asc = |ys|
    when List.first ys is
        Ok first ->
            List.walk_with_index_until
                ys
                (Bool.true, first)
                |(_failed, prev), y, idx|
                    if
                        idx == 0 or (y > prev and y - prev < 4)
                    then
                        Continue (Bool.true, y)
                    else
                        Break (Bool.false, y)

        _ -> (Bool.true, 0)

is_safe : List I16 -> Bool
is_safe = |ys|
    asc = is_safe_asc ys
    # dbg asc
    desc = is_safe_desc ys
    # dbg desc
    asc.0 or desc.0

expect
    expected = Bool.true
    got = is_safe [7, 6, 4, 2, 1]
    dbg got
    expected == got

expect
    expected = Bool.true
    got = is_safe [1, 3, 6, 7, 9]
    dbg got
    expected == got

expect
    expected = Ok 6
    got = parse example_str |> dbg |> Result.map_ok |xs| xs |> List.len
    expected == got

example_str : Str
example_str =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """
