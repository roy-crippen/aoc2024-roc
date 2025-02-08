module [solution_day_02]

import Bool exposing [false, true]
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
    |> List.count_if is_safe
    |> Ok

expect part1 example_str == Ok 2
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> parse
    |> Result.map_err |_e| "failed to parse input string"
    |> try
    |> List.count_if is_safe_part_2
    |> Ok

expect part2 example_str == Ok 4
expect
    got = part2 input_str
    dbg got
    dbg expected_part2
    got == Ok expected_part2

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

is_safe_detailed : List I16 -> (Bool, I16)
is_safe_detailed = |ys|
    # dbg ys
    when ys is
        [] -> (false, 0)
        [v] -> (true, v)
        [first, second, ..] ->
            test_if_safe =
                asc = first < second

                ys
                |> List.drop_first 1
                |> List.walk_until
                    (false, first)
                    |(_failed, prev_y), y|
                        gap_ok = Num.abs_diff prev_y y < 4
                        if
                            (gap_ok and asc and y > prev_y)
                            or
                            (gap_ok and !asc and y < prev_y)
                        then
                            Continue (true, y)
                        else
                            Break (false, y)
            test_if_safe

is_safe : List I16 -> Bool
is_safe = |ys| (is_safe_detailed ys).0

expect is_safe_detailed [7, 6, 4, 2, 1] == (true, 1)
expect is_safe_detailed [1, 3, 6, 7, 9] == (true, 9)

is_safe_part_2 : List I16 -> Bool
is_safe_part_2 = |ys|
    if is_safe ys then
        true
    else
        ys
        |> List.map_with_index |_, i| is_safe(List.drop_at ys i)
        |> List.walk(false, |acc, b| acc or b)

expect is_safe_part_2 [1, 3, 2, 4, 5]
expect is_safe_part_2 [8, 6, 4, 4, 1]
expect is_safe_part_2 [4, 5, 3, 2, 1]
expect is_safe_part_2 [6, 6, 4, 3, 1]
expect is_safe_part_2 [1, 1, 2, 4, 5]
expect is_safe_part_2 [1, 2, 3, 4, 5, 5]

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
