module [solution_day_10]

import Structures.Grid as Gr exposing [Grid, Pos]
import Util exposing [Solution]
import "../data/day_10.txt" as input_str : Str

solution_day_10 : Solution
solution_day_10 = {
    day: 10,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 667

expected_part2 : U64
expected_part2 = 1344

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    g = parse in_str
    Gr.find_positions g (|v| v == 0)
    |> List.walk 0 |acc, pos| acc + (scores g pos |> Util.unique |> List.len)
    |> Ok

expect part1 example_str == Ok 36
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    g = parse in_str
    Gr.find_positions g (|v| v == 0)
    |> List.walk 0 |acc, pos| acc + (scores g pos |> List.len)
    |> Ok

expect part2 example_str == Ok 81
expect part2 input_str == Ok expected_part2

next_pos : Grid U8, Pos -> [Err [NotFound, OutOfBounds], Ok Pos]
next_pos = |g, pos|
    _v = (Gr.get g pos)?
    Ok pos

scores : Grid U8, Pos -> List Pos
scores = |g, pos| scores_loop g pos []

scores_loop : Grid U8, Pos, List Pos -> List Pos
scores_loop = |g, pos, rs|
    pos_val = Gr.get g pos |> Util.msg_unwrap "Gr.get failed"
    if pos_val == 9 then
        List.append rs pos
    else
        Gr.apply4 g pos next_pos
        |> List.keep_if |res| Result.is_ok res
        |> List.map |res|
            when res is
                Ok p ->
                    new_v = Gr.get g p |> Util.msg_unwrap "Gr.get failed"
                    if new_v == pos_val + 1 then
                        scores g p
                    else
                        []

                _ -> crash "non reachable"
        |> List.join

parse : Str -> Grid U8
parse = |s|
    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.msg_unwrap "should be at least 1 row" |> List.len
    data = List.join ls |> List.map |v| v - 48
    { data, rows, cols }

example_str : Str
example_str =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """
