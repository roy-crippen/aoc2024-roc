module [solution_day_12]

import Util exposing [Solution]
import Grid exposing [Grid, Pos]
import "../data/day_12.txt" as input_str : Str
import pf.Utc

parse : Str -> (Grid U8, Dict U8 (List Pos))
parse = |s|

    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.unwrap "should be at least 1 row" |> List.len
    data = List.join ls
    g = { data, rows, cols }

    # make grouped dictionary
    rs = List.range { start: At 0, end: Before rows }
    cs = List.range { start: At 0, end: Before cols }
    xs = Util.cartesian_product rs cs |> List.map |(r, c)| (Num.to_i32 r, Num.to_i32 c)
    d = Util.zip data xs |> Util.group_by |> Util.unwrap "Util.group_by failed" |> Dict.from_list
    (g, d)

distinct_areas : Grid U8, Dict U8 (List Pos) -> List (List Pos)
distinct_areas = |g, char_dict|
    find_areas : List Pos -> List (List Pos)
    find_areas = |ps|
        go : Set Pos, List (List Pos) -> List (List Pos)
        go = |unvisited, areas|
            if Set.is_empty unvisited then
                areas
            else
                start = Set.to_list unvisited |> List.first |> Util.unwrap "List.first failed"
                (area, new_unvisited) = dfs start unvisited
                go new_unvisited (List.prepend areas area)

        dfs : Pos, Set Pos -> (List Pos, Set Pos)
        dfs = |start, unvisited|
            dfs1 : List Pos, Set Pos, List Pos -> (List Pos, Set Pos)
            dfs1 = |ps1, unvisited1, area|
                when ps1 is
                    [] -> (area, unvisited1)
                    [pos, .. as rest] ->
                        ch = Grid.get g pos
                        neighbors =
                            Grid.neighbors4 pos
                            |> List.keep_if |p| Set.contains unvisited1 p and Grid.contains g pos and Grid.get g p == ch
                        unvisited2 = List.walk neighbors unvisited1 |acc, p| Set.remove acc p
                        dfs1 (List.concat neighbors rest) unvisited2 (List.prepend area pos)
            dfs1 [start] (Set.remove unvisited start) []

        go (Set.from_list ps) []

    Dict.values char_dict |> List.map find_areas |> List.join

perimeter : Grid U8, List Pos -> U64
perimeter = |g, ps|
    like_neighbors : Pos -> List Pos
    like_neighbors = |pos|
        Grid.neighbors4 pos
        |> List.keep_if |neighbor|
            ch = Grid.get g pos
            Grid.contains g neighbor and Grid.get g neighbor == ch

    (List.map ps |p| (4 - List.len (like_neighbors p))) |> List.sum

solution_day_12 : Solution
solution_day_12 = {
    day: 12,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 1361494

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    time_start = Utc.now!({})
    (g, d) = parse in_str
    time_end = Utc.now!({})
    duration_parse = (Num.to_frac Utc.delta_as_nanos(time_end, time_start)) / 1000000.0
    dbg duration_parse

    # ls = Dict.values d |> List.map |xs| List.len xs
    # dbg (Dict.values d)
    # dbg (Dict.keys d)

    time_start1 = Utc.now!({})
    das = distinct_areas g d
    time_end1 = Utc.now!({})
    duration_distinct_areas = (Num.to_frac Utc.delta_as_nanos(time_end1, time_start1)) / 1000000.0
    dbg duration_distinct_areas

    time_start2 = Utc.now!({})
    rest =
        das
        |> List.map |area| perimeter g area * List.len area
        |> List.sum
        |> Ok
    time_end2 = Utc.now!({})
    duration_rest = (Num.to_frac Utc.delta_as_nanos(time_end2, time_start2)) / 1000000.0
    dbg duration_rest
    rest

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """

e1 : Str
e1 =
    """
    AAAA
    BBCD
    BBCC
    EEEC
    """

# tests

expect part1 e1 == Ok 140
expect part1 example_str == Ok 1930
expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2
