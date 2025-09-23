module [solution_day_12]

import Util exposing [Solution]
import Grid exposing [Grid, Pos]
import "../data/day_12.txt" as input_str : Str

parse : Str -> (Grid U8, List (Pos, U8))
parse = |s|

    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.unwrap "should be at least 1 row" |> List.len
    data = List.join ls
    g = { data, rows, cols }

    # make position list
    rs = List.range { start: At 0, end: Before rows }
    cs = List.range { start: At 0, end: Before cols }
    xs = Util.cartesian_product rs cs |> List.map |(r, c)| (Num.to_i32 r, Num.to_i32 c)
    ps = Util.zip xs data
    (g, ps)

distinct_areas : Grid U8, List (Pos, U8) -> List (List Pos)
distinct_areas = |g, positions|

    # DFS to find one area starting at pos
    dfs : Pos, U8, Dict Pos Bool -> (List Pos, Dict Pos Bool)
    dfs = |start, ch, visited| dfs_loop [start] visited [] ch

    dfs_loop : List Pos, Dict Pos Bool, List Pos, U8 -> (List Pos, Dict Pos Bool)
    dfs_loop = |stack, visited, area, ch|
        when stack is
            [] -> (area, visited)
            [pos, .. as rest] ->
                if Dict.get visited pos |> Result.with_default Bool.false then
                    dfs_loop rest visited area ch
                else
                    neighbors =
                        Grid.neighbors4 pos
                        |> List.keep_if |p|
                            Grid.get g p == Ok ch and !(Dict.get visited p |> Result.with_default Bool.false)
                    new_visited = Dict.insert visited pos Bool.true
                    new_area = List.append area pos
                    dfs_loop (List.concat neighbors rest) new_visited new_area ch

    # main loop: process all positions
    loop : List (Pos, U8), Dict Pos Bool, List (List Pos) -> List (List Pos)
    loop = |pairs, visited, areas|
        when pairs is
            [] -> areas
            [(pos, ch), .. as rest] ->
                if Dict.contains visited pos then
                    loop rest visited areas
                else
                    (area, new_visited) = dfs pos ch visited
                    loop rest new_visited (List.prepend areas area)

    loop positions (Dict.with_capacity (g.cols * g.rows)) []

perimeter : Grid U8, List Pos -> U64
perimeter = |g, ps|
    like_neighbors : Pos -> List Pos
    like_neighbors = |pos|
        ch = Grid.get g pos
        Grid.neighbor_values4 g pos
        |> List.keep_if |(_neighbor_pos, neighbor_value)| neighbor_value == ch
        |> List.map |(neighbor_pos, _neighbor_value)| neighbor_pos

    (List.map ps |p| (4 - List.len (like_neighbors p))) |> List.sum

corner_counts : Grid U8, List Pos -> U64
corner_counts = |g, ps|
    corner_count : Pos -> U64
    corner_count = |p|
        (n, nw, w, sw, s, se, e, ne) = Grid.neighbor_values8_tup g p
        ch = Grid.get g p
        count =
            (if s == ch and se != ch and e == ch then 1 else 0)
            + (if s == ch and sw != ch and w == ch then 1 else 0)
            + (if n == ch and ne != ch and e == ch then 1 else 0)
            + (if n == ch and nw != ch and w == ch then 1 else 0)
            + (if s != ch and e != ch then 1 else 0)
            + (if s != ch and w != ch then 1 else 0)
            + (if n != ch and e != ch then 1 else 0)
            + (if n != ch and w != ch then 1 else 0)
        count

    List.map ps corner_count |> List.sum

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
expected_part2 = 830516

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    #     time_start = Utc.now!({})
    (g, d) = parse in_str
    #     time_end = Utc.now!({})
    #     duration_parse = (Num.to_frac Utc.delta_as_nanos(time_end, time_start)) / 1000000.0
    #     dbg duration_parse
    distinct_areas g d
    |> List.map |area| perimeter g area * List.len area
    |> List.sum
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (g, d) = parse in_str
    distinct_areas g d
    |> List.map |area| corner_counts g area * List.len area
    |> List.sum
    |> Ok

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

e1 =
    """
    AAAA
    BBCD
    BBCC
    EEEC
    AAAA
    """

e2 =
    """
    EEEEE
    EXXXX
    EEEEE
    EXXXX
    EEEEE
    """

e3 =
    """
    AAAAAA
    AAABBA
    AAABBA
    ABBAAA
    ABBAAA
    AAAAAA
    """

# tests

expect part1 e1 == Ok 180
expect part1 example_str == Ok 1930
expect part1 input_str == Ok expected_part1
expect part2 e2 == Ok 236
expect part2 e3 == Ok 368
expect part2 input_str == Ok expected_part2
