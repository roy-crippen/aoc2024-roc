module [solution_day_20]

import Bool exposing [false, true]
import Util exposing [Solution]
import Structures.Grid as Gr exposing [Grid, Pos]
import "../data/day_20.txt" as input_str : Str

State : { done : Bool, path : Dict Pos I32, pos : Pos }

parse : Str -> Grid U8
parse = |s|
    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.msg_unwrap "should be at least 1 row" |> List.len
    data = List.join ls
    { data, rows, cols }

find_single_path : Grid U8, Pos -> State
find_single_path = |g, start_pos|
    go : State -> State
    go = |st|
        ch = Gr.get_unsafe(g, st.pos)
        if ch == 'E' then
            st
        else
            (r, c) = st.pos
            ls =
                [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
                |> List.keep_if(|(row, col)| row > -1 and col > -1 and row < rows and col < cols)
            next_st = List.walk_until ls st |acc_st, (nr, nc)|
                v = Gr.get_unsafe(g, (nr, nc))
                if v == '#' or Dict.contains(acc_st.path, (nr, nc)) then
                    Continue acc_st
                else if v == 'E' then
                    dist = Dict.get(acc_st.path, acc_st.pos) |> Util.unwrap
                    path = Dict.insert(acc_st.path, (nr, nc), dist + 1)
                    Break { done: true, path, pos: (nr, nc) }
                else
                    dist = Dict.get(acc_st.path, acc_st.pos) |> Util.unwrap
                    path = Dict.insert(acc_st.path, (nr, nc), dist + 1)
                    Continue { done: false, path, pos: (nr, nc) }
            if next_st.done then next_st else go(next_st)

    rows = g.rows |> Num.to_i32
    cols = g.cols |> Num.to_i32
    inital_st = { done: false, path: Dict.single(start_pos, 0), pos: start_pos }
    final_st = go(inital_st)
    final_st

# find_candidates : Pos, I32, Dict Pos I32, I32, I32 -> List Pos
# find_candidates = |(r, c), radius, path, rows, cols|
#    xs =
#        List.range({ start: At 0, end: At radius })
#        |> List.walk [] |acc, dr|
#            dc = radius - dr
#            ls =
#                [(r + dr, c + dc), (r + dr, c - dc), (r - dr, c + dc), (r - dr, c - dc)]
#                |> List.keep_if |(row, col)|
#                    Dict.contains(path, (row, col)) and row > -1 and col > -1 and row < rows and col < cols
#            List.concat(acc, ls)
#    dbg xs
#    xs |> Set.from_list |> Set.to_list

find_candidates : Pos, I32, Dict Pos I32, I32, I32 -> List Pos
find_candidates = |(r, c), radius, path, rows, cols|
    is_inside = |row, col| row >= 0 and row < rows and col >= 0 and col < cols

    List.range { start: At (-radius), end: At radius }
    |> List.walk [] |acc, dr|
        abs_dr = Num.abs dr
        dc = radius - abs_dr
        if dc == 0 then
            row = r + dr
            col = c
            if is_inside(row, col) and Dict.contains(path, (row, col)) and !List.contains(acc, (row, col)) then
                List.append acc (row, col)
            else
                acc
        else
            List.walk [1, -1] acc |acc1, dc_sign|
                row = r + dr
                col = c + dc_sign * dc
                if is_inside(row, col) and Dict.contains(path, (row, col)) and !List.contains(acc1, (row, col)) then
                    List.append acc1 (row, col)
                else
                    acc1

# find_candidates : Pos, I32, Dict Pos I32, I32, I32 -> List Pos
# find_candidates = |(r, c), radius, path, rows, cols|
#    xs =
#        List.range({ start: At 0, end: At radius })
#        |> List.walk [] |acc, dr|
#            dc = radius - dr
#            ls =
#                [(r + dr, c + dc), (r + dr, c - dc), (r - dr, c + dc), (r - dr, c - dc)]
#                |> List.keep_if |(row, col)|
#                    Dict.contains(path, (row, col)) and row > -1 and col > -1 and row < rows and col < cols
#            List.concat(acc, ls)
#    xs |> Set.from_list |> Set.to_list

solution_day_20 : Solution
solution_day_20 = {
    day: 20,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 1422

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    get_savings : List Pos, Pos, I32 -> U64
    get_savings = |candidates, pivot_pos, offset|
        pivot_dist = Dict.get(st.path, pivot_pos) |> Util.unwrap
        candidates
        |> List.map |p|
            candidate_dist = Dict.get(st.path, p) |> Util.unwrap
            candidate_dist - pivot_dist - offset
        |> List.count_if(|d| d >= limit)

    g = parse(in_str)
    rows = g.rows |> Num.to_i32
    cols = g.cols |> Num.to_i32
    start_pos = Gr.find_positions(g, |c| c == 'S') |> List.first |> Util.unwrap

    st = find_single_path(g, start_pos)
    limit = 100i32
    # limit = 20i32

    dbg (Dict.len(st.path))
    res = List.walk Dict.keys(st.path) 0 |acc, p|
        cs = find_candidates(p, 2, st.path, rows, cols)
        save = get_savings(cs, p, 2)
        (acc + save)

    dbg res
    Ok res

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    ###############
    #...#...#.....#
    #.#.#.#.#.###.#
    #S#...#.#.#...#
    #######.#.#.###
    #######.#.#...#
    #######.#.###.#
    ###..E#...#...#
    ###.#######.###
    #...###...#...#
    #.#####.#.###.#
    #.#...#.#.#...#
    #.#.#.#.#.#.###
    #...#...#...###
    ###############
    """

# tests

# expect part1 example_str == Ok 0
# expect part1 example_str |> dbg == Ok 5
# expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

expect
    g = parse(example_str)
    start_pos = Gr.find_positions(g, |c| c == 'S') |> List.first |> Util.unwrap
    st = find_single_path(g, start_pos)
    #                   |pos, radius, path, rows, cols|
    cs = find_candidates((7, 7), 2, st.path, 15, 15)
    dbg cs
    List.len(cs) == 4
