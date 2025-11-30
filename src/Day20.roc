module [solution_day_20]

import Bool exposing [false, true]
import Util exposing [Solution]
import Structures.Grid as Gr exposing [Grid, Pos]
import "../data/day_20.txt" as input_str : Str

State : { done : Bool, path : Dict Pos U64, pos : Pos }

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
expected_part1 = 42

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    g = parse(in_str)
    start_pos = Gr.find_positions(g, |c| c == 'S') |> List.first |> Util.unwrap
    st = find_single_path(g, start_pos)
    path = st.path
    end_pos = st.pos
    base_dist = Dict.get(path, end_pos) |> Util.unwrap

    dbg (start_pos, end_pos, base_dist)

    Ok 42

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

expect part1 example_str == Ok 42
# expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2
