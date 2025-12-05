module [solution_day_20]

import Bool exposing [false, true]
import Util exposing [Solution]
import Structures.Grid as Gr exposing [Grid, Pos]
import "../data/day_20.txt" as input_str : Str

# port of https://github.com/roycrippen4/aoc/blob/master/aoc2024/aoc_ocaml/lib/day20.ml

State : { done : Bool, path : List I32, pos : Pos }

parse : Str -> Grid U8
parse = |s|
    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.msg_unwrap "should be at least 1 row" |> List.len
    data = List.join ls
    { data, rows, cols }

diamond : I32 -> List (I32, I32, I32)
diamond = |n|
    row_n : I32, I32, List (I32, I32, I32) -> List (I32, I32, I32)
    row_n = |dc, l, acc|
        if dc > l then
            acc
        else
            dr = l - dc
            xs1 = [(dr, dc, l)]
            xs2 = if dc != 0 then List.append(xs1, (dr, -dc, l)) else xs1
            xs3 = if dr != 0 then List.append(xs2, (-dr, dc, l)) else xs2
            xs4 = if dc != 0 and dr != 0 then List.append(xs3, (-dr, -dc, l)) else xs3
            row_n(dc + 1, l, List.concat(acc, xs4))

    layers : I32, List (I32, I32, I32) -> List (I32, I32, I32)
    layers = |l, acc| if l == 0 then acc else layers(l - 1, row_n(0, l, acc))

    layers n []

cheats : Pos, I32, List I32, List (I32, I32, I32), I32, I32 -> I32
cheats = |(r, c), radius, path, dia, rows, cols|
    cols_u64 = Num.to_u64 cols
    is_inside = |row, col| row >= 0 and row < rows and col >= 0 and col < cols
    center_idx = pos_to_idx (r, c) cols_u64
    center_steps = List.get path center_idx |> Util.unwrap
    List.walk dia 0 |acc, (dr, dc, dist)|
        (r1, c1) = (r + dr, c + dc)
        if dist <= radius and is_inside(r1, c1) then
            s = List.get path (pos_to_idx (r1, c1) cols_u64) |> Util.unwrap
            if s >= 0 and s > center_steps and s - center_steps - dist >= 100 then acc + 1 else acc
        else
            acc

pos_to_idx : Pos, U64 -> U64
pos_to_idx = |(r, c), cols| (Num.to_u64 r) * cols + (Num.to_u64 c)

find_single_path : Grid U8, Pos -> List I32
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
                n_idx = pos_to_idx (nr, nc) cols_u64
                visited = List.get acc_st.path n_idx |> Util.unwrap
                if v == '#' or visited >= 0 then
                    Continue acc_st
                else if v == 'E' then
                    dist = List.get acc_st.path (pos_to_idx acc_st.pos cols_u64) |> Util.unwrap
                    path = List.set acc_st.path n_idx (dist + 1)
                    Break { done: true, path, pos: (nr, nc) }
                else
                    dist = List.get acc_st.path (pos_to_idx acc_st.pos cols_u64) |> Util.unwrap
                    path = List.set acc_st.path n_idx (dist + 1)
                    Continue { done: false, path, pos: (nr, nc) }
            if next_st.done then next_st else go(next_st)

    rows = g.rows |> Num.to_i32
    cols = g.cols |> Num.to_i32
    cols_u64 = Num.to_u64 cols
    total_cells = (Num.to_u64 rows) * cols_u64
    initial_path = List.repeat -1i32 total_cells |> List.set (pos_to_idx start_pos cols_u64) 0
    inital_st = { done: false, path: initial_path, pos: start_pos }
    final_st = go(inital_st)
    final_st.path

all_pos : I32, I32 -> List Pos
all_pos = |rs_i32, cs_i32|
    rs = Num.to_i64 rs_i32
    cs = Num.to_i64 cs_i32
    r_list = List.range { start: At 0i64, end: Before rs } |> List.map Num.to_i32
    List.join_map r_list |r|
        c_list = List.range { start: At 0i64, end: Before cs } |> List.map Num.to_i32
        List.map c_list |c| (r, c)

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
expected_part2 = 1009299

solve : Str, I32 -> U64
solve = |in_str, radius|
    g = parse(in_str)
    rows = g.rows |> Num.to_i32
    cols = g.cols |> Num.to_i32
    cols_u64 = Num.to_u64 cols
    start_pos = Gr.find_positions(g, |c| c == 'S') |> List.first |> Util.unwrap
    path = find_single_path(g, start_pos)
    dia = diamond(radius)
    positions = all_pos rows cols
    List.walk positions 0u64 |acc, p|
        idx = pos_to_idx p cols_u64
        steps = List.get path idx |> Util.unwrap
        if steps >= 0 then
            acc + (Num.to_u64 (cheats p radius path dia rows cols))
        else
            acc

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str| Ok solve(in_str, 2)

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str| Ok solve(in_str, 20)

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

expect part1 example_str == Ok 0
# expect part1 input_str == Ok expected_part1
expect
    ds = diamond 20
    List.len(ds) == 840
