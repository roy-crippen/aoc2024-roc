module [solution_day_20]

import Bool exposing [false, true]
import Util exposing [Solution]
import Structures.Grid as Gr exposing [Grid, Pos]
import "../data/day_20.txt" as input_str : Str

# port of https://github.com/roycrippen4/aoc/blob/master/aoc2024/aoc_ocaml/lib/day20.ml

State : { done : Bool, path : Dict Pos I32, pos : Pos }

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

cheats : Pos, I32, Dict Pos I32, List (I32, I32, I32), I32, I32 -> I32
cheats = |(r, c), radius, path, dia, rows, cols|
    is_inside = |row, col| row >= 0 and row < rows and col >= 0 and col < cols
    center_steps = Dict.get(path, (r, c)) |> Util.unwrap
    List.walk dia 0 |acc, (dr, dc, dist)|
        (r1, c1) = (r + dr, c + dc)
        if dist <= radius and is_inside(r1, c1) then
            when Dict.get(path, (r1, c1)) is
                Ok s -> if s > center_steps and s - center_steps - dist >= 100 then acc + 1 else acc
                _ -> acc
        else
            acc

find_single_path : Grid U8, Pos -> Dict Pos I32
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
    final_st.path

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
    start_pos = Gr.find_positions(g, |c| c == 'S') |> List.first |> Util.unwrap
    path = find_single_path(g, start_pos)
    dia = diamond(radius)
    List.walk(Dict.keys(path), 0, |acc, p| acc + cheats(p, radius, path, dia, rows, cols)) |> Num.to_u64

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

