module [solution_day_14]

import Bool exposing [not]
import Util exposing [Solution]
import "../data/day_14.txt" as input_str : Str

Pos : I64
Vel : I64
Pt : (Pos, Vel)

parse : Str -> (List Pt, List Pt)
parse = |s| Str.split_on s "\n" |> List.map parse_values |> Util.unzip

parse_value : Str -> Pt
parse_value = |s|
    when Str.split_on s "," is
        [s1, s2] ->
            p = Str.split_on s1 "=" |> List.last |> Util.unwrap |> Str.to_i64 |> Util.unwrap
            v = Str.to_i64 s2 |> Util.unwrap
            (p, v)

        _ -> crash "invalid input"

parse_values : Str -> (Pt, Pt)
parse_values = |s|
    ls = Str.split_on s " "
    (x, y) = List.first ls |> Util.unwrap |> parse_value
    (vx, vy) = List.last ls |> Util.unwrap |> parse_value
    ((x, vx), (y, vy))

process : List Pt, I64, I64 -> List I64
process = |pairs, len, count| List.map pairs (|(x, vx)| Util.modulus (x + vx * count) len)

score_part1 : List (I64, I64), I64, I64 -> U64
score_part1 = |xy_pairs, mid_x, mid_y|
    get_score : (I64, I64, I64, I64), (I64, I64) -> (I64, I64, I64, I64)
    get_score = |(q1, q2, q3, q4), (x, y)|
        when (x <= mid_x, y <= mid_y) is
            (b1, b2) if b1 and b2 -> (q1 + 1, q2, q3, q4)
            (b1, b2) if b1 and not b2 -> (q1, q2, q3 + 1, q4)
            (b1, b2) if not b1 and b2 -> (q1, q2 + 1, q3, q4)
            (b1, b2) if not b1 and not b2 -> (q1, q2, q3, q4 + 1)
            _ -> crash "aaa"

    (v1, v2, v3, v4) = List.walk xy_pairs (0, 0, 0, 0) get_score
    v1 * v2 * v3 * v4 |> Num.to_u64

tree_found : List I64, List I64, I64 -> (Bool, Bool)
tree_found = |xs, ys, rows|
    y_freq_base = List.repeat 0 (Num.to_u64 rows)
    y_freq =
        List.walk ys y_freq_base (|acc, y| List.update acc (Num.to_u64 y) (|a| a + 1))
        |> List.map_with_index (|v, i| (i, v))
        |> List.keep_if |(_i, v)| v > 30

    x_freq =
        y_freq
        |> List.map |(r, _cnt)|
            row = Num.to_i64 r
            List.map_with_index ys |v, i| (v, i)
            |> List.keep_if |(v, _i)| v == row
            |> List.map (|(_v, i)| List.get xs i |> Util.unwrap)
            |> List.sort_asc

    (List.map x_freq max_consecutive |> List.any |cnt| cnt > 25, List.len x_freq > 0)

max_consecutive : List I64 -> U64
max_consecutive = |xs|
    ts = List.sort_asc xs
    head = List.first ts |> Util.unwrap
    tail = List.drop_first ts 1
    initial_state = { acc: 0, max_acc: 0, prev_value: head }

    final_state = List.walk tail initial_state |st, v|
        if
            st.prev_value + 1 == v
        then
            { st & acc: st.acc + 1, prev_value: v }
        else
            new_max = Num.max st.max_acc st.acc
            { acc: 0, max_acc: new_max, prev_value: v }

    Num.max(final_state.acc, final_state.max_acc) + 1

solution_day_14 : Solution
solution_day_14 = {
    day: 14,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 220971520

expected_part2 : U64
expected_part2 = 6355

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (rows, cols) = (103, 101)
    (mid_x, mid_y) = (cols // 2, rows // 2)
    (xs, ys) = parse in_str
    Util.zip (process xs cols 100) (process ys rows 100)
    |> List.keep_if (|(x, y)| x != mid_x and y != mid_y)
    |> score_part1 mid_x mid_y
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    go : I64 -> I64
    go = |cnt|
        (xs, ys) = (process xvs cols cnt, process yvs rows cnt)
        (tree, increase_step) = tree_found xs ys rows
        new_step = if increase_step then rows else 1
        if tree then cnt else go (cnt + new_step)

    (rows, cols) = (103, 101) # actual problem
    (xvs, yvs) = parse in_str
    go 0 |> Num.to_u64 |> Ok

example_str : Str
example_str =
    """
    p=0,4 v=3,-3
    p=6,3 v=-1,-3
    p=10,3 v=-1,2
    p=2,0 v=2,-1
    p=0,0 v=1,3
    p=3,0 v=-2,-2
    p=7,6 v=-1,-3
    p=3,0 v=-1,-2
    p=9,3 v=2,3
    p=7,3 v=-1,2
    p=2,4 v=2,-3
    p=9,5 v=-3,-3
    """

# tests

expect part1 input_str == Ok expected_part1
expect part2 input_str == Ok expected_part2
expect
    parse example_str
    == (
        [(0, 3), (6, -1), (10, -1), (2, 2), (0, 1), (3, -2), (7, -1), (3, -1), (9, 2), (7, -1), (2, 2), (9, -3)],
        [(4, -3), (3, -3), (3, 2), (0, -1), (0, 3), (0, -2), (6, -3), (0, -2), (3, 3), (3, 2), (4, -3), (5, -3)],
    )

# import Grid exposing [Grid]
# view : Str, I64, I64, I64 -> Grid U8
# view = |s, rows, cols, count|
#     (xvs, yvs) = parse s
#     ps = Util.zip (process yvs rows count) (process xvs cols count) |> List.map |(r, c)| (Num.to_i32 r, Num.to_i32 c)
#     g = Grid.make(Num.to_u64 rows, Num.to_u64 cols, '.')
#     g1 = List.walk ps g (|grid, p| Grid.set grid p '@')
#     _ = Grid.show_char g1 |> dbg
#     g

# expect
#     g = view input_str 103 101 6355
#     g.rows == 103

# expect
#     g = view input_str 103 101 72
#     g.rows == 103
