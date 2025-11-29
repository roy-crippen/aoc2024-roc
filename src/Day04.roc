module [solution_day_04]

import Util
import Structures.Grid as Gr exposing [Grid, Dir, Pos]
import "../data/day_04.txt" as input_str : Str

solution_day_04 : Util.Solution
solution_day_04 = {
    day: 04,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 2573

expected_part2 : U64
expected_part2 = 1850

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    g = parse in_str
    Gr.find_positions g |v| v == 'X'
    |> List.walk 0 |acc, pos| (xmas_count g pos) + acc
    |> Ok

expect part1 example_str == Ok 18
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    g = parse in_str
    Gr.find_positions g |v| v == 'A'
    |> List.keep_oks |pos| cross_xmas g pos
    |> List.sum
    |> Ok

expect part2 example_str == Ok 9
expect part2 input_str == Ok expected_part2

cross_xmas : Grid U8, Pos -> [Err [NotFound, OutOfBounds], Ok U64]
cross_xmas = |g, pos|
    # nw and se
    nw = (Gr.get g (Gr.north_west pos))?
    se = (Gr.get g (Gr.south_east pos))?
    nw_se_ok = (nw == 'M' and se == 'S') or (nw == 'S' and se == 'M')

    # ne and sw
    ne = (Gr.get g (Gr.north_east pos))?
    sw = (Gr.get g (Gr.south_west pos))?
    ne_sw_ok = (ne == 'M' and sw == 'S') or (ne == 'S' and sw == 'M')

    if nw_se_ok and ne_sw_ok then Ok 1 else Err NotFound

xmas_count : Grid U8, Pos -> U64
xmas_count = |g, pos|
    [
        check_xmas_in_dir g pos N,
        check_xmas_in_dir g pos NW,
        check_xmas_in_dir g pos W,
        check_xmas_in_dir g pos SW,
        check_xmas_in_dir g pos S,
        check_xmas_in_dir g pos SE,
        check_xmas_in_dir g pos E,
        check_xmas_in_dir g pos NE,
    ]
    |> List.keep_oks (|v| v)
    |> List.len

check_xmas_in_dir : Grid U8, Pos, Dir -> [Err [NotFound, OutOfBounds], Ok U8]
check_xmas_in_dir = |g, pos, dir|
    pos_m = Gr.move pos dir
    m = (Gr.get g pos_m)?

    pos_a = Gr.move pos_m dir
    a = (Gr.get g pos_a)?

    pos_s = Gr.move pos_a dir
    s = (Gr.get g pos_s)?

    when (m, a, s) is
        ('M', 'A', 'S') -> Ok 1
        _ -> Err NotFound

expect Ok 1 == check_xmas_in_dir (parse example_str) (0, 4) SE

parse : Str -> Grid U8
parse = |s|
    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.get ls 0 |> Util.msg_unwrap "should be at least 1 row" |> List.len
    data = List.join ls
    { data, rows, cols }

example_str : Str
example_str =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """
