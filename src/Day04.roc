module [solution_day_04]

import Util
import Grid exposing [Grid, Dir, Pos]
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
    Grid.find_positions g |v| v == 'X'
    |> List.walk 0 |acc, pos| (xmas_count g pos) + acc
    |> Ok

expect part1 example_str == Ok 18
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    g = parse in_str
    Grid.find_positions g |v| v == 'A'
    |> List.keep_oks |pos| cross_xmas g pos
    |> List.sum
    |> Ok

expect part2 example_str == Ok 9
expect part2 input_str == Ok expected_part2
#
cross_xmas : Grid U8, Pos -> [Err [NotFound, OutOfBounds], Ok U64]
cross_xmas = |g, pos|
    # nw and se
    nw = (Grid.get g (Grid.north_west pos))?
    se = (Grid.get g (Grid.south_east pos))?
    nw_se_ok = (nw == 'M' and se == 'S') or (nw == 'S' and se == 'M')

    # ne and sw
    ne = (Grid.get g (Grid.north_east pos))?
    sw = (Grid.get g (Grid.south_west pos))?
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
    pos_m = Grid.move pos dir
    m = (Grid.get g pos_m)?

    pos_a = Grid.move pos_m dir
    a = (Grid.get g pos_a)?

    pos_s = Grid.move pos_a dir
    s = (Grid.get g pos_s)?

    when (m, a, s) is
        ('M', 'A', 'S') -> Ok 1
        _ -> Err NotFound

expect Ok 1 == check_xmas_in_dir (parse example_str) (0, 4) SE |> dbg

parse : Str -> Grid U8
parse = |s| Str.split_on s "\n" |> List.map Str.to_utf8

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
