module [solution_day_08]

import Util exposing [Solution]
import "../data/day_08.txt" as input_str : Str

Board : { rows : I32, cols : I32, ants : List (U8, List (I32, I32)) }

solution_day_08 : Solution
solution_day_08 = {
    day: 08,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 396

expected_part2 : U64
expected_part2 = 1200

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    b = in_str |> parse
    List.walk b.ants Set.empty({}) |acc_set, vs|
        Set.union acc_set (make_antis vs b.rows b.cols)
    |> Set.len
    |> Ok

expect part1 example_str == Ok 14
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 34

# expect part2 example_str == Ok 34
# expect part2 input_str == Ok expected_part2

make_antis : (U8, List (I32, I32)), I32, I32 -> Set (I32, I32)
make_antis = |(_ch, xs), rows, cols|
    in_range = |i, j| i >= 0 and i < rows and j >= 0 and j < cols
    combos = Util.k_combos_without_reps xs 2
    anti_list = List.walk combos [] |acc0, pairs|
        when pairs is
            [(r1, c1), (r2, c2)] ->
                r3 = 2 * r1 - r2
                c3 = 2 * c1 - c2
                acc1 = if in_range r3 c3 then List.append acc0 (r3, c3) else acc0

                r4 = 2 * r2 - r1
                c4 = 2 * c2 - c1
                acc2 = if in_range r4 c4 then List.append acc1 (r4, c4) else acc1

                acc2

            _ -> crash("should not be here")

    Set.from_list anti_list

expect
    expected = [(0, 11), (3, 2)]
    got = (make_antis (48, [(1, 8), (2, 5)]) 12 12) |> Set.to_list
    expected == got

parse : Str -> Board
parse = |in_str|
    lss = Str.split_on in_str "\n" |> List.map Str.to_utf8
    rows = List.len lss |> Num.to_i32
    cols = List.get lss 0 |> Util.unwrap "should be at least 1 row" |> List.len |> Num.to_i32
    ys =
        lss
        |>
        List.map_with_index |ls, r|
            List.map_with_index ls |ch, c| (ch, (r |> Num.to_i32, c |> Num.to_i32))
            |> List.keep_if |(ch, _)| ch != '.'
        |> List.keep_if |vs| vs != []
        |> List.join

    ants =
        ys
        |> List.walk Dict.empty({}) |acc_dict, (ch, pos)|
            when Dict.get acc_dict ch is
                Ok zs0 ->
                    zs1 = List.append zs0 pos
                    Dict.insert acc_dict ch zs1

                Err _ ->
                    Dict.insert acc_dict ch (List.single pos)
        |> Dict.to_list

    { rows, cols, ants: ants }

example_str : Str
example_str =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """
