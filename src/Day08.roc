module [solution_day_08]

import Util exposing [Solution]
import "../data/day_08.txt" as input_str : Str

Board : { rows : I32, cols : I32, ants : List (U8, List Pos) }
Pos : (I32, I32)

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
        combos = make_combos vs.1
        Set.union acc_set (make_antis_part1 combos b.rows b.cols)
    |> Set.len
    |> Ok

expect part1 example_str == Ok 14
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    b = in_str |> parse
    List.walk b.ants Set.empty({}) |acc_set, vs|
        combos = make_combos vs.1
        Set.union acc_set (make_antis_part2 combos b.rows b.cols)
    |> Set.len
    |> Ok

expect part2 example_str == Ok 34
expect part2 input_str == Ok expected_part2

make_combos : List Pos -> Set (Pos, Pos)
make_combos = |xs|
    combos =
        xs
        |> Util.k_combos_without_reps 2
        |> List.map |pairs|
            when pairs is
                [p1, p2] -> (p1, p2)
                _ -> crash("should not be here")

    combos |> Set.from_list

make_anti : Pos, Pos, I32, I32 -> [Err [OutOfBounds], Ok Pos]
make_anti = |(r1, c1), (r2, c2), rows, cols|
    r3 = 2 * r1 - r2
    c3 = 2 * c1 - c2
    if r3 >= 0 and r3 < rows and c3 >= 0 and c3 < cols then
        Ok (r3, c3)
    else
        Err OutOfBounds

make_antis_part1 : Set (Pos, Pos), I32, I32 -> Set Pos
make_antis_part1 = |combos, rows, cols|
    anti_set = Set.walk combos Set.empty({}) |acc0, ((r1, c1), (r2, c2))|
        acc1 =
            when make_anti (r1, c1) (r2, c2) rows cols is
                Ok(pos) -> Set.insert acc0 pos
                _ -> acc0

        acc2 =
            when make_anti (r2, c2) (r1, c1) rows cols is
                Ok(pos) -> Set.insert acc1 pos
                _ -> acc1

        acc2

    anti_set

make_antis : Pos, Pos, I32, I32 -> List Pos
make_antis = |pos1_, pos2_, rows_, cols_|
    go : Pos, Pos, I32, I32, List Pos -> List Pos
    go = |pos1, pos2, rows, cols, res0|
        when make_anti pos1 pos2 rows cols is
            Ok(pos3) ->
                res1 = go pos2 pos3 rows cols (List.append res0 pos3)
                go pos3 pos1 rows cols res1

            _ -> res0

    go pos1_ pos2_ rows_ cols_ []

make_antis_part2 : Set (Pos, Pos), I32, I32 -> Set Pos
make_antis_part2 = |combos, rows, cols|
    anti_list = Set.walk combos [] |acc0, (pos1, pos2)|
        acc1 = List.concat acc0 [pos1, pos2]
        acc2 = List.concat acc1 (make_antis pos1 pos2 rows cols)
        List.concat acc2 (make_antis pos2 pos1 rows cols)

    Set.from_list anti_list

parse : Str -> Board
parse = |in_str|
    lss = Str.split_on in_str "\n" |> List.map Str.to_utf8
    rows = List.len lss |> Num.to_i32
    cols = List.get lss 0 |> Util.unwrap "should be at least 1 row" |> List.len |> Num.to_i32

    ys : List (U8, Pos)
    ys =
        lss
        |>
        List.map_with_index |ls, r|
            List.map_with_index ls |ch, c| (ch, (r |> Num.to_i32, c |> Num.to_i32))
            |> List.keep_if |(ch, _)| ch != '.'
        |> List.keep_if |vs| vs != []
        |> List.join

    ants : List (U8, List Pos)
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

    { rows, cols, ants }

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
