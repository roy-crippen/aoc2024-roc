module [solution_day_13]

import Util exposing [Solution]
import "../data/day_13.txt" as input_str : Str

ABV : (I64, I64, I64) # type for equation ax + by = v
XY : (I64, I64) # type for solution (x, y)

parse_values : Str, Str -> (I64, I64)
parse_values = |s, ch|
    when List.map (Str.split_on s ",") |s1| Str.split_on s1 ch is
        [[_, t1], [_, t2]] -> (Str.to_i64 t1 |> Util.unwrap, Str.to_i64 t2 |> Util.unwrap)
        _ -> crash "invalid input"

parse_abv : List Str -> (ABV, ABV)
parse_abv = |xs|
    (s1, s2, s3) =
        when xs is
            [t1, t2, t3] -> (t1, t2, t3)
            _ -> crash "invalid input"
    (a1, a2) = parse_values s1 "+"
    (b1, b2) = parse_values s2 "+"
    (v1, v2) = parse_values s3 "="
    abv1 = (a1, b1, v1)
    abv2 = (a2, b2, v2)
    (abv1, abv2)

parse : Str -> List (ABV, ABV)
parse = |s|
    xs1 = Str.split_on s "\n"
    xs2 = List.keep_if xs1 |s1| !(Str.is_empty s1)
    xs3 = List.chunks_of xs2 3 |> List.map |xs| parse_abv xs
    xs3

solve_equation : (ABV, ABV), I64 -> [Err [NoSolution], Ok XY]
solve_equation = |((a1, b1, v1_), (a2, b2, v2_)), offset|
    v1 = v1_ + offset
    v2 = v2_ + offset
    d = a1 * b2 - a2 * b1
    if d == 0 then
        Err NoSolution
    else
        nx = v1 * b2 - v2 * b1
        ny = a1 * v2 - a2 * v1
        if Num.is_multiple_of nx d and Num.is_multiple_of ny d then
            Ok (nx // d, ny // d)
        else
            Err NoSolution

solve : (ABV, ABV), I64 -> U64
solve = |equation, offset|
    when solve_equation equation offset is
        Ok (a, b) -> Num.to_u64 (3 * a + b)
        _ -> 0

solution_day_13 : Solution
solution_day_13 = {
    day: 13,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 29598

expected_part2 : U64
expected_part2 = 93217456941970

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    parse in_str
    |> List.map (|y| solve y 0)
    |> List.sum
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    parse in_str
    |> List.map (|y| solve y 10000000000000)
    |> List.sum
    |> Ok

example_str : Str
example_str =
    """
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    """

# tests

expect part1 example_str == Ok 480
expect part1 input_str == Ok expected_part1
expect part2 example_str == Ok 875318608908
expect part2 input_str == Ok expected_part2
expect parse_values "Button A: X+94, Y+34" "+" == (94, 34)
expect parse_values "Prize: X=8400, Y=5400" "=" == (8400, 5400)
expect parse_abv ["Button A: X+94, Y+34", "Button B: X+22, Y+67", "Prize: X=8400, Y=5400"] == ((94, 22, 8400), (34, 67, 5400))

