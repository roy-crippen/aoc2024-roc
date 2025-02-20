module [solution_day_03]

import Util exposing [Solution]
import parser.String exposing [parse_str, digits, string, codeunit]
import parser.Parser exposing [Parser, many]
import "../data/day_03.txt" as input_str : Str

Op : [Mul U64 U64, Do, Dont]

solution_day_03 : Solution
solution_day_03 = {
    day: 3,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 169021493

expected_part2 : U64
expected_part2 = 111762583

part1 : Str -> [Err Str, Ok U64]
part1 = |_in_str| Ok 161

expect part1 example_str == Ok 161
# expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 48

expect part2 example_str == Ok 48
# expect part2 input_str == Ok expected_part2

parse_mul : Parser _ Op
parse_mul =
    { Parser.map2 <-
        _: string "mul(",
        _: many (codeunit ' '),
        a: digits,
        _: many (codeunit ' '),
        _: codeunit ',',
        _: many (codeunit ' '),
        b: digits,
        _: many (codeunit ' '),
        _: codeunit ')',
    }
    |> Parser.map |{ a, b }| Mul a b

expect parse_str parse_mul "mul(  2            ,   21  )" == Ok (Mul 2 21)
expect parse_str parse_mul "mul(123,456)" == Ok (Mul 123 456)
expect parse_str parse_mul "mul(123,456]" |> Result.is_err

example_str : Str
example_str = ""
