module [solution_day_03]

import parser.String exposing [parse_str, digits, string, codeunit, one_of]
import parser.Parser exposing [Parser, parse_partial]
import Util exposing [Solution]
import "../data/day_03.txt" as input_str : Str

Op : [Do, Dont, Mul U64 U64]

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
part1 = |in_str|
    in_str
    |> parse
    |> evaluate
    |> Ok

# expect part1 example_str1 == Ok 161
# expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> parse
    |> filter_part2
    |> evaluate
    |> Ok

expect part2 example_str2 == Ok 48
# expect part2 input_str == Ok expected_part2

filter_part2 : List Op -> List Op
filter_part2 = |ops| filter_part2_loop([], ops, Bool.true)

filter_part2_loop : List Op, List Op, Bool -> List Op
filter_part2_loop = |acc, ops, is_do|
    when ops is
        [] -> acc
        [op, .. as rest] ->
            when op is
                Mul _ _ if is_do -> filter_part2_loop(List.append acc op, rest, is_do)
                Mul _ _ -> filter_part2_loop(acc, rest, is_do)
                Do -> filter_part2_loop(acc, rest, Bool.true)
                Dont -> filter_part2_loop(acc, rest, Bool.false)

evaluate : List Op -> U64
evaluate = |muls|
    muls
    |> List.walk 0 |acc, op|
        when op is
            Mul v1 v2 -> acc + v1 * v2
            _ -> acc

parse : Str -> List Op
parse = |s| parse_loop (Str.to_utf8 s) []

parse_loop : List U8, List Op -> List Op
parse_loop = |xs, ops|
    when xs is
        [] -> ops
        [_, .. as rest] ->
            when parse_partial(parse_op, xs) is
                Ok { val: op, input: remaining } ->
                    # dbg (Str.from_utf8 remaining)
                    parse_loop(remaining, List.append(ops, op))

                Err _ ->
                    # dbg (Str.from_utf8 rest)
                    parse_loop(rest, ops)

parse_op : Parser (List U8) Op
parse_op = one_of([parse_mul, parse_do_or_dont])

parse_do_or_dont : Parser (List U8) Op
parse_do_or_dont = one_of(
    [
        string("do()") |> Parser.map |_| Do,
        string("don't()") |> Parser.map |_| Dont,
    ],
)

parse_mul : Parser (List U8) Op
parse_mul =
    { Parser.map2 <-
        _: string "mul(",
        a: digits,
        _: codeunit ',',
        b: digits,
        _: codeunit ')',
    }
    |> Parser.map |{ a, b }| Mul a b

expect parse_str parse_mul "mul(123,456)" == Ok (Mul 123 456)
expect parse_str parse_mul "mul(123,456]" |> Result.is_err

expect
    expected = [Mul 2 4, Mul 5 5, Mul 11 8, Mul 8 5]
    got = parse example_str1 |> dbg
    expected == got

expect
    expected = [Mul 2 4, Dont, Mul 5 5, Mul 11 8, Do, Mul 8 5]
    got = parse example_str2 |> dbg
    expected == got

expect
    expected = [Mul 2 4, Mul 8 5]
    got = parse example_str2 |> filter_part2 |> dbg
    expected == got

example_str1 : Str
example_str1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example_str2 : Str
example_str2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
