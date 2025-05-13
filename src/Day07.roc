module [solution_day_07]

import Util exposing [Solution, unwrap]
import "../data/day_07.txt" as input_str : Str

Op : { res : U64, vals : List U64 }

solution_day_07 : Solution
solution_day_07 = {
    day: 07,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 5837374519342

expected_part2 : U64
expected_part2 = 492383931650959

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    in_str
    |> parse
    |> List.walk 0 |acc, op|
        if eval_1 op.res op.vals then op.res + acc else acc
    |> Ok

expect part1 example_str == Ok 3749
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> parse
    |> List.walk 0 |acc, op|
        if eval_2 op.res op.vals then op.res + acc else acc
    |> Ok

expect part2 example_str == Ok 11387
expect part2 input_str == Ok expected_part2

eval_1 : U64, List U64 -> Bool
eval_1 = |res, vals|
    when vals is
        [_] -> Bool.false
        [v1, v2] -> v1 + v2 == res or v1 * v2 == res
        _ ->
            val = List.last vals |> unwrap "List.last failed"
            new_vals = List.drop_last vals 1
            is_mul = res % val == 0 and eval_1(res // val, new_vals)
            is_add = res >= val and eval_1(res - val, new_vals)
            is_mul or is_add

expect eval_1 190 [10, 19] == Bool.true
expect eval_1 3267 [81, 40, 27] == Bool.true
expect eval_1 161011 [16, 10, 13] == Bool.false

eval_2 : U64, List U64 -> Bool
eval_2 = |res, vals|
    when vals is
        [v] -> v == res
        [v1, v2] -> v1 + v2 == res or v1 * v2 == res or concat_u64 v1 v2 == res
        _ ->
            val = List.last vals |> unwrap "List.last failed"
            new_vals = List.drop_last vals 1
            is_mul = res % val == 0 and eval_2(res // val, new_vals)
            is_add = res >= val and eval_2(res - val, new_vals)
            is_concat =
                when un_concat_u64 res val is
                    Ok(lhs) -> eval_2(lhs, new_vals)
                    _ -> Bool.false

            is_mul or is_add or is_concat

expect eval_2 7290 [6, 8, 6, 15] == Bool.true

concat_u64 : U64, U64 -> U64
concat_u64 = |a, b|
    calc_base : U64, U64 -> U64
    calc_base = |t, v|
        if t > 0 then
            calc_base (t // 10) (v * 10)
        else
            v

    base = calc_base b 1
    a * base + b

expect concat_u64 123 456 == 123456

un_concat_u64 : U64, U64 -> Result U64 [NotMatching]
un_concat_u64 = |joined, right_side|
    calc_divisor : U64, U64 -> U64
    calc_divisor = |check, divisor_|
        if check >= 10 then
            calc_divisor (check // 10) (divisor_ * 10)
        else
            divisor_

    divisor = calc_divisor right_side 10
    if joined % divisor == right_side then
        Ok (joined // divisor)
    else
        Err NotMatching

expect un_concat_u64 123456 456 == Ok 123
expect un_concat_u64 123456 3456 == Ok 12
expect un_concat_u64 123456 111 == Err NotMatching

parse : Str -> List Op
parse = |in_str|
    in_str
    |> Str.split_on "\n"
    |> List.map |line|
        parts = Str.split_on line ": "
        res = List.first parts |> unwrap "List.first failed" |> Str.to_u64 |> unwrap "Str.to_U64 failed"
        rest = List.get parts 1 |> unwrap "List.get failed"
        vals =
            rest
            |> Str.split_on " "
            |> List.map |s| Str.to_u64 s |> unwrap "Str.to_u64 failed"
        { res, vals }

example_str : Str
example_str =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """
