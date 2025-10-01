module [solution_day_11]

import Util exposing [Solution]
import "../data/day_11.txt" as input_str : Str

Label : U64
Quantity : U64
StoneCounts : Dict Label Quantity

solution_day_11 : Solution
solution_day_11 = {
    day: 11,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 239714

expected_part2 : U64
expected_part2 = 284973560658514

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    stone_counts = parse in_str
    blink stone_counts 25
    |> Dict.values
    |> List.sum
    |> Ok

expect part1 example_str == Ok 55312
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    stone_counts = parse in_str
    blink stone_counts 75
    |> Dict.values
    |> List.sum
    |> Ok

expect part2 example_str |> dbg == Ok 65601038650482
expect part2 input_str |> dbg == Ok expected_part2

blink : StoneCounts, U8 -> StoneCounts
blink = |stone_counts, iters|
    if iters == 0 then
        stone_counts
    else
        new_dict = Dict.walk stone_counts (Dict.empty {}) |acc_dict, label, qty|
            alter_value : Result U64 [Missing] -> Result U64 [Missing]
            alter_value = |possible_value|
                when possible_value is
                    Err Missing -> Ok(qty)
                    Ok value -> Ok(value + qty)

            when label is
                0 -> Dict.update acc_dict 1 alter_value
                n ->
                    if (digits_cnt n) % 2 == 0 then
                        (l1, l2) = split n
                        d = Dict.update acc_dict l1 alter_value
                        Dict.update d l2 alter_value
                    else
                        Dict.update acc_dict (n * 2024) alter_value
        blink new_dict (iters - 1)

parse : Str -> StoneCounts
parse = |in_str|
    in_str
    |> Str.split_on " "
    |> List.map |s|
        label = (Str.to_u64 s |> Util.msg_unwrap "Num.to_u64 failed")
        (label, 1)
    |> Dict.from_list

digits_cnt : U64 -> U64
digits_cnt = |n| if n < 10 then 1 else (1 + digits_cnt (n // 10))

expect digits_cnt 12345 == 5
expect digits_cnt 123457689 == 9
expect digits_cnt 1 == 1

split : U64 -> (U64, U64)
split = |n|
    digits = digits_cnt n
    divisor = Num.pow_int 10 (digits // 2)
    (n // divisor, n % divisor)

expect split 5 == (5, 0)
expect split 56 == (5, 6)
expect split 563 == (56, 3)
expect split 5635 == (56, 35)
expect split 562001 == (562, 1)
expect split 5600 == (56, 0)

example_str : Str
example_str = "125 17"
