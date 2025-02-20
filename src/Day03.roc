module [solution_day_03]

import Util exposing [Solution]
import "../data/day_03.txt" as input_str : Str

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
part1 = |_in_str| Ok 42

expect part1 example_str == Ok 161
# expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

expect part2 example_str == Ok 48
# expect part2 input_str == Ok expected_part2

example_str : Str
example_str = ""
