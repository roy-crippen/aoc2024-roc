module [solution_day_99]

import Util exposing [Solution]
import "../data/day_99.txt" as input_str : Str

solution_day_99 : Solution
solution_day_99 = {
    day: 99,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 42

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |_in_str| Ok 42

expect part1 example_str == Ok 42
# expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

example_str : Str
example_str = ""
