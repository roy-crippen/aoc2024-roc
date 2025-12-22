app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br",
    # parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.10.0/6eZYaXkrakq9fJ4oUc0VfdxU1Fap2iTuAN18q9OgQss.tar.br",
}

import pf.Stdout
import pf.Utc
import Str exposing [concat, count_utf8_bytes, repeat]
import Util exposing [Solution, blue, green, red, yellow, rounded_str]
# import Day01 exposing [solution_day_01]
# import Day02 exposing [solution_day_02]
# import Day03 exposing [solution_day_03]
# import Day04 exposing [solution_day_04]
# import Day05 exposing [solution_day_05]
# import Day06 exposing [solution_day_06]
# import Day07 exposing [solution_day_07]
# import Day08 exposing [solution_day_08]
# import Day09 exposing [solution_day_09]
# import Day10 exposing [solution_day_10]
# import Day11 exposing [solution_day_11]
# import Day12 exposing [solution_day_12]
# import Day13 exposing [solution_day_13]
# import Day14 exposing [solution_day_14]
# import Day15 exposing [solution_day_15]
# import Day16 exposing [solution_day_16]
# import Day17 exposing [solution_day_17]
# import Day18 exposing [solution_day_18]
# import Day19 exposing [solution_day_19]
# import Day20 exposing [solution_day_20]
# import Day21 exposing [solution_day_21]
# import Day22 exposing [solution_day_22]
# import Day23 exposing [solution_day_23]
import Day24 exposing [solution_day_24]

main! = |_args|
    # dirs = Path.from_str "." |> (Path.list_dir! |> Result.with_default []
    # _ = List.map dirs (|p| (Path.display p) |> dbg)

    sols
    |> List.for_each!(
        |sol|
            _ = Stdout.line!(run_solution! sol)
            {},
    )
    Ok {}

sols : List Solution
sols = [
    # solution_day_01,
    # solution_day_02,
    # solution_day_03,
    # solution_day_04,
    # solution_day_05,
    # solution_day_06,
    # solution_day_07,
    # solution_day_08,
    # solution_day_09,
    # solution_day_10,
    # solution_day_11,
    # solution_day_12,
    # solution_day_13,
    # solution_day_14,
    # solution_day_15,
    # solution_day_16,
    # solution_day_17,
    # solution_day_18,
    # solution_day_19,
    # solution_day_20,
    # solution_day_21,
    # solution_day_22,
    # solution_day_23,
    solution_day_24,
]

run_solution! : Solution => Str
run_solution! = |sol|
    day_str = Num.to_str sol.day
    day_formatted = if count_utf8_bytes day_str == 1 then concat "  " day_str else concat " " day_str
    Str.join_with
        [
            blue "day ${day_formatted} part 1",
            run_part! (sol, Bool.true),
            "\n",
            blue "day ${day_formatted} part 2",
            run_part! (sol, Bool.false),
        ]
        ""

run_part! : (Solution, Bool) => Str
run_part! = |(sol, is_part1)|
    (f, expected) = if is_part1 then (sol.part1, sol.expected_part1) else (sol.part2, sol.expected_part2)

    time_start = Utc.now!({})
    res = f sol.input_str
    time_end = Utc.now!({})
    duration = (Num.to_frac Utc.delta_as_nanos(time_end, time_start)) / 1000000.0
    color = if duration < 100.0 then green else yellow

    when res is
        Ok v ->
            if
                v == expected
            then
                v_str = Num.to_str v
                v_spaces = repeat(" ", 17 - count_utf8_bytes v_str)
                v_str_formatted = concat v_spaces v_str
                dur_str = rounded_str duration 2
                dur_spaces = repeat(" ", 10 - count_utf8_bytes dur_str)
                dur_str_formatted = Str.join_with [dur_spaces, dur_str, "ms"] ""
                "${blue v_str_formatted} ${color dur_str_formatted}"
            else
                red " --- ${Num.to_str v} != expected ${Num.to_str expected} ---"

        _ -> red "failed to execute"
