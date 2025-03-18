app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    # parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.10.0/6eZYaXkrakq9fJ4oUc0VfdxU1Fap2iTuAN18q9OgQss.tar.br",
}

import pf.Stdout
import pf.Utc
import Str exposing [concat, count_utf8_bytes, repeat]
import Util exposing [Solution, blue, green, orange, red, yellow]
import Day01 exposing [solution_day_01]
import Day02 exposing [solution_day_02]
import Day03 exposing [solution_day_03]
import Day04 exposing [solution_day_04]
import Day06 exposing [solution_day_06]
import Day09 exposing [solution_day_09]

main! = |_args|
    _ = Stdout.line! "\n"
    _ = Stdout.line! (blue "        -------- part 1 --------  -------- part 2 --------")
    _ = Stdout.line! (blue "   day          value       time          value       time")
    _ = Stdout.line! (blue "------  -------------  ---------  -------------  ---------")
    sols
    |> List.for_each!(
        |sol|
            _ = Stdout.line!(run_solution! sol)
            {},
    )
    Ok {}

sols : List Solution
sols = [
    solution_day_01,
    solution_day_02,
    solution_day_03,
    solution_day_04,
    solution_day_06,
    solution_day_09,
]

run_solution! : Solution => Str
run_solution! = |sol|
    day_str = Num.to_str sol.day
    day_formated = if count_utf8_bytes day_str == 1 then concat "     " day_str else concat "    " day_str
    Str.join_with
        [
            blue "${day_formated}",
            run_part! (sol, Bool.true),
            run_part! (sol, Bool.false),
        ]
        ""

run_part! : (Solution, Bool) => Str
run_part! = |(sol, is_part1)|
    (f, expected) = if is_part1 then (sol.part1, sol.expected_part1) else (sol.part2, sol.expected_part2)

    time_start = Utc.now!({})
    res = f sol.input_str
    time_end = Utc.now!({})
    duration_us = Utc.delta_as_nanos(time_end, time_start) // 1000
    (duration, duration_str, color) =
        if
            duration_us > 5000
        then
            duration_ms = duration_us // 1000
            if duration_ms < 500 then (duration_ms, "ms", yellow) else (duration_ms, "ms", orange)
        else
            (duration_us, "µs", green)

    when res is
        Ok v ->
            if
                v == expected
            then
                v_str = Num.to_str v
                v_spaces = repeat(" ", 15 - count_utf8_bytes v_str)
                v_str_formatted = concat v_spaces v_str
                dur_str = Num.to_str duration
                dur_spaces = repeat(" ", 8 - count_utf8_bytes dur_str)
                dur_str_formatted = concat dur_spaces dur_str
                color "${v_str_formatted} ${dur_str_formatted}${duration_str}"
            else
                red " --- ${Num.to_str v} != expected ${Num.to_str expected} ---"

        _ -> red "failed to execute"
