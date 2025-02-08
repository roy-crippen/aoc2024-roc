app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
}

import pf.Stdout
import pf.Utc
import Util exposing [Solution, red, green, blue]
import Day01 exposing [solution_day_01]
import Day02 exposing [solution_day_02]
import Day03 exposing [solution_day_03]

main! = |_args|
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
]

run_solution! : Solution => Str
run_solution! = |sol|
    Str.join_with
        [
            blue "day ${Num.to_str sol.day} -> part1: ",
            run_part! (sol, Bool.true),
            blue ", part2: ",
            run_part! (sol, Bool.false),
        ]
        ""

run_part! : (Solution, Bool) => Str
run_part! = |(sol, is_part1)|
    (f, expected) = if is_part1 then (sol.part1, sol.expected_part1) else (sol.part2, sol.expected_part2)

    time_start = Utc.now!({})
    res = f sol.input_str
    time_end = Utc.now!({})
    duration = Utc.delta_as_millis(time_end, time_start)
    # dbg duration

    when res is
        Ok v ->
            if
                v == expected
            then
                green "(v: ${Num.to_str v}, dur: ${Num.to_str duration}ms)"
            else
                red "${Num.to_str v} != expected ${Num.to_str expected}"

        _ -> red "failed to execute"
