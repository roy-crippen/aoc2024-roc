module [solution_day_22]

import Util exposing [Solution]
import "../data/day_22.txt" as input_str : Str

State : { a : U64, b : U64, c : U64, d : U64, seen : Set U64, totals : Dict U64 I32 }

parse : Str -> List U64
parse = |s| s |> Str.split_on("\n") |> List.map(|str| Str.to_u64(str) |> Util.unwrap)

mask : U64
mask = Num.shift_left_by(1, 24) - 1

step : U64 -> U64
step = |v|
    v1 = Num.bitwise_xor(v, v * 64) |> Num.bitwise_and(mask)
    v2 = Num.bitwise_xor(v1, v1 // 32) |> Num.bitwise_and(mask)
    Num.bitwise_xor(v2, v2 * 2048) |> Num.bitwise_and(mask)

step_n : U64, U64 -> U64
step_n = |v, n|
    when n is
        0 -> v
        _ -> step_n(step(v), n - 1)

encode : (I32, I32, I32, I32)* -> U64
encode = |(a, b, c, d)|
    off = |x| x + 9
    ((((((off a * 19) + off b) * 19) + off c) * 19) + off d) |> Num.to_u64

solution_day_22 : Solution
solution_day_22 = {
    day: 22,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 21147129593

expected_part2 : U64
expected_part2 = 2445

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    parse(in_str)
    |> List.walk 0 |acc, v| acc + step_n(v, 2000)
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    next_n : State, U64 -> State
    next_n = |st, n|
        when n is
            0 -> st
            _ ->
                alter_value : Result I32 [Missing] -> Result I32 [Missing]
                alter_value = |possible_value|
                    when possible_value is
                        Err Missing -> Ok(price)
                        Ok value -> Ok(value + price)

                next_secret = step(st.d)
                pa = st.a % 10 |> Num.to_i32
                pb = st.b % 10 |> Num.to_i32
                pc = st.c % 10 |> Num.to_i32
                pd = st.d % 10 |> Num.to_i32
                pe = next_secret % 10 |> Num.to_i32
                sum_diffs = pe - pa
                price = pe |> Num.to_i32
                diffs = (pb - pa, pc - pb, pd - pc, pe - pd)
                key = encode(diffs)
                next_st =
                    if sum_diffs != 0 or price < 2 or Set.contains(st.seen, key) then
                        { st & a: st.b, b: st.c, c: st.d, d: next_secret }
                    else
                        next_totals = Dict.update(st.totals, key, alter_value)
                        next_seen = Set.insert(st.seen, key)
                        { a: st.b, b: st.c, c: st.d, d: next_secret, seen: next_seen, totals: next_totals }
                next_n(next_st, n - 1)

    process_buyers : List U64 -> List State
    process_buyers = |buyers|
        buyers
        |> List.map |v|
            next_n(
                {
                    a: v,
                    b: step_n(v, 1),
                    c: step_n(v, 2),
                    d: step_n(v, 3),
                    seen: Set.with_capacity(175),
                    totals: Dict.with_capacity(175),
                },
                1996,
            )

    parse(in_str)
    |> process_buyers
    |> List.map(|st| st.totals |> Dict.to_list)
    |> List.join
    |> Util.group_by
    |> Util.unwrap
    |> List.map(|(_k, ls)| List.sum(ls))
    |> List.max
    |> Util.unwrap
    |> Num.to_u64
    |> Ok

example_str : Str
example_str =
    """
    1
    10
    100
    2024
    """

# tests

expect part1 example_str == Ok 37327623
expect step(123) == 15887950
expect step_n(2024, 2000) == 8667524
