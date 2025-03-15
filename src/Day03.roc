module [solution_day_03]
import Bool exposing [true, false]
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
part1 = |in_str|
    in_str
    |> parse_mul_pairs
    |> List.walk(0, |acc, (v1, v2)| acc + Num.to_u64 v1 * Num.to_u64 v2)
    |> Ok

expect part1 example_str1 == Ok 161
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> remove_donts
    |> parse_mul_pairs
    |> List.walk(0, |acc, (v1, v2)| acc + Num.to_u64 v1 * Num.to_u64 v2)
    |> Ok

expect part2 example_str2 == Ok 48
expect part2 input_str == Ok expected_part2

parse_mul_pairs : Str -> List (U32, U32)
parse_mul_pairs = |input|
    input
    |> Str.to_utf8
    |> List.walk { pairs: [], current: [], after_mul: false, in_numbers: false } process_char
    |> .pairs

ParseMulState : { pairs : List (U32, U32), current : List U8, after_mul : Bool, in_numbers : Bool }
process_char : ParseMulState, U8 -> ParseMulState
process_char = |state, char|
    when char is
        'l' if List.len state.current >= 2 and List.sublist state.current { start: List.len state.current - 2, len: 2 } == ['m', 'u'] ->
            { state & after_mul: true, current: List.append state.current char }

        '(' if state.after_mul ->
            { state & current: [], after_mul: false, in_numbers: true }

        ')' if state.in_numbers ->
            nums = Str.split_on (Str.from_utf8 state.current |> Result.with_default "") ","
            when nums is
                [first, second] ->
                    when (Str.to_u32 first, Str.to_u32 second) is
                        (Ok n1, Ok n2) ->
                            pairs = List.append state.pairs (n1, n2)
                            { state & pairs, current: [], after_mul: false, in_numbers: false }

                        _ ->
                            { state & current: [], after_mul: false, in_numbers: false }

                _ ->
                    { state & current: [], after_mul: false, in_numbers: false }

        _ if state.in_numbers ->
            { state & current: List.append state.current char }

        _ ->
            { state & current: List.append state.current char, after_mul: false }

remove_donts : Str -> Str
remove_donts = |input|
    parsed = input |> Str.to_utf8 |> List.walk { prev: [], keep: [], is_do: true } process_char1
    Str.from_utf8(parsed.keep) |> Result.with_default ""

StateDont : { prev : List U8, keep : List U8, is_do : Bool }
process_char1 : StateDont, U8 -> StateDont
process_char1 = |st, char|
    st1 = { st & prev: List.append st.prev char }
    st2 = if st1.is_do then { st1 & keep: List.append st1.keep char } else st1
    when char is
        ')' ->
            donts = List.sublist st2.prev { start: List.len st2.prev - 7, len: 7 }
            dos = donts |> List.drop_first 3
            when (dos, donts) is
                (['d', 'o', '(', ')'], _) -> { st2 & is_do: true }
                (_, ['d', 'o', 'n', '\'', 't', '(', ')']) -> { st2 & is_do: false }
                (_, _) -> st2

        _ -> st2

example_str1 : Str
example_str1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example_str2 : Str
example_str2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
