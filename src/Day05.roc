module [solution_day_05]

import Bool exposing [true, false]
import Util exposing [Solution]
import "../data/day_05.txt" as input_str : Str

Rules : Dict U16 (Set U16)

solution_day_05 : Solution
solution_day_05 = {
    day: 05,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 6034

expected_part2 : U64
expected_part2 = 6305

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (rules, updates) = parse in_str
    goods = updates |> List.keep_if |xs| is_in_order rules xs
    middle_f = |xs| List.drop_first xs (List.len(xs) // 2) |> List.take_first 1
    centers = goods |> List.map middle_f |> List.join

    # sum up the centers
    centers |> List.sum |> Num.to_u64 |> Ok

expect part1 example_str == Ok 143
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (rules, updates) = parse in_str
    bads = updates |> List.keep_if |xs| !(is_in_order rules xs)
    sort_f = |xs| List.sort_with xs |a, b| cmp_by_rule rules a b
    goods = bads |> List.map sort_f
    middle_f = |xs| List.drop_first xs (List.len(xs) // 2) |> List.take_first 1
    centers = goods |> List.map middle_f |> List.join

    # sum up the centers
    centers |> List.sum |> Num.to_u64 |> Ok

expect part2 example_str == Ok 123
expect part2 input_str == Ok expected_part2

cmp_by_rule : Rules, U16, U16 -> [LT, GT, EQ]
cmp_by_rule = |rules, a, b|
    a_sucs = Dict.get rules a
    b_sucs = Dict.get rules b
    when (a_sucs, b_sucs) is
        (Err _, _) -> GT
        (_, Err _) -> LT
        (Ok a_set, Ok b_set) ->
            b_in_set_a = Set.contains a_set b
            a_in_set_b = Set.contains b_set a
            if b_in_set_a and !a_in_set_b then
                LT
            else if !b_in_set_a and a_in_set_b then
                GT
            else
                EQ # will never happen

        (_, _) -> EQ

is_in_order : Rules, List U16 -> Bool
is_in_order = |rules, xs|
    Util.window_by_2 xs
    |> List.walk_until true |acc, (curr, next)|
        if Dict.contains rules curr then
            set0 = Dict.get rules curr |> Util.msg_unwrap "Dict.get failed"
            if Set.contains set0 next then Continue acc else Break false
        else
            Break false

parse : Str -> (Rules, List (List U16))
parse = |s|
    rec = Str.split_on s "\n" |> List.split_on "" |> List.split_at 1
    s_rules = rec.before |> List.first |> Util.msg_unwrap "List.first failed"
    s_updates = rec.others |> List.first |> Util.msg_unwrap "List.first failed"

    pairs =
        List.map s_rules |ss|
            when Str.split_on ss "|" is
                [s1, s2] ->
                    v1 = Str.to_u16 s1 |> Util.msg_unwrap "Str.to_u16 fail"
                    v2 = Str.to_u16 s2 |> Util.msg_unwrap "Str.to_u16 fail"
                    (v1, v2)

                _ -> crash("parse failed")

    new_dict : Dict U16 (Set U16)
    new_dict = Dict.with_capacity 2000
    rules = List.walk pairs new_dict |acc_dict, pair|
        (k, v) = pair
        if Dict.contains acc_dict k then
            set0 = Dict.get acc_dict k |> Util.msg_unwrap "Dict.get failed"
            set1 = Set.insert set0 v
            Dict.insert acc_dict k set1
        else
            set0 : Set U16
            set0 = Set.empty {} |> Set.insert v
            Dict.insert acc_dict k set0

    updates = List.map s_updates |s1|
        Str.split_on s1 ","
        |> List.map |s2| Str.to_u16 s2 |> Util.msg_unwrap "Str.to_u16 failed"

    (rules, updates)

# (s_rules, s_updates) = Str.split_on s "\n" |> Str.split_first

example_str : Str
example_str =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """
