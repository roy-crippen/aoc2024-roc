module [solution_day_19]

import Util exposing [Solution]
import "../data/day_19.txt" as input_str : Str
import Bool exposing [true, false]

solution_day_19 : Solution
solution_day_19 = {
    day: 19,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

parse : Str -> (List (List U8), Set (List U8), U64)
parse = |s|
    lines = Str.split_on(s, "\n")
    patterns = List.get(lines, 0) |> Util.unwrap |> Str.split_on(", ") |> List.map(Str.to_utf8)
    max_pat_len = patterns |> List.map(List.len) |> List.max |> Util.unwrap
    designs = List.drop_first(lines, 2) |> List.map(Str.to_utf8)
    (designs, patterns |> Set.from_list, max_pat_len)

can_obtain : List U8, Set (List U8), U64 -> Bool
can_obtain = |design, patterns, max_pat_len|
    go : List U8 -> Bool
    go = |ds|
        if List.is_empty(ds) then
            true
        else
            vs = List.range({ start: At 0, end: At Num.min(List.len(ds), max_pat_len) })
            List.walk_until vs false |_, i|
                split = List.split_at(ds, i)
                if Set.contains(patterns, split.before) then
                    if go(split.others) then Break true else Continue false
                else
                    Continue false

    go(design)

expected_part1 : U64
expected_part1 = 242

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (designs, patterns, max_pat_len) = parse(in_str)
    count =
        designs
        |> List.keep_if(|design| can_obtain(design, patterns, max_pat_len))
        |> List.len
    Ok count

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    r, wr, b, g, bwu, rb, gb, br

    brwrr
    bggr
    gbbr
    rrbgbr
    ubwu
    bwurrg
    brgr
    bbrgwb
    """

# tests

expect part1 example_str == Ok 6
expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2
expect
    (designs, patterns, max_pat_len) = parse(example_str)
    design = designs |> List.first |> Util.unwrap
    found = can_obtain(design, patterns, max_pat_len)
    found == true

