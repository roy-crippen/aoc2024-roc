module [solution_day_21]

import Util exposing [Solution]

# port of https://github.com/hyperneutrino/advent-of-code/blob/main/2024/day21p2.py

input_str : Str
input_str =
    """
    839A
    169A
    579A
    670A
    638A
    """

dir_seqs : Dict (U8, U8) (List Str)
dir_seqs = Dict.from_list [
    (('<', '<'), ["A"]),
    (('<', '>'), [">>A"]),
    (('<', 'A'), [">^>A", ">>^A"]),
    (('<', '^'), [">^A"]),
    (('<', 'v'), [">A"]),
    (('>', '<'), ["<<A"]),
    (('>', '>'), ["A"]),
    (('>', 'A'), ["^A"]),
    (('>', '^'), ["^<A", "<^A"]),
    (('>', 'v'), ["<A"]),
    (('A', '<'), ["v<<A", "<v<A"]),
    (('A', '>'), ["vA"]),
    (('A', 'A'), ["A"]),
    (('A', '^'), ["<A"]),
    (('A', 'v'), ["v<A", "<vA"]),
    (('^', '<'), ["v<A"]),
    (('^', '>'), ["v>A", ">vA"]),
    (('^', 'A'), [">A"]),
    (('^', '^'), ["A"]),
    (('^', 'v'), ["vA"]),
    (('v', '<'), ["<A"]),
    (('v', '>'), [">A"]),
    (('v', 'A'), ["^>A", ">^A"]),
    (('v', '^'), ["^A"]),
    (('v', 'v'), ["A"]),
]

dir_lengths : Dict (U8, U8)* U64
dir_lengths = Dict.map(dir_seqs, |_k, ls| ls |> List.first |> Util.unwrap |> Str.to_utf8 |> List.len)

num_seqs : Dict (U8, U8) (List Str)
num_seqs = Dict.from_list [
    (('0', '0'), ["A"]),
    (('0', '1'), ["^<A"]),
    (('0', '2'), ["^A"]),
    (('0', '3'), ["^>A", ">^A"]),
    (('0', '4'), ["^^<A"]),
    (('0', '5'), ["^^A"]),
    (('0', '6'), ["^^>A", ">^^A"]),
    (('0', '7'), ["^^^<A"]),
    (('0', '8'), ["^^^A"]),
    (('0', '9'), ["^^^>A", ">^^^A"]),
    (('0', 'A'), [">A"]),
    (('1', '0'), [">vA"]),
    (('1', '1'), ["A"]),
    (('1', '2'), [">A"]),
    (('1', '3'), [">>A"]),
    (('1', '4'), ["^A"]),
    (('1', '5'), ["^>A", ">^A"]),
    (('1', '6'), ["^>>A", ">>^A"]),
    (('1', '7'), ["^^A"]),
    (('1', '8'), ["^^>A", ">^^A"]),
    (('1', '9'), ["^^>>A", ">>^^A"]),
    (('1', 'A'), [">>vA"]),
    (('2', '0'), ["vA"]),
    (('2', '1'), ["<A"]),
    (('2', '2'), ["A"]),
    (('2', '3'), [">A"]),
    (('2', '4'), ["^<A", "<^A"]),
    (('2', '5'), ["^A"]),
    (('2', '6'), ["^>A", ">^A"]),
    (('2', '7'), ["^^<A", "<^^A"]),
    (('2', '8'), ["^^A"]),
    (('2', '9'), ["^^>A", ">^^A"]),
    (('2', 'A'), [">vA", "v>A"]),
    (('3', '0'), ["<vA", "v<A"]),
    (('3', '1'), ["<<A"]),
    (('3', '2'), ["<A"]),
    (('3', '3'), ["A"]),
    (('3', '4'), ["^<<A", "<<^A"]),
    (('3', '5'), ["^<A", "<^A"]),
    (('3', '6'), ["^A"]),
    (('3', '7'), ["^^<<A", "<<^^A"]),
    (('3', '8'), ["^^<A", "<^^A"]),
    (('3', '9'), ["^^A"]),
    (('3', 'A'), ["vA"]),
    (('4', '0'), [">vvA"]),
    (('4', '1'), ["vA"]),
    (('4', '2'), [">vA", "v>A"]),
    (('4', '3'), [">>vA", "v>>A"]),
    (('4', '4'), ["A"]),
    (('4', '5'), [">A"]),
    (('4', '6'), [">>A"]),
    (('4', '7'), ["^A"]),
    (('4', '8'), ["^>A", ">^A"]),
    (('4', '9'), ["^>>A", ">>^A"]),
    (('4', 'A'), [">>vvA"]),
    (('5', '0'), ["vvA"]),
    (('5', '1'), ["<vA", "v<A"]),
    (('5', '2'), ["vA"]),
    (('5', '3'), [">vA", "v>A"]),
    (('5', '4'), ["<A"]),
    (('5', '5'), ["A"]),
    (('5', '6'), [">A"]),
    (('5', '7'), ["^<A", "<^A"]),
    (('5', '8'), ["^A"]),
    (('5', '9'), ["^>A", ">^A"]),
    (('5', 'A'), [">vvA", "vv>A"]),
    (('6', '0'), ["<vvA", "vv<A"]),
    (('6', '1'), ["<<vA", "v<<A"]),
    (('6', '2'), ["<vA", "v<A"]),
    (('6', '3'), ["vA"]),
    (('6', '4'), ["<<A"]),
    (('6', '5'), ["<A"]),
    (('6', '6'), ["A"]),
    (('6', '7'), ["^<<A", "<<^A"]),
    (('6', '8'), ["^<A", "<^A"]),
    (('6', '9'), ["^A"]),
    (('6', 'A'), ["vvA"]),
    (('7', '0'), [">vvvA"]),
    (('7', '1'), ["vvA"]),
    (('7', '2'), [">vvA", "vv>A"]),
    (('7', '3'), [">>vvA", "vv>>A"]),
    (('7', '4'), ["vA"]),
    (('7', '5'), [">vA", "v>A"]),
    (('7', '6'), [">>vA", "v>>A"]),
    (('7', '7'), ["A"]),
    (('7', '8'), [">A"]),
    (('7', '9'), [">>A"]),
    (('7', 'A'), [">>vvvA"]),
    (('8', '0'), ["vvvA"]),
    (('8', '1'), ["<vvA", "vv<A"]),
    (('8', '2'), ["vvA"]),
    (('8', '3'), [">vvA", "vv>A"]),
    (('8', '4'), ["<vA", "v<A"]),
    (('8', '5'), ["vA"]),
    (('8', '6'), [">vA", "v>A"]),
    (('8', '7'), ["<A"]),
    (('8', '8'), ["A"]),
    (('8', '9'), [">A"]),
    (('8', 'A'), [">vvvA", "vvv>A"]),
    (('9', '0'), ["<vvvA", "vvv<A"]),
    (('9', '1'), ["<<vvA", "vv<<A"]),
    (('9', '2'), ["<vvA", "vv<A"]),
    (('9', '3'), ["vvA"]),
    (('9', '4'), ["<<vA", "v<<A"]),
    (('9', '5'), ["<vA", "v<A"]),
    (('9', '6'), ["vA"]),
    (('9', '7'), ["<<A"]),
    (('9', '8'), ["<A"]),
    (('9', '9'), ["A"]),
    (('9', 'A'), ["vvvA"]),
    (('A', '0'), ["<A"]),
    (('A', '1'), ["^<<A"]),
    (('A', '2'), ["^<A", "<^A"]),
    (('A', '3'), ["^A"]),
    (('A', '4'), ["^^<<A"]),
    (('A', '5'), ["^^<A", "<^^A"]),
    (('A', '6'), ["^^A"]),
    (('A', '7'), ["^^^<<A"]),
    (('A', '8'), ["^^^<A", "<^^^A"]),
    (('A', '9'), ["^^^A"]),
    (('A', 'A'), ["A"]),
]

product : List (List Str) -> List Str
product = |lists|
    when lists is
        [] -> [""]
        [hd, .. as tl] ->
            rest_products = product tl
            List.join_map hd |prefix|
                List.map(rest_products, |suffix| Str.concat prefix suffix)

selections : Str -> List Str
selections = |s|
    Util.zip(Str.to_utf8(Str.concat("A", s)), Str.to_utf8(s))
    |> List.map(|key| Dict.get(num_seqs, key) |> Util.unwrap)
    |> product

compute_lengths : Str, U8, Dict (Str, U8) U64 -> (U64, Dict (Str, U8) U64)
compute_lengths = |seq, depth, cache|
    when Dict.get(cache, (seq, depth)) is
        Ok v -> (v, cache)
        _ ->
            zs = Util.zip(Str.to_utf8(Str.concat("A", seq)), Str.to_utf8(seq))
            if depth == 1 then
                min_len = zs |> List.map(|key| Dict.get(dir_lengths, key) |> Util.unwrap) |> List.sum
                (min_len, Dict.insert(cache, (seq, 1), min_len))
            else
                (v, c) = List.walk zs (0, cache) |(acc_v, acc_cache), key|
                    subseqs = Dict.get(dir_seqs, key) |> Util.unwrap

                    (v1, c1) = List.walk subseqs (Num.max_u64, acc_cache) |(acc_v1, acc_cache1), subseq|
                        (v2, c2) = compute_lengths(subseq, depth - 1, acc_cache1)
                        min = Num.min(acc_v1, v2)
                        (min, c2)

                    sum = acc_v + v1
                    (sum, Dict.insert(c1, (seq, depth), sum))

                (v, c)

solution_day_21 : Solution
solution_day_21 = {
    day: 21,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 205160

expected_part2 : U64
expected_part2 = 252473394928452

solve : Str, U8 -> U64
solve = |in_str, depth|
    cache = Dict.empty({})
    xs = in_str |> Str.split_on("\n")
    (v1, _cache1) =
        xs
        |> List.walk (0, cache) |(acc_v, acc_c), s|
            (v2, cache2) =
                s
                |> selections
                |> List.walk (Num.max_u64, acc_c) |(acc_v1, acc_c1), sel|
                    (v3, cache3) = compute_lengths(sel, depth, acc_c1)
                    (Num.min(acc_v1, v3), cache3)
            a = Str.drop_suffix(s, "A") |> Str.to_u64 |> Util.unwrap
            (acc_v + v2 * a, cache2)

    v1

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str| Ok solve(in_str, 2)

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str| Ok solve(in_str, 25)

example_str : Str
example_str =
    """
    029A
    980A
    179A
    456A
    379A
    """

# tests

expect part1 example_str == Ok 126384
expect part1 input_str == Ok expected_part1
expect part2 example_str == Ok 154115708116294
# expect part2 input_str == Ok expected_part2

expect
    expected = ["a1bc1d", "a1bc2d", "a1bc3d", "a2bc1d", "a2bc2d", "a2bc3d"]
    product([["a1", "a2"], ["b"], ["c1", "c2", "c3"], ["d"]]) == expected
expect product([["a"], ["b"]]) == ["ab"]
expect product([["a"], ["b", "c"]]) == ["ab", "ac"]

