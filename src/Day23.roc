module [solution_day_23]

import "../data/day_23.txt" as input_str : Str
import Util exposing [Solution, unwrap]

parse : Str -> Dict Str (Set Str)
parse = |s|
    Str.split_on(s, "\n")
    |> List.map(|str| Str.split_on(str, "-"))
    |> List.walk(
        Dict.with_capacity(520),
        |acc, pair|
            update : Dict Str (Set Str), Str, Str -> Dict Str (Set Str)
            update = |d, sa, sb|
                when Dict.get(d, sa) is
                    Ok(set) -> Dict.insert(d, sa, Set.insert(set, sb))
                    _ -> Dict.insert(d, sa, Set.with_capacity(15) |> Set.insert(sb))

            s1 = List.get(pair, 0) |> unwrap
            s2 = List.get(pair, 1) |> unwrap
            acc |> update(s1, s2) |> update(s2, s1),
    )

encode : Str -> U16
encode = |s|
    when Str.to_utf8 s is
        [b1, b2] ->
            Num.bitwise_or (Num.shift_left_by (Num.to_u16 b1) 8) (Num.to_u16 b2)

        _ -> crash "Expected exactly 2 ASCII characters"

decode : U16 -> Str
decode = |key|
    b1 = Num.to_u8 (Num.shift_right_by key 8)
    b2 = Num.to_u8 (Num.bitwise_and key 0xFFu16)
    when Str.from_utf8 [b1, b2] is
        Ok s -> s
        Err _ -> crash "Invalid UTF8"

find_cliques : Dict Str (Set Str) -> Set (List U16)
find_cliques = |edges|
    es = Dict.map edges |_k, set| Set.to_list(set)
    List.walk Dict.keys(es) Set.with_capacity(12000) |acc1, x|
        List.walk (Dict.get(es, x) |> unwrap) acc1 |acc2, y|
            List.walk (Dict.get(es, y) |> unwrap) acc2 |acc3, z|
                if x != z and Set.contains(Dict.get(edges, z) |> unwrap, x) then
                    Set.insert(acc3, List.sort_asc [encode(x), encode(y), encode(z)])
                else
                    acc3

solution_day_23 : Solution
solution_day_23 = {
    day: 23,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 1000

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    parse(in_str)
    |> find_cliques
    |> Set.to_list
    |> List.map(|vs| vs |> List.map(decode))
    |> List.keep_if(|ss| ss |> List.any(|s| Str.starts_with(s, "t")))
    |> List.len
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str|
    # time_start1 = Utc.now!({})
    # d1 = (Num.to_frac Utc.delta_as_nanos(Utc.now!({}), time_start1)) / 1000000.0
    Ok 42

example_str : Str
example_str =
    """
    kh-tc
    qp-kh
    de-cg
    ka-co
    yn-aq
    qp-ub
    cg-tb
    vc-aq
    tb-ka
    wh-tc
    yn-cg
    kh-ub
    ta-co
    de-co
    tc-td
    tb-wq
    wh-td
    ta-ka
    td-qp
    aq-cg
    wq-ub
    ub-vc
    de-ta
    wq-aq
    wq-vc
    wh-yn
    ka-de
    kh-ta
    co-tc
    wh-qp
    tb-vc
    td-yn
    """

# tests

# expect part1 example_str == Ok 7
# expect part1 input_str |> dbg == Ok expected_part1
expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

