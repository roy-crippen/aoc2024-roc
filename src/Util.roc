module [
    zip,
    unzip,
    window_by_2,
    unique,
    remove_consecutive_duplicates,
    unwrap,
    msg_unwrap,
    blue,
    green,
    orange,
    red,
    yellow,
    Solution,
    k_combos_without_reps,
    round_n,
    rounded_str,
    cartesian_product,
    group_by,
    modulus,
]

zip : List a, List b -> List (a, b)
zip = |a, b| List.map2(a, b, |x, y| (x, y))

expect zip [1, 2, 3] [4, 5, 6] == [(1, 4), (2, 5), (3, 6)]

unzip : List (a, b) -> (List a, List b)
unzip = |xs| unzip_loop xs [] []

unzip_loop : List (a, b), List a, List b -> (List a, List b)
unzip_loop = |xs, one, other|
    when xs is
        [] -> (one, other)
        [(first_one, first_other), .. as rest] ->
            unzip_loop rest (List.append one first_one) (List.append other first_other)

expect unzip [(1, 4), (2, 5), (3, 6)] == ([1, 2, 3], [4, 5, 6])
expect unzip [(1, 'a'), (2, 'b'), (3, 'c')] == ([1, 2, 3], ['a', 'b', 'c'])

window_by_2 : List a -> List (a, a)
window_by_2 = |xs|
    cs = List.drop_last xs 1
    ds = List.drop_first xs 1
    zip cs ds

expect window_by_2 [1, 2, 3, 4] == [(1, 2), (2, 3), (3, 4)]

unique : List a -> List a where a implements Hash & Eq
unique = |xs| xs |> Set.from_list |> Set.to_list

remove_consecutive_duplicates : List a -> List a where a implements Eq
remove_consecutive_duplicates = |xs|
    when xs is
        [] -> []
        [first, ..] ->
            List.walk xs [first] |acc, elem|
                if List.last acc == Ok elem then
                    acc
                else
                    List.append acc elem

unwrap : [Err *, Ok a] -> a
unwrap = |result|
    when result is
        Ok(x) -> x
        Err(_) -> crash "failed to unwrap value from Result"

msg_unwrap : [Err *, Ok a], Str -> a
msg_unwrap = |result, message|
    when result is
        Ok(x) -> x
        Err(_) -> crash(message)

Solution : {
    day : U64,
    input_str : Str,
    part1 : Str -> [Err Str, Ok U64],
    part2 : Str -> [Err Str, Ok U64],
    expected_part1 : U64,
    expected_part2 : U64,
}

blue : Str -> Str
blue = |str| "\u(001b)[0;34m${str}\u(001b)[0m"

green : Str -> Str
green = |str| "\u(001b)[38;5;46m${str}\u(001b)[0m"

red : Str -> Str
red = |str| "\u(001b)[0;31m${str}\u(001b)[0m"

yellow : Str -> Str
yellow = |str| "\u(001b)[38;5;226m${str}\u(001b)[0m"

orange : Str -> Str
orange = |str| "\u(001b)[38;5;208m${str}\u(001b)[0m"

k_combos_without_reps : List a, U64 -> List (List a)
k_combos_without_reps = |xs, k|
    if k == 0 or k > List.len xs then
        []
    else if k == 1 then
        List.map xs |x| [x]
    else
        List.walk_with_index xs [] |acc, elem, index|
            rest = List.drop_first xs (index + 1)
            sub_combos = k_combos_without_reps rest (k - 1)
            combos_with_elem = List.map sub_combos |sub| List.prepend sub elem
            List.concat acc combos_with_elem

expect k_combos_without_reps([1, 2, 3], 2) == [[1, 2], [1, 3], [2, 3]]
expect k_combos_without_reps([1, 2, 3, 4], 3) == [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]

round_n : Dec, Int a -> Dec
round_n = |num, places|
    pow_places = Num.pow_int 10 places |> Num.to_frac
    Num.round (num * pow_places) |> Num.to_frac |> |n| n / pow_places

expect round_n 123.45678 2 == 123.46

rounded_str : Dec, U64 -> Str
rounded_str = |num, places|
    rounded = round_n num places
    str = Num.to_str rounded
    parts = Str.split_on str "."
    when parts is
        [whole, fraction] ->
            xs = Str.to_utf8 fraction
            if
                List.len xs < places
            then
                "${whole}.${fraction}0"
            else
                frac = xs |> List.take_first places |> Str.from_utf8 |> unwrap
                "${whole}.${frac}"

        [whole] -> "${whole}.00"
        _ -> str

expect rounded_str 123.456789 2 == "123.46"
expect rounded_str 123.456789 3 == "123.457"
expect rounded_str 123.456789 4 == "123.4568"

cartesian_product : List a, List b -> List (a, b)
cartesian_product = |xs, ys|
    xs
    |> List.map |x|
        ys |> List.map |y| (x, y)
    |> List.join

expect cartesian_product [1, 2] [1, 2] == [(1, 1), (1, 2), (2, 1), (2, 2)]

group_by : List (Num a, b) -> Result (List (Num a, List b)) [ListWasEmpty] where a implements Hash & Eq & Inspect, b implements Inspect
group_by = |pairs|
    first_key = (List.first pairs)?.0
    sorted_pairs = List.sort_with pairs |(k1, _), (k2, _)| Num.compare k1 k2
    initial_state = { indexes: [], idx: 0u64, key: first_key }
    final_state = List.walk_with_index sorted_pairs initial_state |state, (key, _v), idx|
        when state.key is
            state_key if state_key == key -> state
            _ ->
                indexes = List.append state.indexes { k: state.key, start: state.idx, len: idx - state.idx }
                { indexes, idx, key }

    # add last chunk info and create groups
    len = List.len sorted_pairs - final_state.idx
    cs = List.append final_state.indexes { k: final_state.key, start: final_state.idx, len }
    groups = List.map cs |c|
        ls = List.sublist sorted_pairs { start: c.start, len: c.len } |> List.map |(_, pos)| pos
        (c.k, ls)
    Ok groups

expect
    got = group_by [('a', 1), ('b', 5), ('c', 7), ('a', 2), ('a', 3), ('b', 6), ('c', 8), ('c', 9)]
    expected = Ok [('a', [1, 2, 3]), ('b', [5, 6]), ('c', [7, 8, 9])]
    got == expected

modulus : Int a, Int a -> Int a
modulus = |a, b|
    remainder = Num.rem a b
    if remainder < 0 then remainder + Num.abs b else remainder
