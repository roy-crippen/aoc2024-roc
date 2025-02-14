module [solution_day_09]

import Util exposing [Solution]
import "../data/day_09.txt" as input_str : Str

solution_day_09 : Solution
solution_day_09 = {
    day: 9,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 6310675819476

expected_part2 : U64
expected_part2 = 6335972980679

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    in_str
    |> parse
    |> expand_disk
    |> compress
    |> Result.map_err |_e| "failed to compress the disk"
    |> try
    |> check_sum
    |> Ok

expect part1 example_str_1 == Ok 1928
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

# expect part2 example_str_1 == Ok 2858
# expect part2 input_str == Ok expected_part2

get_hole_idxs : List I32 -> List U64
get_hole_idxs = |xs| xs |> List.walk_with_index [] |ls, x, idx| if x == -1 then List.append(ls, idx) else ls

# get_files_reverse : List I32 -> List I32
# get_files_reverse = |xs|
#     xs
#     |> List.walk [] |ls, x| if x != -1 then List.append(ls, x) else ls
#     |> List.reverse

compress : List I32 -> [Err [NotFound, OutOfBounds], Ok (List I32)]
compress = |fs|
    go = |ls, hole_idxs, hole_cnt|
        if
            ls |> List.take_last hole_cnt |> List.all |v| v == -1
        then
            Ok ls
        else
            { before, others } = List.split_at hole_idxs 1
            hole_idx = List.get(before, 0)?
            file_idx = (List.find_last_index ls |v| v != -1)?
            ls1 = List.swap ls hole_idx file_idx
            go ls1 others hole_cnt

    holes = get_hole_idxs fs
    go fs holes (List.len holes)

check_sum : List I32 -> U64
check_sum = |fs| fs |> List.walk_with_index(0, |acc, v, idx| if v > 0 then acc + (Num.to_u64 v) * idx else acc)

# get_hole_cnt : List I32 -> U64
# get_hole_cnt = |fs| fs |> List.walk 0 |acc, v| if v == -1 then acc + 1 else acc

parse : Str -> List I32
parse = |in_str|
    in_str
    |> Str.to_utf8
    |> List.map |u| (Num.to_i32 u) - 48

expand_disk : List I32 -> List I32
expand_disk = |xs|
    xs
    |> List.walk_with_index(
        ([], 0),
        |(rs, id), cnt, idx|
            if
                Num.is_even idx
            then
                (List.concat rs (List.repeat id (Num.to_u64 cnt)), id + 1)
            else
                (List.concat rs (List.repeat -1 (Num.to_u64 cnt)), id),
    )
    |> .0

expand_disk_rec : List I32 -> List I32
expand_disk_rec = |inputs_|

    go : List I32, I32, I32, List I32 -> List I32
    go = |inputs, idx, id, results|
        when inputs is
            [] -> results
            [v, .. as rest] ->
                cnt = Num.to_u64 v
                if
                    Num.is_even idx
                then
                    go(
                        rest,
                        idx + 1,
                        id + 1,
                        List.concat results (List.repeat id cnt),
                    )
                else
                    go(
                        rest,
                        idx + 1,
                        id,
                        List.concat results (List.repeat -1 cnt),
                    )

    go inputs_ 0 0 []

expect
    expected = [2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2]
    got = example_str_1 |> parse
    expected == got

expect
    expected = [0, 0, -1, -1, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4, 4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, 9, 9]
    got = example_str_1 |> parse |> expand_disk
    expected == got

expect
    expected = [0, 0, -1, -1, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4, 4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, 9, 9]
    got = example_str_1 |> parse |> expand_disk_rec
    expected == got

expect
    expected = 1928
    got = [0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] |> check_sum
    expected == got

expect
    expected = [2, 3, 4, 8, 9, 10, 12, 13, 14, 18, 21, 26, 31, 35]
    got = example_str_1 |> parse |> expand_disk |> get_hole_idxs |> dbg
    expected == got

# expect
#     expected = [9, 9, 8, 8, 8, 8, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4, 3, 3, 3, 2, 1, 1, 1, 0, 0]
#     got = example_str_1 |> parse |> expand_disk |> get_files_reverse |> dbg
#     expected == got

example_str_1 : Str
example_str_1 = "2333133121414131402"
