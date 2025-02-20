module [solution_day_09]

import Util exposing [Solution]
import "../data/day_09.txt" as input_str : Str

Hole : { len : U32, start_idx : U64 }
File : { len : U32, start_idx : U64, val : I32 }

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
    |> compress_part1
    |> Result.map_err |_e| "failed to compress the disk"
    |> try
    |> check_sum
    |> Ok

expect part1 example_str_1 == Ok 1928
expect part1 input_str == Ok expected_part1

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    in_str
    |> parse
    |> expand_disk
    |> compress_part2
    |> Result.map_err |_e| "failed to compress the disk"
    |> try
    |> check_sum
    |> Ok

expect part2 example_str_1 == Ok 2858
expect part2 input_str == Ok expected_part2

parse : Str -> List I32
parse = |in_str|
    in_str
    |> Str.to_utf8
    |> List.map |u| (Num.to_i32 u) - 48

expand_disk : List I32 -> List I32
expand_disk = |inputs_|
    go : List I32, I32, I32, List I32 -> List I32
    go = |inputs, idx, id, results|
        when inputs is
            [] -> results
            [v, .. as rest] ->
                cnt = Num.to_u64 v
                if
                    Num.is_even idx
                then
                    go(rest, idx + 1, id + 1, List.concat results (List.repeat id cnt))
                else
                    go(rest, idx + 1, id, List.concat results (List.repeat -1 cnt))

    go inputs_ 0 0 []

compress_part1 : List I32 -> [Err [NotFound, OutOfBounds], Ok (List I32)]
compress_part1 = |fs|
    go : List I32, List U64, U64 -> [Err [NotFound, OutOfBounds], Ok (List I32)]
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

get_hole_idxs : List I32 -> List U64
get_hole_idxs = |xs| xs |> List.walk_with_index [] |ls, x, idx| if x == -1 then List.append(ls, idx) else ls

check_sum : List I32 -> U64
check_sum = |fs| fs |> List.walk_with_index(0, |acc, v, idx| if v > 0 then acc + (Num.to_u64 v) * idx else acc)

compress_part2 : List I32 -> [Err [NotFound, OutOfBounds], Ok (List I32)]
compress_part2 = |xs|
    go = |xs0, holes0, files0|
        when files0 is
            [] -> Ok xs0
            [file, .. as rest_files] ->
                when find_next_hole_idx(holes0, file) is
                    Ok hole_idx ->
                        hole = List.get(holes0, hole_idx)?
                        (xs1, updated_hole) = fill_hole(xs0, hole, file)
                        holes1 = List.set(holes0, hole_idx, updated_hole)
                        holes2 = remove_empty_hole_records(holes1)
                        go xs1 holes2 rest_files

                    _ -> go xs0 holes0 rest_files

    holes = get_hole_groups xs
    files = List.reverse (get_file_groups xs)
    go xs holes files

get_hole_groups : List I32 -> List Hole
get_hole_groups = |xs|
    xs
    |> List.walk_with_index ([], None) |(holes, prev_hole_opt), v, idx|
        if
            v == -1
        then
            when prev_hole_opt is
                Some prev_hole -> (holes, Some { len: prev_hole.len + 1, start_idx: prev_hole.start_idx })
                None -> (holes, Some { len: 1, start_idx: idx })
        else
            when prev_hole_opt is
                Some prev_hole ->
                    holes1 = List.append holes prev_hole
                    (holes1, None)

                None -> (holes, None)
    |> .0

get_file_groups : List I32 -> List File
get_file_groups = |xs|

    (result_files, last_file_opt) = List.walk_with_index xs ([], None) |(files, prev_file_opt), v, idx|
        if
            v != -1
        then
            when prev_file_opt is
                Some prev_file ->
                    if
                        prev_file.val == v
                    then
                        new_file = { len: prev_file.len + 1, start_idx: prev_file.start_idx, val: prev_file.val }
                        (files, Some new_file)
                    else
                        files1 = List.append files prev_file
                        (files1, Some { len: 1, start_idx: idx, val: v })

                None -> (files, Some { len: 1, start_idx: idx, val: v })
        else
            when prev_file_opt is
                Some prev_file ->
                    files1 = List.append files prev_file
                    (files1, None)

                None -> (files, None)

    when last_file_opt is
        Some last_file -> List.append result_files last_file
        None -> result_files

remove_empty_hole_records : List Hole -> List Hole
remove_empty_hole_records = |hole_recs| hole_recs |> List.keep_if |{ len }| len > 0

find_next_hole_idx : List Hole, File -> Result U64 [NotFound]
find_next_hole_idx = |holes, file|
    holes |> List.find_first_index |hole| hole.len >= file.len and hole.start_idx < file.start_idx

fill_hole : List I32, Hole, File -> (List I32, Hole)
fill_hole = |xs, hole, file|
    go : List I32, Hole, File, U32 -> (List I32, Hole)
    go = |xs0, hole0, file0, cnt|
        when cnt is
            0 -> (xs0, hole0)
            _ ->
                xs1 = List.swap xs0 file0.start_idx hole0.start_idx
                hole1 = { len: hole0.len - 1, start_idx: hole0.start_idx + 1 }
                file1 = { len: file0.len - 1, start_idx: file0.start_idx + 1, val: file0.val }
                go xs1 hole1 file1 (cnt - 1)

    go xs hole file file.len

expect
    xs0 = [0, 0, -1, -1, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4, 4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, 9, 9]
    xs1 = [0, 0, 9, 9, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4, 4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, -1, -1]
    expected = (xs1, { start_idx: 4, len: 1 })
    got = fill_hole xs0 { start_idx: 2, len: 3 } { len: 2, start_idx: 40, val: 9 }
    expected == got

expect
    holes = [{ start_idx: 2, len: 3 }, { start_idx: 8, len: 3 }]
    expected = Ok 0
    got = find_next_hole_idx(holes, { len: 2, start_idx: 40, val: 9 })
    expected == got

expect
    holes = [{ start_idx: 2, len: 3 }, { start_idx: 8, len: 3 }]
    expected = Err NotFound
    got = find_next_hole_idx(holes, { len: 4, start_idx: 36, val: 8 })
    expected == got

expect
    expected = [{ start_idx: 2, len: 3 }, { start_idx: 12, len: 3 }]
    got = [{ start_idx: 2, len: 3 }, { start_idx: 8, len: 0 }, { start_idx: 12, len: 3 }] |> remove_empty_hole_records
    expected == got

expect
    expected = [2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2]
    got = example_str_1 |> parse
    expected == got

expect
    expected = [0, 0, -1, -1, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4, 4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, 9, 9]
    got = example_str_1 |> parse |> expand_disk
    expected == got

expect
    expected = [
        { start_idx: 2, len: 3 },
        { start_idx: 8, len: 3 },
        { start_idx: 12, len: 3 },
        { start_idx: 18, len: 1 },
        { start_idx: 21, len: 1 },
        { start_idx: 26, len: 1 },
        { start_idx: 31, len: 1 },
        { start_idx: 35, len: 1 },
    ]
    got = example_str_1 |> parse |> expand_disk |> get_hole_groups
    expected == got

expect
    expected = [
        { len: 2, start_idx: 0, val: 0 },
        { len: 3, start_idx: 5, val: 1 },
        { len: 1, start_idx: 11, val: 2 },
        { len: 3, start_idx: 15, val: 3 },
        { len: 2, start_idx: 19, val: 4 },
        { len: 4, start_idx: 22, val: 5 },
        { len: 4, start_idx: 27, val: 6 },
        { len: 3, start_idx: 32, val: 7 },
        { len: 4, start_idx: 36, val: 8 },
        { len: 2, start_idx: 40, val: 9 },
    ]
    got = example_str_1 |> parse |> expand_disk |> get_file_groups
    expected == got

expect
    expected = 1928
    got = [0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1] |> check_sum
    expected == got

expect
    expected = [2, 3, 4, 8, 9, 10, 12, 13, 14, 18, 21, 26, 31, 35]
    got = example_str_1 |> parse |> expand_disk |> get_hole_idxs
    expected == got

example_str_1 : Str
example_str_1 = "2333133121414131402"
