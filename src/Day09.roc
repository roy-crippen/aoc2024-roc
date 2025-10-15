module [solution_day_09]

import Util exposing [Solution]
import "../data/day_09.txt" as input_str : Str

Hole : { len : U32, start_idx : U64 }
File : { len : U32, start_idx : U64, val : I16 }

parse : Str -> List I16
parse = |in_str|
    in_str
    |> Str.to_utf8
    |> List.map |u| (Num.to_i16 u) - 48

expand_disk : List I16 -> List I16
expand_disk = |inputs_|
    go : List I16, I16, I16, List I16 -> List I16
    go = |inputs, idx, id, results|
        when inputs is
            [] -> results
            [v, .. as rest] ->
                cnt = Num.to_u64 v
                if Num.is_even idx then
                    go(rest, idx + 1, id + 1, List.concat results (List.repeat id cnt))
                else
                    go(rest, idx + 1, id, List.concat results (List.repeat -1 cnt))

    go inputs_ 0 0 []

compress_part1 : List I16 -> [Err [NotFound, OutOfBounds], Ok (List I16)]
compress_part1 = |fs|
    go : List I16, List U64, List U64, U64 -> [Err [NotFound, OutOfBounds], Ok (List I16)]
    go = |ls, rem_holes, rem_avail, remaining|
        if remaining == 0 then
            Ok ls
        else
            when (List.get rem_holes 0, List.get rem_avail 0) is
                (Ok hole_idx, Ok file_idx) ->
                    ls1 = List.swap ls hole_idx file_idx
                    go ls1 (List.drop_first rem_holes 1) (List.drop_first rem_avail 1) (remaining - 1)

                _ ->
                    Err NotFound

    holes = get_hole_idxs fs
    hole_cnt = List.len holes

    # Compute rem: files to move to clear right end (matches original stop after these swaps)
    right_end = List.take_last fs hole_cnt
    rem = List.keep_if right_end (|v| v != -1) |> List.len

    # Leftmost rem holes to fill
    remaining_holes = List.take_first holes rem

    # Rightmost rem files (first rem in reversed all-files)
    all_file_idxs =
        fs
        |> List.walk_with_index [] |acc, v, idx| if v != -1 then List.append acc idx else acc
        |> List.reverse
    avail = List.take_first all_file_idxs rem
    if rem > List.len avail then
        Err OutOfBounds # Not enough right files
    else
        go fs remaining_holes avail rem

get_hole_idxs : List I16 -> List U64
get_hole_idxs = |xs| xs |> List.walk_with_index [] |ls, x, idx| if x == -1 then List.append(ls, idx) else ls

check_sum : List I16 -> U64
check_sum = |fs| fs |> List.walk_with_index(0, |acc, v, idx| if v > 0 then acc + (Num.to_u64 v) * idx else acc)

compress_part2 : List I16 -> [Err [NotFound, OutOfBounds], Ok (List I16)]
compress_part2 = |xs|
    go = |xs0, holes0, files0|
        when files0 is
            [] -> Ok xs0
            [file, .. as rest_files] ->
                when find_next_hole_idx(holes0, file) is
                    Ok hole_idx ->
                        hole = List.get(holes0, hole_idx)?
                        (xs1, updated_hole) = fill_hole(xs0, hole, file)
                        holes1 =
                            if updated_hole.len > 0 then
                                List.set(holes0, hole_idx, updated_hole)
                            else
                                List.drop_at(holes0, hole_idx)
                        go xs1 holes1 rest_files

                    _ -> go xs0 holes0 rest_files

    holes = get_hole_groups xs
    files = List.reverse (get_file_groups xs)
    go xs holes files

get_hole_groups : List I16 -> List Hole
get_hole_groups = |xs|
    xs
    |> List.walk_with_index ([], None) |(holes, prev_hole_opt), v, idx|
        if v == -1 then
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

get_file_groups : List I16 -> List File
get_file_groups = |xs|

    (result_files, last_file_opt) = List.walk_with_index xs ([], None) |(files, prev_file_opt), v, idx|
        if
            v != -1
        then
            when prev_file_opt is
                Some prev_file ->
                    if prev_file.val == v then
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

find_next_hole_idx : List Hole, File -> Result U64 [NotFound]
find_next_hole_idx = |holes, file|
    holes |> List.find_first_index |hole| hole.len >= file.len and hole.start_idx < file.start_idx

fill_hole : List I16, Hole, File -> (List I16, Hole)
fill_hole = |xs, hole, file|
    go : List I16, Hole, File, U32 -> (List I16, Hole)
    go = |xs0, hole0, file0, cnt|
        when cnt is
            0 -> (xs0, hole0)
            _ ->
                xs1 = List.swap xs0 file0.start_idx hole0.start_idx
                hole1 = { len: hole0.len - 1, start_idx: hole0.start_idx + 1 }
                file1 = { len: file0.len - 1, start_idx: file0.start_idx + 1, val: file0.val }
                go xs1 hole1 file1 (cnt - 1)

    go xs hole file file.len

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

example_str_1 : Str
example_str_1 = "2333133121414131402"

# tests

expect part1 example_str_1 == Ok 1928
expect part1 input_str == Ok expected_part1
expect part2 example_str_1 == Ok 2858
# expect part2 input_str == Ok expected_part2

