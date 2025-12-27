module [solution_day_25]

import Util exposing [Solution, unwrap]
import "../data/day_25.txt" as input_str : Str

solution_day_25 : Solution
solution_day_25 = {
    day: 25,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 3264

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    blocks = Str.split_on(in_str, "\n\n")
    (locks, keys) =
        blocks
        |> List.map(
            |s|
                (cs, vs) =
                    Str.split_on(s, "\n")
                    |> List.map(Str.to_utf8)
                    |> Util.transpose_unsafe
                    |> List.map(
                        |zs|
                            cnt = zs |> List.count_if(|z| z == '#')
                            if List.get(zs, 0) |> unwrap == '#' then ("#", cnt - 1) else (".", cnt - 1),
                    )
                    |> Util.unzip
                (List.get(cs, 0) |> unwrap, vs),
        )
        |> List.walk(
            ([], []),
            |(acc_locks, acc_keys), (key, vs)|
                if key == "#" then (List.append(acc_locks, vs), acc_keys) else (acc_locks, List.append(acc_keys, vs)),
        )

    count =
        locks
        |> List.walk(
            0,
            |acc, lock|
                x = keys |> List.keep_if(|key| List.map2(lock, key, |l, v| l + v <= 5) |> List.all(|b| b)) |> List.len
                acc + x,
            # )
        )

    Ok count

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    #####
    .####
    .####
    .####
    .#.#.
    .#...
    .....

    #####
    ##.##
    .#.##
    ...##
    ...#.
    ...#.
    .....

    .....
    #....
    #....
    #...#
    #.#.#
    #.###
    #####

    .....
    .....
    #.#..
    ###..
    ###.#
    ###.#
    #####

    .....
    .....
    .....
    #....
    #.#..
    #.#.#
    #####
    """

# tests

expect part1 example_str == Ok 3
expect part1 input_str == Ok expected_part1
