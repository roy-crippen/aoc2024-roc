module [solution_day_15]

import Util exposing [Solution]
import Gr exposing [Grid, Pos, Dir]
import "../data/day_15.txt" as input_str : Str

Element : [Robot, Wall, Box, BoxS, BoxE, Empty]

ch_to_element : U8 -> Element
ch_to_element = |ch|
    when ch is
        '@' -> Robot
        '#' -> Wall
        'O' -> Box
        '[' -> BoxS
        ']' -> BoxE
        '.' -> Empty
        _ -> crash "invalid element character"

ch_to_dir : U8 -> Dir
ch_to_dir = |ch|
    when ch is
        '^' -> N
        'v' -> S
        '<' -> W
        '>' -> E
        _ -> crash "invalid direction character"

parse : Str -> (Grid Element, List Dir)
parse = |s|
    when Str.split_on s "\n\n" is
        [gr_str, movement_str] ->
            ls = gr_str |> Str.split_on "\n" |> List.map Str.to_utf8
            rows = List.len ls
            cols = List.first ls |> Util.unwrap |> List.len
            data = List.join ls |> List.map ch_to_element
            g = { data, rows, cols }

            movements = movement_str |> Str.replace_each "\n" "" |> Str.to_utf8 |> List.map ch_to_dir
            (g, movements)

        _ -> crash "bad input data for day 15"

find_robot_pos : Grid Element -> U64
find_robot_pos = |g| g.data |> List.find_first_index (|e| e == Robot) |> Util.unwrap

build_block : Grid Element, Pos, Dir -> (Bool, Set Pos)
build_block = |g, pos, dir|
    go : Set Pos, Pos -> (Bool, Set Pos)
    go = |pos_set, p|
        when Gr.get_unsafe g p is
            Wall -> (Bool.false, pos_set)
            Empty -> (Bool.true, pos_set)
            BoxS if dir == N or dir == S ->
                # move the left box
                (left_box_moved, set0) = go(Set.insert pos_set p, Gr.move_unsafe g p dir)
                if left_box_moved then
                    # move the right box
                    pos_east = Gr.move_unsafe g p E
                    go(Set.insert set0 pos_east, Gr.move_unsafe g pos_east dir)
                else
                    # move not possible
                    (Bool.false, pos_set)

            BoxE if dir == N or dir == S ->
                # move the right box
                (right_box_moved, set0) = go(Set.insert pos_set p, Gr.move_unsafe g p dir)
                if right_box_moved then
                    # move the left box
                    pos_west = Gr.move_unsafe g p W
                    go(Set.insert set0 pos_west, Gr.move_unsafe g pos_west dir)
                else
                    # move not possible
                    (Bool.false, pos_set)

            Box | BoxS | BoxE -> go(Set.insert pos_set p, Gr.move_unsafe g p dir)
            Robot -> crash "found another robot"

    go Set.insert(Set.with_capacity 20, pos) pos

sort_pair_asc : List (U64, U64) -> List (U64, U64)
sort_pair_asc = |pairs| List.sort_with pairs |(k1, _), (k2, _)| Num.compare k1 k2

sort_pair_desc : List (U64, U64) -> List (U64, U64)
sort_pair_desc = |pairs| List.sort_with pairs |(k1, _), (k2, _)| Num.compare k2 k1

move_block : Grid Element, List Pos, Dir -> Grid Element
move_block = |g, ps, dir|
    ordered_ps =
        when dir is
            N -> List.map ps (|p| (Gr.pos_row g p, p)) |> sort_pair_asc
            S -> List.map ps (|p| (Gr.pos_row g p, p)) |> sort_pair_desc
            W -> List.map ps (|p| (Gr.pos_col g p, p)) |> sort_pair_asc
            E -> List.map ps (|p| (Gr.pos_col g p, p)) |> sort_pair_desc
            _ -> crash "invalid direction"
    List.walk ordered_ps g |g_acc, (_, p)| Gr.swap g_acc p (Gr.move_unsafe g p dir)

move_robot : Grid Element, Pos, Dir -> (Grid Element, Pos)
move_robot = |g, robot_pos, dir|
    next_robot_pos = Gr.move_unsafe g robot_pos dir
    when Gr.get_unsafe g next_robot_pos is
        Wall -> (g, robot_pos)
        Box | BoxS | BoxE ->
            when build_block g next_robot_pos dir is
                (b, move_set) if b ->
                    moves = Set.insert move_set robot_pos |> Set.to_list
                    new_grid = move_block g moves dir
                    (new_grid, next_robot_pos)

                _ ->
                    (g, robot_pos)

        Empty -> (Gr.swap g robot_pos next_robot_pos, next_robot_pos)
        Robot -> crash "found another robot"

apply_movements : (Grid Element, List Dir) -> Grid Element
apply_movements = |(g, ms)|
    robot_pos = find_robot_pos g
    (final_grid, _final_robot_pos) = List.walk ms (g, robot_pos) |(grid, pos), m| move_robot grid pos m
    final_grid

score_boxes : Grid Element -> U64
score_boxes = |g|
    Gr.find_positions g (|element| element == Box or element == BoxS)
    |> List.walk 0 |acc, pos| acc + 100 * Gr.pos_row g pos + (Gr.pos_col g pos)

expand_grid : Grid Element -> Grid Element
expand_grid = |g|
    data =
        List.map g.data |el|
            when el is
                Wall -> [Wall, Wall]
                Box -> [BoxS, BoxE]
                Empty -> [Empty, Empty]
                Robot -> [Robot, Empty]
                _ -> crash "bad input grid to expand_grid"
        |> List.join
    { data, rows: g.rows, cols: g.cols * 2 }

solution_day_15 : Solution
solution_day_15 = {
    day: 15,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 1441031

expected_part2 : U64
expected_part2 = 1425169

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (g, movements) = parse in_str
    apply_movements (g, movements) |> score_boxes |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (g, movements) = parse in_str
    apply_movements (expand_grid g, movements) |> score_boxes |> Ok

example_str : Str
example_str =
    """
    ##########
    #..O..O.O#
    #......O.#
    #.OO..O.O#
    #..O@..O.#
    #O#..O...#
    #O..O..O.#
    #.OO.O.OO#
    #....O...#
    ##########

    <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
    vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
    ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
    <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
    ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
    ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
    >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
    <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
    ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
    v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^

    """

# tests

expect part1 example_str == Ok 10092
expect sort_pair_asc [(3, 8), (2, 7), (4, 8), (1, 7)] == [(1, 7), (2, 7), (3, 8), (4, 8)]
expect sort_pair_desc [(3, 8), (2, 7), (4, 8), (1, 7)] == [(4, 8), (3, 8), (2, 7), (1, 7)]
expect part1 input_str == Ok expected_part1
expect part2 example_str == Ok 9021
expect part2 input_str == Ok expected_part2
