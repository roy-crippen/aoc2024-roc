module [solution_day_15]

import Util exposing [Solution]
import Gr exposing [Grid, Pos, Dir]
import "../data/day_15.txt" as input_str : Str

Element : [Robot, Wall, Box, StartBox, EndBox, Empty]

ch_to_element : U8 -> Element
ch_to_element = |ch|
    when ch is
        '@' -> Robot
        '#' -> Wall
        'O' -> Box
        '[' -> StartBox
        ']' -> EndBox
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
            # grid
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
        # dbg (dir, p, pos_set, Gr.get_unsafe g p)
        when Gr.get_unsafe g p is
            Wall -> (Bool.false, pos_set)
            Empty -> (Bool.true, pos_set)
            Box -> go(Set.insert pos_set p, Gr.move_unsafe g p dir)
            Robot | StartBox | EndBox -> crash "found another robot"

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
    # dbg (dir, ordered_ps)

    new_grid = List.walk ordered_ps g |g_acc, (_, p)|
        Gr.swap g_acc p (Gr.move_unsafe g p dir)

    new_grid

move_robot : Grid Element, Pos, Dir -> (Grid Element, Pos)
move_robot = |g, robot_pos, dir|
    # dbg dir
    next_robot_pos = Gr.move_unsafe g robot_pos dir

    when Gr.get_unsafe g next_robot_pos is
        Wall -> (g, robot_pos)
        Box | StartBox | EndBox ->
            when build_block g next_robot_pos dir is
                (b, move_set) if b ->
                    moves = Set.insert move_set robot_pos |> Set.to_list
                    new_grid = move_block g moves dir
                    (new_grid, next_robot_pos)

                _ ->
                    (g, robot_pos)

        # (g, next_robot_pos)
        Empty -> (Gr.swap g robot_pos next_robot_pos, next_robot_pos)
        Robot -> crash "found another robot"

apply_movements : (Grid Element, List Dir) -> Grid Element
apply_movements = |(g, ms)|
    robot_pos = find_robot_pos g
    (final_grid, _final_robot_pos) = List.walk ms (g, robot_pos) |(grid, pos), m|
        (g_, p_) = move_robot grid pos m
        # dbg m
        # dbg (Gr.show g_ 6 |> Str.replace_each "Empty" "     ")
        (g_, p_)

    final_grid

score_boxes : Grid Element -> U64
score_boxes = |g|
    ps = Gr.find_positions g |element| element == Box or element == StartBox
    aaa = List.walk ps 0 |acc, pos| acc + 100 * (Gr.pos_row g pos) + (Gr.pos_col g pos)
    aaa

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

    # dbg (Gr.show g 6 |> Str.replace_each "Empty" "     ")
    g1 = apply_movements (g, movements)
    # dbg (Gr.show g1 6 |> Str.replace_each "Empty" "     ")

    sum = score_boxes g1
    Ok sum

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

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
expect part1 input_str |> dbg == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2
