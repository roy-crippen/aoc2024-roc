module [solution_day_06]

import Util exposing [Solution]
import "../data/day_06.txt" as input_str : Str

Pos : { row : U16, col : U16 }
Dir : [Up, Down, Left, Right, Done]
State : { pos : Pos, dir : Dir, rows : U16, cols : U16 }

solution_day_06 : Solution
solution_day_06 = {
    day: 6,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 5329

expected_part2 : U64
expected_part2 = 2162

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (initial_state, guards) =
        in_str
        |> parse
        |> Result.map_err |_e| "failed to parse input string"
        |> try

    visits = go_part1(initial_state, guards, Set.empty({}))
    Ok (Set.len visits)

expect part1 example_str == Ok 41
expect part1 input_str == Ok expected_part1

go_part1 : State, Set Pos, Set Pos -> Set Pos
go_part1 = |curr_state, guard_set, visit_set|
    next_state = get_next_state curr_state
    when next_state is
        None -> visit_set |> Set.remove curr_state.pos
        Some state ->
            if
                Set.contains(guard_set, state.pos)
            then
                next_dir = get_next_dir state.dir
                state1 = { state & pos: curr_state.pos, dir: next_dir }
                go_part1(state1, guard_set, visit_set)
            else
                visit_set1 = Set.insert(visit_set, state.pos)
                go_part1(state, guard_set, visit_set1)

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    go : State, Set Pos, Set (Pos, Dir) -> Bool
    go = |curr_state, guard_set, visit_set|
        next_state = get_next_state curr_state
        when next_state is
            None -> Bool.false
            Some state ->
                if
                    Set.contains(guard_set, state.pos)
                then
                    next_dir = get_next_dir state.dir
                    state1 = { state & pos: curr_state.pos, dir: next_dir }
                    go(state1, guard_set, visit_set)
                else
                    v = (state.pos, curr_state.dir)
                    if
                        Set.contains(visit_set, v)
                    then
                        Bool.true
                    else
                        visit_set1 = Set.insert(visit_set, v)
                        go(state, guard_set, visit_set1)

    (initial_state, guards) =
        in_str
        |> parse
        |> Result.map_err |_e| "failed to parse input string"
        |> try

    candidates = go_part1(initial_state, guards, Set.empty({}))

    loop_count =
        candidates
        |> Set.walk 0 |acc, v|
            guards1 = Set.insert guards v
            if go(initial_state, guards1, Set.empty({})) then acc + 1 else acc

    Ok loop_count

expect part2 example_str == Ok 6
# expect part2 input_str == Ok expected_part2

get_next_state : State -> [Some State, None]
get_next_state = |state|
    when state.dir is
        Up -> if state.pos.row > 0 then Some { state & pos: dec_row(state.pos) } else None
        Down -> if state.pos.row < state.rows then Some { state & pos: inc_row(state.pos) } else None
        Left -> if state.pos.col > 0 then Some { state & pos: dec_col(state.pos) } else None
        Right -> if state.pos.col < state.cols then Some { state & pos: inc_col(state.pos) } else None
        _ -> None

get_next_dir : Dir -> Dir
get_next_dir = |dir|
    when dir is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
        Done -> Done

inc_row : Pos -> Pos
inc_row = |pos| { pos & row: pos.row + 1 }

dec_row : Pos -> Pos
dec_row = |pos| { pos & row: pos.row - 1 }

inc_col : Pos -> Pos
inc_col = |pos| { pos & col: pos.col + 1 }

dec_col : Pos -> Pos
dec_col = |pos| { pos & col: pos.col - 1 }

parse : Str -> [Ok (State, Set Pos), Err [InvalidNumStr, OutOfBounds, NotFound]]
parse = |in_str|
    # get a list of list of position with char
    xss =
        in_str
        |> Str.trim
        |> Str.split_on "\n"
        |> List.map_with_index |line, r|
            Str.to_utf8 line
            |> List.map_with_index |ch, c|
                row = r |> Num.to_u16
                col = c |> Num.to_u16
                { pos: { row, col }, ch }

    rows = List.len xss |> Num.to_u16
    cols = List.get(xss, 0)? |> List.len |> Num.to_u16

    # keep just the guard positions and the start position
    ls =
        xss
        |> List.join
        |> List.keep_if |v| v.ch != '.'

    # get the start position
    start_idx = (ls |> List.find_first_index |v| v.ch == '^')?
    pos = List.get(ls, start_idx)?.pos

    # get the guard set
    guard_set =
        ls
        |> List.drop_at start_idx
        |> List.map |v| v.pos
        |> Set.from_list

    state = { pos, dir: Up, rows, cols }
    Ok (state, guard_set)

expect
    expected_state = { pos: { row: 6, col: 4 }, dir: Up, rows: 10, cols: 10 }
    when example_str |> parse is
        Ok (state, _guard_set) -> expected_state == state
        _ -> Bool.true

example_str : Str
example_str =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

