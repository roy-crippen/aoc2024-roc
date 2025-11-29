module [solution_day_06]

import Util exposing [Solution]
import Structures.Grid as Gr exposing [Grid, Dir, Pos]
import Bool exposing [not]
import "../data/day_06.txt" as input_str : Str

State : { pos : Pos, dir : Dir }
Status : [Guard, OutOfBounds, Running]

parse : Str -> (Grid U8, State)
parse = |s|
    ls = Str.split_on s "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.first ls |> Util.unwrap |> List.len
    data = List.join ls
    g = { data, rows, cols }
    pos = Gr.find_positions g (|ch| ch == '^') |> List.first |> Util.unwrap
    st = { pos, dir: N }
    (g, st)

next_dir : Dir -> Dir
next_dir = |dir|
    when dir is
        N -> E
        E -> S
        S -> W
        W -> N
        _ -> crash "invalid direction"

traverse_grid_part1 : Grid U8, State, List State -> List State
traverse_grid_part1 = |g, st, xs|
    go : State, List State, Status -> List State
    go = |state, ps, status|
        when status is
            Guard -> traverse_grid_part1(g, { state & dir: next_dir state.dir }, ps)
            OutOfBounds -> ps
            Running ->
                (new_state, new_status) = to_next g state
                ps1 = List.append ps new_state
                go new_state ps1 new_status

    go st xs Running

to_next : Grid U8, State -> (State, Status)
to_next = |g, st|
    new_pos = Gr.move st.pos st.dir
    is_inside = Gr.is_inside g.rows g.cols new_pos
    is_guard = is_inside and Gr.get_unsafe g new_pos == '#'
    when (is_inside, is_guard) is
        (b1, b2) if b1 and b2 -> (st, Guard)
        (b1, _) if not b1 -> (st, OutOfBounds)
        (b1, b2) if b1 and not b2 -> ({ st & pos: new_pos }, Running)
        _ -> crash "to_next failed"

dir_to_i32 : Dir -> I32
dir_to_i32 = |dir|
    when dir is
        N -> 0
        E -> 1
        S -> 2
        W -> 3
        _ -> crash "invalid direction"

# helper function for bit vector operations
get_bit : List U64, U64 -> Bool
get_bit = |visits, key|
    bucket = key // 64
    bit = key % 64
    val = List.get visits bucket |> Result.with_default 0u64
    shifted = Num.shift_right_by val (Num.to_u8 bit)
    masked = Num.bitwise_and shifted 1u64
    Bool.is_not_eq masked 0u64

# helper function for bit vector operations
set_bit : List U64, U64 -> List U64
set_bit = |visits, key|
    bucket = key // 64
    bit = key % 64
    val = List.get visits bucket |> Result.with_default 0u64
    bit_mask = Num.shift_left_by 1u64 (Num.to_u8 bit)
    new_val = Num.bitwise_or val bit_mask
    List.set visits bucket new_val

# create an index from a Pos and Dir
pos_dir_to_index : I32, Pos, Dir -> U64
pos_dir_to_index = |cols, (r, c), dir| ((r * cols + c) * 4 + dir_to_i32 dir) |> Num.to_u64

# traverse grid from state return true if path loops
is_loop : Grid U8, State, I32, List U64 -> Bool
is_loop = |g, st, i32_cols, visits|
    go : List U64, State, Status -> Bool
    go = |visits1, state, status|
        key = pos_dir_to_index i32_cols state.pos state.dir
        visited = get_bit visits1 key
        if visited then
            Bool.true
        else
            when status is
                Guard -> go(visits1, { state & dir: next_dir state.dir }, Running)
                OutOfBounds -> Bool.false
                Running ->
                    (next_state, next_status) = to_next g state
                    next_visits = if next_status == Running then set_bit visits1 key else visits1
                    go next_visits next_state next_status

    (init_st, init_status) = to_next g st
    go visits init_st init_status

solution_day_06 : Solution
solution_day_06 = {
    day: 06,
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
    (g, st) = parse in_str
    traverse_grid_part1 g st [st]
    |> List.map (|state| state.pos)
    |> Set.from_list
    |> Set.len
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (g, st) = parse in_str
    i32_cols = Num.to_i32 g.cols
    route =
        traverse_grid_part1 g st [st]
        |> List.map |st_| (Num.to_u64 (st_.pos.0 * i32_cols + st_.pos.1), st_.pos, st_.dir)

    init_idx_pos_dir = List.first route |> Util.unwrap
    init_is_cycle = Set.with_capacity (List.len route)
    init_pos_used = Set.with_capacity (List.len route)

    # Initialize bit vector: ceiling(67600 / 64) = 1057 U64s
    total_states = g.rows * g.cols * 4
    num_buckets = (total_states + 63) // 64
    init_visited = List.repeat 0u64 num_buckets

    (_final_state, cycle_positions, _used_positions) =
        List.walk
            route
            (init_idx_pos_dir, init_is_cycle, init_pos_used)
            (|(idx_pos_dir, is_cycle, pos_used), (next_idx, next_pos, next_direction)|
                (_prev_idx, prev_pos, prev_dir) = idx_pos_dir
                prev_state = { pos: prev_pos, dir: prev_dir }
                grid = Gr.set g next_pos '#'
                has_pos_been_used = Set.contains pos_used next_idx
                updated_is_cycle =
                    if
                        not has_pos_been_used and is_loop(grid, prev_state, i32_cols, init_visited)
                    then
                        Set.insert is_cycle next_idx
                    else
                        is_cycle
                ((next_idx, next_pos, next_direction), updated_is_cycle, Set.insert pos_used next_idx))

    Ok (Set.len cycle_positions)

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

# tests

expect part1 example_str == Ok 41
# expect part1 input_str == Ok expected_part1
expect part2 example_str == Ok 6
# expect part2 input_str |> dbg == Ok expected_part2 |> dbg
