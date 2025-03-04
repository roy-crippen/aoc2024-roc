module [solution_day_06]

import Util exposing [Solution]
import "../data/day_06.txt" as input_str : Str

#     (row, col)
Pos : (U16, U16)
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
    (initial_st, guards) =
        in_str
        |> parse
        |> Result.map_err |_e| "failed to parse input string"
        |> try

    empty_list = List.with_capacity 7000
    xs = part1_loop(guards, initial_st, empty_list)
    cnt = xs |> Set.from_list |> Set.len
    Ok cnt

expect part1 example_str |> dbg == Ok 41
expect part1 input_str == Ok expected_part1

part1_loop : Set Pos, State, List Pos -> List Pos
part1_loop = |guards, curr_st, xs|
    (maybe_st, new_xs) = skip_to_next_state1(guards, curr_st, xs)
    when maybe_st is
        None -> new_xs
        Some(new_st) -> part1_loop(guards, new_st, new_xs)

skip_to_next_state1 : Set Pos, State, List Pos -> ([Some State, None], List Pos)
skip_to_next_state1 = |guards, st, xs|
    when st.dir is
        Up -> next_up1(guards, st, xs)
        Down -> next_down1(guards, st, xs)
        Left -> next_left1(guards, st, xs)
        Right -> next_right1(guards, st, xs)
        _ -> (None, xs)

next_up1 : Set Pos, State, List Pos -> ([Some State, None], List Pos)
next_up1 = |guards, st, xs|
    when st.pos.0 is
        0 -> (None, xs)
        _ ->
            when dec_row st.pos is
                pos if Set.contains(guards, pos) -> (Some { st & dir: Right }, xs)
                pos -> next_up1(guards, { st & pos }, List.append(xs, pos))

next_down1 : Set Pos, State, List Pos -> ([Some State, None], List Pos)
next_down1 = |guards, st, xs|
    when st.pos.0 + 1 is
        r if r == st.rows -> (None, xs)
        _ ->
            when inc_row st.pos is
                pos if Set.contains(guards, pos) -> (Some { st & dir: Left }, xs)
                pos -> next_down1(guards, { st & pos }, List.append(xs, pos))

next_left1 : Set Pos, State, List Pos -> ([Some State, None], List Pos)
next_left1 = |guards, st, xs|
    when st.pos.1 is
        0 -> (None, xs)
        _ ->
            when dec_col st.pos is
                pos if Set.contains(guards, pos) -> (Some { st & dir: Up }, xs)
                pos -> next_left1(guards, { st & pos }, List.append(xs, pos))

next_right1 : Set Pos, State, List Pos -> ([Some State, None], List Pos)
next_right1 = |guards, st, xs|
    when st.pos.1 + 1 is
        c if c == st.cols -> (None, xs)
        _ ->
            when inc_col st.pos is
                pos if Set.contains(guards, pos) -> (Some { st & dir: Down }, xs)
                pos -> next_right1(guards, { st & pos }, List.append(xs, pos))

# ********* part 2 *********
part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (initial_st, guards) =
        in_str
        |> parse
        |> Result.map_err |_e| "failed to parse input string"
        |> try

    empty_list = List.with_capacity 7000
    candidates =
        part1_loop(guards, initial_st, empty_list)
        |> Set.from_list

    empty_set = Set.with_capacity 700
    loop_count =
        candidates
        |> Set.walk 0 |acc, pos|
            guards1 = Set.insert guards pos
            if part2_loop(guards1, initial_st, empty_set, 0) then acc + 1 else acc

    Ok loop_count

expect part2 example_str |> dbg == Ok 6
## expect part2 input_str == Ok expected_part2

part2_loop : Set Pos, State, Set (Pos, Dir), U64 -> Bool
part2_loop = |guards, curr_st, vs, cnt|
    maybe_st = skip_to_next_state2(guards, curr_st)
    when maybe_st is
        None -> Bool.false
        Some(new_st) ->
            # if we are here then we ran into a guard
            if
                Set.contains(vs, (new_st.pos, new_st.dir))
            then
                Bool.true
            else
                # at a guard so add to visits set
                new_vs = Set.insert(vs, (new_st.pos, new_st.dir))
                part2_loop(guards, new_st, new_vs, cnt + 1)

skip_to_next_state2 : Set Pos, State -> [Some State, None]
skip_to_next_state2 = |guards, st|
    when st.dir is
        Up -> next_up2(guards, st)
        Down -> next_down2(guards, st)
        Left -> next_left2(guards, st)
        Right -> next_right2(guards, st)
        _ -> None

next_up2 : Set Pos, State -> [Some State, None]
next_up2 = |guards, st|
    when st.pos.0 is
        0 -> None
        _ ->
            when dec_row st.pos is
                pos if Set.contains(guards, pos) -> Some { st & dir: Right }
                pos -> next_up2(guards, { st & pos })

next_down2 : Set Pos, State -> [Some State, None]
next_down2 = |guards, st|
    when st.pos.0 + 1 is
        r if r == st.rows -> None
        _ ->
            when inc_row st.pos is
                pos if Set.contains(guards, pos) -> Some { st & dir: Left }
                pos -> next_down2(guards, { st & pos })

next_left2 : Set Pos, State -> [Some State, None]
next_left2 = |guards, st|
    when st.pos.1 is
        0 -> None
        _ ->
            when dec_col st.pos is
                pos if Set.contains(guards, pos) -> Some { st & dir: Up }
                pos -> next_left2(guards, { st & pos })

next_right2 : Set Pos, State -> [Some State, None]
next_right2 = |guards, st|
    when st.pos.1 + 1 is
        c if c == st.cols -> None
        _ ->
            when inc_col st.pos is
                pos if Set.contains(guards, pos) -> Some { st & dir: Down }
                pos -> next_right2(guards, { st & pos })

inc_row : Pos -> Pos
inc_row = |(r, c)| (r + 1, c)

dec_row : Pos -> Pos
dec_row = |(r, c)| (r - 1, c)

inc_col : Pos -> Pos
inc_col = |(r, c)| (r, c + 1)

dec_col : Pos -> Pos
dec_col = |(r, c)| (r, c - 1)

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
                { pos: (row, col), ch }

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
    expected_state = { pos: (6, 4), dir: Up, rows: 10, cols: 10 }
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
