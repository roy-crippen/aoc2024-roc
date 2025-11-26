module [solution_day_18]

import Bool exposing [false, true]
import Util exposing [Solution]
import "../data/day_18.txt" as input_str : Str
import Structures.Grid as Gr
import Structures.Deque as Dq

State : { q : Dq.Deque (I32, I32, I32), seen : Set (I32, I32), done : Bool }

parse : Str -> List Gr.RC
parse = |s|
    Str.split_on(s, "\n")
    |> List.map |str|
        when Str.split_on(str, ",") is
            [s1, s2] -> (Str.to_u64(s2) |> Util.unwrap, Str.to_u64(s1) |> Util.unwrap)
            _ -> crash("day18 parse failed")

run : Gr.Grid U8 -> (I32, I32, U64, Bool)
run = |g|
    go : State -> State
    go = |st_in|
        if Dq.is_empty(st_in.q) then
            { st_in & done: false }
        else
            ((r, c, d), temp_q) = Dq.pop_back(st_in.q) |> Util.unwrap
            st = { st_in & q: temp_q }

            ls : List (I32, I32)
            ls =
                [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
                |> List.keep_if(|(row, col)| row > -1 and col > -1 and row < rows and col < cols)

            state = List.walk_until ls st |acc, (nr, nc)|
                if Set.contains(acc.seen, (nr, nc)) then
                    Continue acc
                else if Gr.get_unsafe(g, Gr.rc_to_pos(g, (Num.to_u64(nr), Num.to_u64(nc)))) == 1 then
                    Continue acc
                else if nr == rows - 1 and nc == cols - 1 then
                    Break { acc & q: Dq.push_front(acc.q, (nr, nc, d + 1)), done: true }
                else
                    seen1 = Set.insert(acc.seen, (nr, nc))
                    q1 = Dq.push_front(acc.q, (nr, nc, d + 1))
                    Continue { q: q1, seen: seen1, done: false }

            if state.done then state else go(state)

    rows = g.rows |> Num.to_i32
    cols = g.cols |> Num.to_i32
    q = Dq.from_list([(0, 0, 0)])
    seen = Set.single((0, 0))
    st_out = go({ q, seen, done: false })
    if st_out.done then
        ((r1, c1, distance), _q) = Dq.pop_front(st_out.q) |> Util.unwrap
        (r1, c1, distance |> Num.to_u64, st_out.done)
    else
        (0, 0, 0, false)

find_block : Gr.Grid U8, U64, List Gr.RC, U64 -> Gr.RC
find_block = |g, cols, rcs, floor_idx|
    go : U64, U64 -> (Gr.RC, Bool)
    go = |start_idx, end_idx|
        if start_idx + 1 >= end_idx then
            rc_ = List.get(rcs, start_idx) |> Util.unwrap
            (rc_, true)
        else
            mid_idx = (start_idx + end_idx) // 2
            xs = List.take_first(rcs, mid_idx)
            data = List.walk xs g.data |acc, (r, c)| List.set(acc, r * cols + c, 1)
            (_row, _col, _dist, completed) = run({ g & data })

            if completed then go(mid_idx, end_idx) else go(start_idx, mid_idx)

    (rc, _completed) = go(floor_idx, List.len(rcs) - 1)
    rc

solution_day_18 : Solution
solution_day_18 = {
    day: 18,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 276

expected_part2 : U64
expected_part2 = 2220 # c,r = 60,37, val = 60*37 =

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    # for example
    # take_n = 12
    # rows = 7
    # cols = 7

    take_n = 1024
    rows = 71
    cols = 71

    temp_g : Gr.Grid U8
    temp_g = Gr.make(rows, cols, 0)
    rcs = parse(in_str) |> List.take_first(take_n)
    new_data = List.walk rcs temp_g.data |acc, (r, c)| List.set(acc, r * cols + c, 1)
    g = { temp_g & data: new_data }

    (_row, _col, dist, _completed) = run(g)
    Ok dist

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    # for example
    # safe_n = 12
    # rows = 7
    # cols = 7

    safe_n = 1024
    rows = 71
    cols = 71

    g : Gr.Grid U8
    g = Gr.make(rows, cols, 0)
    rcs = parse(in_str)
    rc = find_block(g, cols, rcs, safe_n - 1)
    Ok (rc.0 * rc.1)

# example_str : Str
# example_str =
#    """
#    5,4
#    4,2
#    4,5
#    3,0
#    2,1
#    6,3
#    2,4
#    1,5
#    0,6
#    3,3
#    2,6
#    5,1
#    1,2
#    5,5
#    2,5
#    6,5
#    1,4
#    0,4
#    6,4
#    1,1
#    6,1
#    1,0
#    0,5
#    1,6
#    2,0
#    """

# tests

# expect part1 example_str |> dbg == Ok 22
# expect part2 example_str |> dbg == Ok 6
