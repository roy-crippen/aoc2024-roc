module [
    Grid,
    Pos,
    RC,
    Dir,
    make,
    show,
    show_char,
    is_inside,
    get,
    get_unsafe,
    set,
    swap,
    find_positions,
    pos_row,
    pos_col,
    pos_to_rc,
    rc_to_pos,
    move,
    move_unsafe,
    neighbors4,
    # north,
    # north_west,
    # west,
    # south_west,
    # south,
    # south_east,
    # east,
    # north_east,
    # neighbors4,
    # neighbor_values4,
    # neighbor_values8_tup,
    # apply4,
    # apply8,
]

import Util

Grid a : { data : List a, rows : U64, cols : U64 } where a implements Inspect
Pos : U64
RC : (U64, U64)
Dir : [N, NW, W, SW, S, SE, E, NE]

# returns a Grid of rows by cols all with values v
make : U64, U64, a -> Grid a
make = |rows, cols, v|
    data = List.repeat v (rows * cols)
    { data, rows, cols }

# pretty print string from a grid
show : Grid a, U64 -> Str
show = |g, min_len|
    List.walk_with_index g.data "" |acc, v, idx|
        s = if idx % g.cols == 0 then "\n" else " "
        v_str = Inspect.to_str v
        v_len = Str.to_utf8 v_str |> List.len
        pad_len = if v_len < min_len then min_len - v_len else 0
        pad_str = Str.concat(v_str, Str.repeat(" ", pad_len))
        acc |> Str.concat s |> Str.concat pad_str
    |> Str.concat "\n"

# pretty print string from a grid
show_char : Grid U8 -> Str
show_char = |g|
    List.walk_with_index g.data "" |acc, v_, idx|
        v = Str.from_utf8 [v_] |> Util.msg_unwrap "Str.from_utf8 failed"
        s = if idx % g.cols == 0 then "\n" else " "
        acc |> Str.concat s |> Str.concat (Inspect.to_str v)
    |> Str.concat "\n"
    |> Str.replace_each "\"" ""

# returns true if pos is inside the grid otherwise false
is_inside : Grid a, Pos -> Bool
is_inside = |g, pos| pos < g.rows * g.cols

# returns the value in the grid at pos
get : Grid a, Pos -> Result a [OutOfBounds]
get = |g, pos| List.get g.data pos

get_unsafe : Grid a, Pos -> a
get_unsafe = |g, pos| List.get g.data pos |> Util.msg_unwrap "Grid.get_unsafe failed"

# return a new grid after setting the value in the grid at (r, c)
set : Grid a, Pos, a -> Grid a where a implements Inspect
set = |g, pos, v| { g & data: List.set g.data pos v }

swap : Grid a, Pos, Pos -> Grid a
swap = |g, p1, p2| { g & data: List.swap g.data p1 p2 }

find_positions : Grid a, (a -> Bool) -> List Pos
find_positions = |g, f|
    List.walk_with_index g.data [] |acc, v, idx| if f v then List.append acc idx else acc

pos_row : Grid a, Pos -> U64
pos_row = |g, pos| pos // g.rows

pos_col : Grid a, Pos -> U64
pos_col = |g, pos| pos % g.cols

pos_to_rc : Grid a, U64 -> RC
pos_to_rc = |g, pos|
    r = pos // g.rows
    c = pos % g.cols
    (r, c)

rc_to_pos : Grid a, RC -> U64
rc_to_pos = |g, (r, c)| r * g.cols + c

move : Grid a, Dir, Pos -> Result Pos [OutOfBounds]
move = |g, dir, pos|
    max_col = g.cols - 1
    max_row = g.rows - 1
    (r, c) = pos_to_rc g pos
    when dir is
        N -> if r > 0 then Ok (pos - g.cols) else Err OutOfBounds
        NW -> if r > 0 and c > 0 then Ok (pos - g.cols - 1) else Err OutOfBounds
        W -> if c > 0 then Ok (pos - 1) else Err OutOfBounds
        SW -> if r < max_row and c > 0 then Ok (pos + g.cols - 1) else Err OutOfBounds
        S -> if r < max_row then Ok (pos + g.cols) else Err OutOfBounds
        SE -> if r < max_row and c < max_col then Ok (pos + g.cols + 1) else Err OutOfBounds
        E -> if c < max_col then Ok (pos + 1) else Err OutOfBounds
        NE -> if r > 0 and c < max_col then Ok (pos - g.cols + 1) else Err OutOfBounds

move_unsafe : Grid a, Pos, Dir -> Pos
move_unsafe = |g, pos, dir|
    when dir is
        N -> pos - g.cols
        NW -> pos - g.cols - 1
        W -> pos - 1
        SW -> pos + g.cols - 1
        S -> pos + g.cols
        SE -> pos + g.cols + 1
        E -> pos + 1
        NE -> pos - g.cols + 1

# north = |pos| move pos N
# north_west = |pos| move pos NW
# west = |pos| move pos W
# south_west = |pos| move pos SW
# south = |pos| move pos S
# south_east = |pos| move pos SE
# east = |pos| move pos E
# north_east = |pos| move pos NE

neighbors4 : Grid a, Pos -> List (Result Pos [OutOfBounds])
neighbors4 = |g, pos| [
    move g N pos,
    move g W pos,
    move g S pos,
    move g E pos,
]

# neighbor_values4 : Grid a, Pos -> List (Pos, Result a [OutOfBounds])
# neighbor_values4 = |g, pos|
#     (n, w, s, e) = (move g N pos, move g W pos, move g S pos, move g E pos)
#     [
#         (n, get g n),
#         (w, get g w),
#         (s, get g s),
#         (e, get g e),
#     ]

# neighbor_values8_tup :
#     Grid a,
#     Pos
#     -> (
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#         [Err [OutOfBounds], Ok a],
#     )
# neighbor_values8_tup = |g, pos|
#     (
#         get g (north pos),
#         get g (north_west pos),
#         get g (west pos),
#         get g (south_west pos),
#         get g (south pos),
#         get g (south_east pos),
#         get g (east pos),
#         get g (north_east pos),
#     )

# apply4 : Grid a, Pos, (Grid a, Pos -> b) -> List b
# apply4 = |g, pos, f| [
#     f g (north pos),
#     f g (west pos),
#     f g (south pos),
#     f g (east pos),
# ]

# apply8 : Grid a, Pos, (Grid a, Pos -> b) -> List b
# apply8 = |g, pos, f| [
#     f g (north pos),
#     f g (north_west pos),
#     f g (west pos),
#     f g (south_west pos),
#     f g (south pos),
#     f g (south_east pos),
#     f g (east pos),
#     f g (north_east pos),
# ]

# tests

