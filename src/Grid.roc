module [
    Grid,
    Pos,
    Dir,
    make,
    show,
    show_char,
    is_inside,
    pos_to_idx,
    idx_to_pos,
    get,
    get_unsafe,
    set,
    find_positions,
    move,
    north,
    north_west,
    west,
    south_west,
    south,
    south_east,
    east,
    north_east,
    neighbors4,
    neighbor_values4,
    neighbor_values8_tup,
    apply4,
    apply8,
]

import Util

Grid a : { data : List a, rows : U64, cols : U64 } where a implements Inspect
Pos : (I32, I32)
Dir : [N, NW, W, SW, S, SE, E, NE]

# returns a Grid of rows by cols all with values v
make : U64, U64, a -> Grid a
make = |rows, cols, v|
    data = List.repeat v (rows * cols)
    { data, rows, cols }

# pretty print string from a grid
show : Grid a -> Str
show = |g|
    List.walk_with_index g.data "" |acc, v, idx|
        s = if idx % g.cols == 0 then "\n" else " "
        acc |> Str.concat s |> Str.concat (Inspect.to_str v)
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
is_inside = |g, pos|
    r = pos.0 |> Num.to_u64
    c = pos.1 |> Num.to_u64
    r < g.rows and c < g.cols

pos_to_idx : Pos, U64 -> U64
pos_to_idx = |(r, c), cols| (Num.to_u64 r) * cols + (Num.to_u64 c)

idx_to_pos : U64, U64, U64 -> Pos
idx_to_pos = |idx, rows, cols|
    r = idx // rows |> Num.to_i32
    c = idx % cols |> Num.to_i32
    (r, c)
    

# returns the value in the grid at (r, c)
get : Grid a, Pos -> [Err [OutOfBounds], Ok a]
get = |g, pos|
    r = Num.to_u64 pos.0
    c = Num.to_u64 pos.1
    if r >= 0 and r < g.rows and c >= 0 and c < g.cols then
        (List.get g.data (pos_to_idx pos g.cols))? |> Ok
    else
        Err OutOfBounds

get_unsafe : Grid a, Pos -> a
get_unsafe = |g, pos| List.get g.data (pos_to_idx pos g.cols) |> Util.unwrap

# return a new grid after setting the value in the grid at (r, c)
set : Grid a, Pos, a -> Grid a where a implements Inspect
set = |g, pos, v| { g & data: List.set g.data (pos_to_idx pos g.cols) v }

find_positions : Grid a, (a -> Bool) -> List Pos
find_positions = |g, f|
    ls = List.walk_with_index g.data [] |acc, v, idx| if f v then List.append acc idx else acc
    List.map ls |idx| idx_to_pos idx g.rows g.cols

move : Pos, Dir -> Pos
move = |(r, c), dir|
    when dir is
        N -> (r - 1, c)
        NW -> (r - 1, c - 1)
        W -> (r, c - 1)
        SW -> (r + 1, c - 1)
        S -> (r + 1, c)
        SE -> (r + 1, c + 1)
        E -> (r, c + 1)
        NE -> (r - 1, c + 1)

north = |pos| move pos N
north_west = |pos| move pos NW
west = |pos| move pos W
south_west = |pos| move pos SW
south = |pos| move pos S
south_east = |pos| move pos SE
east = |pos| move pos E
north_east = |pos| move pos NE

neighbors4 : Pos -> List Pos
neighbors4 = |pos| [north pos, west pos, south pos, east pos]

neighbor_values4 : Grid a, Pos -> List (Pos, [Err [OutOfBounds], Ok a])
neighbor_values4 = |g, pos|
    (n, w, s, e) = (north pos, west pos, south pos, east pos)
    [
        (n, get g n),
        (w, get g w),
        (s, get g s),
        (e, get g e),
    ]

neighbor_values8_tup :
    Grid a,
    Pos
    -> (
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
        [Err [OutOfBounds], Ok a],
    )
neighbor_values8_tup = |g, pos|
    (
        get g (north pos),
        get g (north_west pos),
        get g (west pos),
        get g (south_west pos),
        get g (south pos),
        get g (south_east pos),
        get g (east pos),
        get g (north_east pos),
    )

apply4 : Grid a, Pos, (Grid a, Pos -> b) -> List b
apply4 = |g, pos, f| [
    f g (north pos),
    f g (west pos),
    f g (south pos),
    f g (east pos),
]

apply8 : Grid a, Pos, (Grid a, Pos -> b) -> List b
apply8 = |g, pos, f| [
    f g (north pos),
    f g (north_west pos),
    f g (west pos),
    f g (south_west pos),
    f g (south pos),
    f g (south_east pos),
    f g (east pos),
    f g (north_east pos),
]
