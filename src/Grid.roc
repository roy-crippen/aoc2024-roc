module [
    Grid,
    Pos,
    Dir,
    make,
    show,
    show_char,
    get_rows,
    get_cols,
    get_size,
    is_inside,
    get,
    set,
    find_positions,
    move,
    north,
    north_west,
    west,
    south_west,
    south,
    south_east,
    north_east,
    apply4,
    apply8,
]

import Util

Grid a : List (List a) where a implements Inspect
Pos : (I32, I32)
Dir : [N, NW, W, SW, S, SE, E, NE]

# returns a Grid of rows by cols all with values v
make : U64, U64, a -> Grid a
make = |rows, cols, v|
    col_list = List.repeat v cols
    List.repeat col_list rows

# pretty print string from a grid
show : Grid a -> Str
show = |g|
    vs1 = g |> List.map |xs| Str.concat (Inspect.to_str xs) "\n"
    List.prepend vs1 "\n" |> Str.join_with ""

# pretty print string from a grid
show_char : Grid U8 -> Str
show_char = |g|
    vs1 = List.map g |xs|
        ls = List.map xs |x| Str.from_utf8 [x] |> Util.unwrap "Str.from_utf8 failed"
        Str.join_with ls " " |> Str.concat "\n"
    List.prepend vs1 "\n" |> Str.join_with ""

# get the number of rows in the grid
get_rows : Grid a -> I32
get_rows = |g| List.len g |> Num.to_i32

# get the number of cols in the grid
get_cols : Grid a -> [Err [OutOfBounds], Ok I32]
get_cols = |g| (List.get g 0)? |> List.len |> Num.to_i32 |> Ok

# returns (rows, cols)
get_size : Grid a -> [Err [OutOfBounds], Ok (I32, I32)]
get_size = |g| (get_rows g, (get_cols g)?) |> Ok

# returns true if pos is inside the grid otherwise false
is_inside : Grid a, Pos -> Bool
is_inside = |g, (r, c)|
    col_ok =
        when get_cols g is
            Ok cols if c < cols -> Bool.true
            _ -> Bool.false
    r < get_rows(g) and col_ok

# returns the value in the grid at (r, c)
get : Grid a, Pos -> [Err [OutOfBounds], Ok a]
get = |g, (r, c)| (List.get g Num.to_u64(r))? |> List.get Num.to_u64(c)

# returns the value in the grid at (r, c)
set : Grid a, Pos, a -> [Err [OutOfBounds], Ok (Grid a)] where a implements Inspect
set = |g, (r_, c_), v|
    r = Num.to_u64(r_)
    c = Num.to_u64(c_)
    vs = (List.get g r)? |> List.set c v
    List.set g r vs |> Ok

find_positions : Grid a, (a -> Bool) -> List Pos
find_positions = |g, pred_f|
    flat_ls =
        List.map_with_index g |xs, i|
            List.map_with_index xs |x, j| (Num.to_i32(i), Num.to_i32(j), x)
        |> List.join

    keep_f = |(i, j, x)| if pred_f x then Ok((i, j)) else Err(None)
    List.keep_oks flat_ls keep_f

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
