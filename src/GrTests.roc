module [gr_test]

# import Bool exposing [true]

import Gr exposing [
    Grid,
    make,
    show_char,
    is_inside,
    get,
    get_unsafe,
    set,
    find_positions,
    rc_to_pos,
    pos_to_rc,
    move,
    move_unsafe,
    neighbors4,
]

gr_test : Str
gr_test = "gr_test"

g_ : Grid U8
g_ = make 3 3 '.'
g = set g_ 4 '^'

expect rc_to_pos g (1, 1) == 4
expect rc_to_pos g (2, 2) == 8
expect rc_to_pos g (0, 2) == 2
expect rc_to_pos g (2, 0) == 6

expect pos_to_rc g 4 == (1, 1)
expect pos_to_rc g 8 == (2, 2)
expect pos_to_rc g 2 == (0, 2)
expect pos_to_rc g 6 == (2, 0)

expect
    s = show_char g |> Str.trim
    should_be =
        """
        . . .
        . ^ .
        . . .
        """
    s == should_be

expect is_inside g 0
expect is_inside g 8
expect is_inside g 2
expect is_inside g 6
expect !(is_inside g 9)

expect Ok '.' == get g 0
expect Err OutOfBounds == get g 15
expect Ok '^' == get g 4

expect '.' == get_unsafe g 0
expect '^' == get_unsafe g 4

expect [4] == find_positions g (|v| v == '^')
expect [4] == find_positions g (|v| v == '^')

expect [0, 1, 2, 3, 5, 6, 7, 8] == find_positions g (|v| v == '.')

expect move g N 4 == Ok 1
expect move g NW 4 == Ok 0
expect move g W 4 == Ok 3
expect move g SW 4 == Ok 6
expect move g S 4 == Ok 7
expect move g SE 4 == Ok 8
expect move g E 4 == Ok 5
expect move g NE 4 == Ok 2

expect move g N 1 == Err OutOfBounds
expect move g NW 0 == Err OutOfBounds
expect move g W 3 == Err OutOfBounds
expect move g SW 6 == Err OutOfBounds
expect move g S 7 == Err OutOfBounds
expect move g SE 8 == Err OutOfBounds
expect move g E 5 == Err OutOfBounds
expect move g NE 2 == Err OutOfBounds

expect move_unsafe g 4 N == 1
expect move_unsafe g 4 NW == 0
expect move_unsafe g 4 W == 3
expect move_unsafe g 4 SW == 6
expect move_unsafe g 4 S == 7
expect move_unsafe g 4 SE == 8
expect move_unsafe g 4 E == 5
expect move_unsafe g 4 NE == 2

expect neighbors4 g 4 == [Ok 1, Ok 3, Ok 7, Ok 5]

# expect
#     expected = [true, true, true, true, true, true, true, true]
#     got = apply8 g (1, 1) is_inside
#     expected == got

# expect
#     expected = [Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.']
#     got = apply8 g (1, 1) get
#     expected == got

# expect
#     expected = [Err OutOfBounds, Err OutOfBounds, Ok '.', Ok '.', Ok '^', Ok '.', Ok '.', Err OutOfBounds]
#     got = apply8 g (0, 1) get
#     expected == got

# expect
#     expected = [true, true, true, true]
#     got = apply4 g (1, 1) is_inside
#     expected == got

# expect
#     expected = [Ok '.', Ok '.', Ok '.', Ok '.']
#     got = apply4 g (1, 1) get
#     expected == got

# expect
#     expected = [true, true, true, true, true, true, true, true]
#     got = apply8 g (1, 1) is_inside
#     expected == got

# expect
#     expected = [Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.']
#     got = apply8 g (1, 1) get
#     expected == got

# expect
#     expected = [Err OutOfBounds, Err OutOfBounds, Ok '.', Ok '.', Ok '^', Ok '.', Ok '.', Err OutOfBounds]
#     got = apply8 g (0, 1) get
#     expected == got

# expect
#     expected = [true, true, true, true]
#     got = apply4 g (1, 1) is_inside
#     expected == got

# expect
#     expected = [Ok '.', Ok '.', Ok '.', Ok '.']
#     got = apply4 g (1, 1) get
#     expected == got

# expect
#     expected = [Err OutOfBounds, Ok '.', Ok '^', Ok '.']
#     got = apply4 g (0, 1) get
#     expected == got

# expect
#     expected = [Err OutOfBounds, Ok '.', Ok '^', Ok '.']
#     got = apply4 g (0, 1) get
#     expected == got
