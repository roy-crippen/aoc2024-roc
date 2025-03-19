module [aaa]

import Bool exposing [true]

import Grid exposing [
    Grid,
    make,
    show_char,
    is_inside,
    get,
    set,
    find_positions,
    apply4,
    apply8,
]

aaa : Str
aaa = "aaa"

g_ : Grid U8
g_ = make 3 3 '.'
g = set g_ (1, 1) '^'

expect !(show_char g |> dbg |> Str.is_empty)
expect is_inside   g (0, 0)
expect is_inside   g (2, 2)
expect is_inside   g (0, 2)
expect is_inside   g (2, 0)
expect !(is_inside g (3, 0))
expect !(is_inside g (0, 3))
expect !(is_inside g (3, 3))
expect Ok '.' == get g (0, 0)
expect Err OutOfBounds == get g (6, 0)
expect Ok '^' == get g (1, 1)
expect [(1, 1)] == find_positions g (|v| v == '^')
expect [(0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)] == find_positions g (|v| v == '.')

expect
    expected = [true, true, true, true, true, true, true, true]
    got = apply8 g (1, 1) is_inside
    expected == got

expect
    expected = [Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.']
    got = apply8 g (1, 1) get
    expected == got

expect
    expected = [Err OutOfBounds, Err OutOfBounds, Ok '.', Ok '.', Ok '^', Ok '.', Ok '.', Err OutOfBounds]
    got = apply8 g (0, 1) get
    expected == got

expect
    expected = [true, true, true, true]
    got = apply4 g (1, 1) is_inside
    expected == got

expect
    expected = [Ok '.', Ok '.', Ok '.', Ok '.']
    got = apply4 g (1, 1) get
    expected == got

expect
    expected = [true, true, true, true, true, true, true, true]
    got = apply8 g (1, 1) is_inside
    expected == got

expect
    expected = [Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.', Ok '.']
    got = apply8 g (1, 1) get
    expected == got

expect
    expected = [Err OutOfBounds, Err OutOfBounds, Ok '.', Ok '.', Ok '^', Ok '.', Ok '.', Err OutOfBounds]
    got = apply8 g (0, 1) get
    expected == got

expect
    expected = [true, true, true, true]
    got = apply4 g (1, 1) is_inside
    expected == got

expect
    expected = [Ok '.', Ok '.', Ok '.', Ok '.']
    got = apply4 g (1, 1) get
    expected == got

expect
    expected = [Err OutOfBounds, Ok '.', Ok '^', Ok '.']
    got = apply4 g (0, 1) get
    expected == got

expect
    expected = [Err OutOfBounds, Ok '.', Ok '^', Ok '.']
    got = apply4 g (0, 1) get
    expected == got
