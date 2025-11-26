# Deque.roc
module [
    Deque,
    empty,
    from_list,
    push_front,
    push_back,
    pop_front,
    pop_back,
    peek_front,
    peek_back,
    is_empty,
    len,
    to_list,
]

Deque a : {
    front : List a,
    back : List a,
    len_front : U64,
    len_back : U64,
} where a implements Inspect

empty : {} -> Deque a
empty = |_|
    xs : List a
    xs = List.with_capacity(1024)
    { front: xs, back: xs, len_front: 0u64, len_back: 0u64 }

from_list : List a -> Deque a
from_list = |list|
    { front: list, back: [], len_front: List.len list, len_back: 0 }

push_front : Deque a, a -> Deque a
push_front = |d, elem|
    new_front = List.prepend d.front elem
    new_len_front = d.len_front + 1

    if should_balance new_len_front d.len_back then
        balance new_front d.back new_len_front d.len_back
    else
        { front: new_front, back: d.back, len_front: new_len_front, len_back: d.len_back }

push_back : Deque a, a -> Deque a
push_back = |d, elem|
    new_back = List.prepend d.back elem
    new_len_back = d.len_back + 1

    if should_balance d.len_front new_len_back then
        balance d.front new_back d.len_front new_len_back
    else
        { front: d.front, back: new_back, len_front: d.len_front, len_back: new_len_back }

pop_front : Deque a -> Result (a, Deque a) [Empty]
pop_front = |d|
    when d.front is
        [head, .. as rest] ->
            new_len_front = d.len_front - 1
            new_deque =
                if new_len_front == 0 and List.is_empty d.back then
                    empty({})
                else if should_balance new_len_front d.len_back then
                    balance rest d.back new_len_front d.len_back
                else
                    { front: rest, back: d.back, len_front: new_len_front, len_back: d.len_back }

            Ok (head, new_deque)

        [] ->
            when List.last d.back is
                Ok head ->
                    dropped = List.drop_last d.back 1
                    new_front = List.reverse dropped
                    new_deque = { front: new_front, back: [], len_front: d.len_back - 1, len_back: 0 }
                    Ok (head, new_deque)

                Err _ -> Err Empty

pop_back : Deque a -> Result (a, Deque a) [Empty]
pop_back = |d|
    when d.back is
        [head_back, .. as rest] ->
            new_len_back = d.len_back - 1
            new_deque =
                if new_len_back == 0 and List.is_empty d.front then
                    empty({})
                else if should_balance d.len_front new_len_back then
                    balance d.front rest d.len_front new_len_back
                else
                    { front: d.front, back: rest, len_front: d.len_front, len_back: new_len_back }

            Ok (head_back, new_deque)

        [] ->
            when List.last d.front is
                Ok last ->
                    init = List.drop_last d.front 1
                    Ok (last, { front: init, back: [], len_front: d.len_front - 1, len_back: 0 })

                Err _ -> Err Empty

peek_front : Deque a -> Result a [Empty]
peek_front = |d|
    when d.front is
        [head, ..] -> Ok head
        [] ->
            when List.last d.back is
                Ok x -> Ok x
                Err _ -> Err Empty

peek_back : Deque a -> Result a [Empty]
peek_back = |d|
    when d.back is
        [head, ..] -> Ok head
        [] ->
            when List.last d.front is
                Ok x -> Ok x
                Err _ -> Err Empty

is_empty : Deque a -> Bool
is_empty = |d| d.len_front == 0 and d.len_back == 0

len : Deque a -> U64
len = |d| d.len_front + d.len_back

to_list : Deque a -> List a
to_list = |d| List.concat d.front (List.reverse d.back)

# ---- internal helpers -----------------------------------------------------

should_balance : U64, U64 -> Bool
should_balance = |len_f, len_b|
    len_f > 3 * len_b + 1 or len_b > 3 * len_f + 1

balance : List a, List a, U64, U64 -> Deque a
balance = |f, b, lf, lb|
    total = lf + lb
    half = total // 2
    extra = total % 2
    front_size = half + extra
    if lf > lb then
        { before, others } = List.split_at f front_size
        rev_others = List.reverse others
        new_back = List.concat b rev_others
        { front: before, back: new_back, len_front: front_size, len_back: half }
    else
        move = front_size - lf
        rev_b = List.reverse b
        { before, others } = List.split_at rev_b move
        new_front = List.concat f before
        new_back = List.reverse others
        { front: new_front, back: new_back, len_front: front_size, len_back: half }

# ---- tests ----------------------------------------------------------------

expect is_empty empty({}) == Bool.true

expect
    d : Deque U16
    d = empty({}) |> push_front 42 |> push_back 7
    peek_front d == Ok 42 and peek_back d == Ok 7

expect
    d : Deque U16
    d = from_list [1, 2, 3, 4]
    to_list d == [1, 2, 3, 4]

expect
    d : Deque U16
    d = empty({}) |> push_back 1 |> push_back 2 |> push_back 3
    when pop_front d is
        Ok (a, d1) ->
            when pop_front d1 is
                Ok (b, d2) ->
                    when pop_front d2 is
                        Ok (c, _) -> a == 1 and b == 2 and c == 3
                        Err _ -> Bool.false

                Err _ -> Bool.false

        Err _ -> Bool.false

expect
    d : Deque U16
    d = empty({}) |> push_front 3 |> push_front 2 |> push_front 1
    when pop_back d is
        Ok (a, d1) ->
            when pop_back d1 is
                Ok (b, d2) ->
                    when pop_back d2 is
                        Ok (c, _) -> a == 3 and b == 2 and c == 1
                        Err _ -> Bool.false

                Err _ -> Bool.false

        Err _ -> Bool.false

expect len (from_list [10, 20, 30, 40, 50]) == 5

expect
    d : Deque U16
    d = empty({})
    pop_front d == Err Empty

expect
    d : Deque U16
    d = empty({})
    pop_back d == Err Empty

expect
    d : Deque U16
    d = empty({})
    peek_front d == Err Empty

expect
    d : Deque U16
    d = empty({})
    peek_back d == Err Empty

expect
    d : Deque U16
    d = empty({}) |> push_front 1u16
    when pop_front d is
        Ok (x, d2) -> x == 1u16 && is_empty d2
        Err _ -> Bool.false

expect
    d : Deque U16
    d = empty({}) |> push_back 1u16
    when pop_back d is
        Ok (x, d2) -> x == 1u16 && is_empty d2
        Err _ -> Bool.false

expect
    d : Deque U16
    d = empty({}) |> push_front 1u16 |> push_front 2u16 |> push_front 3u16 |> push_front 4u16 |> push_front 5u16
    to_list d == [5u16, 4u16, 3u16, 2u16, 1u16] # Test balance after many front pushes

expect
    d : Deque U16
    d = empty({}) |> push_back 1u16 |> push_back 2u16 |> push_back 3u16 |> push_back 4u16 |> push_back 5u16
    to_list d == [1u16, 2u16, 3u16, 4u16, 5u16] # Test balance after many back pushes

expect
    res = empty({}) |> push_front 1u16 |> pop_back
    when res is
        Ok (x, _) -> x == 1u16
        Err _ -> Bool.false

expect
    res = empty({}) |> push_back 1u16 |> pop_front
    when res is
        Ok (x, _) -> x == 1u16
        Err _ -> Bool.false
