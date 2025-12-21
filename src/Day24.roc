module [solution_day_24]

import Util exposing [Solution, unwrap]
import "../data/day_24.txt" as input_str : Str

OpCode : [AND, OR, XOR]
Expr : { op1 : Str, op2 : Str, op_code : OpCode }
Operand : [V U8, E Expr]

parse : Str -> Dict Str Operand
parse = |s|
    (vals, exprs) =
        when s |> Str.split_on("\n") |> List.split_on("") is
            [vs, es] -> (vs, es)
            _ -> crash "parse error"
    dbg vals
    dbg exprs

    d0 =
        vals
        |> List.walk(
            Dict.empty({}),
            |acc, str|
                when str |> Str.split_on(": ") is
                    [name, s_val] -> Dict.insert(acc, name, V (Str.to_u8(s_val) |> unwrap))
                    _ -> crash "parse error",
        )

    d0

eval : Str, Dict Str Operand -> (U8, Dict Str Operand)
eval = |id, d|
    when Dict.get(d, id) |> unwrap is
        V val -> (val, d)
        E expr ->
            (v1, d1) = eval(expr.op1, d)
            (v2, d2) = eval(expr.op2, d1)
            val =
                when expr.op_code is
                    AND -> Num.bitwise_and(v1, v2)
                    OR -> Num.bitwise_or(v1, v2)
                    XOR -> Num.bitwise_xor(v1, v2)
            (val, Dict.insert(d2, id, V val))

solution_day_24 : Solution
solution_day_24 = {
    day: 24,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 42

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    d = parse(in_str)
    dbg
        (
            Dict.to_list(d)
            |> List.for_each!(
                |pair|
                    dbg pair
                    {},
            )
        )

    Ok 42

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    x00: 1
    x01: 1
    x02: 1
    y00: 0
    y01: 1
    y02: 0

    x00 AND y00 -> z00
    x01 XOR y01 -> z01
    x02 OR y02 -> z02
    """

# tests

test_prg_ls : List (Str, Operand)
test_prg_ls = [
    ("x00", V 1),
    ("x01", V 1),
    ("x02", V 1),
    ("y00", V 0),
    ("y01", V 1),
    ("y02", V 0),
    ("z00", E { op1: "x00", op2: "y00", op_code: AND }),
    ("z01", E { op1: "x01", op2: "y01", op_code: XOR }),
    ("z02", E { op1: "x02", op2: "y02", op_code: OR }),
    ("z03", E { op1: "z00", op2: "z02", op_code: OR }),
]

test_prg : Dict Str Operand
test_prg = test_prg_ls |> Dict.from_list

expect part1 example_str == Ok 42
# expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

expect Dict.len(test_prg) == List.len(test_prg_ls)
expect
    (v, _d) = eval("x00", test_prg)
    v == 1
expect
    (v, _d) = eval("y00", test_prg)
    v == 0
expect
    (v, _d) = eval("z00", test_prg)
    v == 0
expect
    (v, _d) = eval("z01", test_prg)
    v == 0
expect
    (v, _d) = eval("z02", test_prg)
    v == 1
expect
    (v, _d) = eval("z03", test_prg)
    v == 1

