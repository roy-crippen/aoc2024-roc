module [solution_day_24]

import Util exposing [Solution, unwrap]
import "../data/day_24.txt" as input_str : Str

OpCode : [AND, OR, XOR]
Expr : { op1 : Str, op2 : Str, op_code : OpCode }
Operand : [V U8, E Expr]

op_code_from_str : Str -> OpCode
op_code_from_str = |s|
    when s is
        "AND" -> AND
        "OR" -> OR
        "XOR" -> XOR
        _ -> crash "invalid op_code string"

parse : Str -> Dict Str Operand
parse = |s|
    (vals, exprs) =
        when s |> Str.split_on("\n") |> List.split_on("") is
            [vs, es] -> (vs, es)
            _ -> crash "parse error"
    # dbg vals
    # dbg exprs

    d0 =
        vals
        |> List.walk(
            Dict.empty({}),
            |acc, str|
                when str |> Str.split_on(": ") is
                    [name, s_val] -> Dict.insert(acc, name, V (Str.to_u8(s_val) |> unwrap))
                    _ -> crash "parse error",
        )

    d1 =
        exprs
        |> List.walk(
            d0,
            |acc, str|
                when str |> Str.split_on(" ") is
                    [name1, op_code, name2, _, name3] ->
                        expr = { op1: name1, op2: name2, op_code: op_code_from_str(op_code) }
                        Dict.insert(acc, name3, E expr)

                    _ -> crash "parse error",
        )

    d1

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

bits_to_num : List U8 -> U64
bits_to_num = |xs|
    (n, _) = List.walk(
        xs,
        (0, 0),
        |(acc_res, p), v|
            m = Num.pow_int(2, p)
            (m * Num.to_u64(v) + acc_res, p + 1),
    )
    n

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
expected_part1 = 56939028423824

expected_part2 : U64
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    d = parse(in_str)
    Dict.keys(d)
    |> List.keep_if(|s| Str.starts_with(s, "z"))
    |> List.walk(d, |acc_d, id| (eval(id, acc_d)).1)
    |> Dict.keep_if(|(k, _v)| Str.starts_with(k, "z"))
    |> Dict.to_list
    |> List.map(
        |(k, v)|
            num_key = Str.drop_prefix(k, "z") |> Str.to_u16 |> unwrap
            bit =
                when v is
                    V val -> val
                    _ -> crash "error, not a value"
            (num_key, bit),
    )
    |> List.sort_with(|(k1, _), (k2, _)| Num.compare k1 k2)
    |> List.map(|(_k, bit)| bit)
    |> bits_to_num
    |> Ok

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    x00: 1
    x01: 0
    x02: 1
    x03: 1
    x04: 0
    y00: 1
    y01: 1
    y02: 1
    y03: 1
    y04: 1

    ntg XOR fgs -> mjb
    y02 OR x01 -> tnw
    kwq OR kpj -> z05
    x00 OR x03 -> fst
    tgd XOR rvg -> z01
    vdt OR tnw -> bfw
    bfw AND frj -> z10
    ffh OR nrd -> bqk
    y00 AND y03 -> djm
    y03 OR y00 -> psh
    bqk OR frj -> z08
    tnw OR fst -> frj
    gnj AND tgd -> z11
    bfw XOR mjb -> z00
    x03 OR x00 -> vdt
    gnj AND wpb -> z02
    x04 AND y00 -> kjc
    djm OR pbm -> qhw
    nrd AND vdt -> hwm
    kjc AND fst -> rvg
    y04 OR y02 -> fgs
    y01 AND x02 -> pbm
    ntg OR kjc -> kwq
    psh XOR fgs -> tgd
    qhw XOR tgd -> z09
    pbm OR djm -> kpj
    x03 XOR y03 -> ffh
    x00 XOR y04 -> ntg
    bfw OR bqk -> z06
    nrd XOR fgs -> wpb
    frj XOR qhw -> z04
    bqk OR frj -> z07
    y03 OR x01 -> nrd
    hwm AND bqk -> z03
    tgd XOR rvg -> z12
    tnw OR pbm -> gnj
    """

# tests

expect part1 example_str == Ok 2024
expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

