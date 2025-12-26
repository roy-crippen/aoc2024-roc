module [solution_day_24]

import Util exposing [Solution, unwrap]
import "../data/day_24.txt" as input_str : Str

OpCode : [AND, OR, XOR]
Expr : { op1 : Str, op2 : Str, op_code : OpCode, out : Str }
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
                    [name1, op_code, name2, _, out] ->
                        expr = { op1: name1, op2: name2, op_code: op_code_from_str(op_code), out }
                        Dict.insert(acc, out, E expr)

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
bits_to_num = |vs|
    (n, _) = List.walk_backwards(
        vs,
        (0, 0),
        |(acc_res, p), v|
            m = Num.pow_int(2, p)
            (m * Num.to_u64(v) + acc_res, p + 1),
    )
    n

operands_to_u8s : List (Str, Operand), Str -> List U8
operands_to_u8s = |vs, prefix|
    vs
    |> List.map(
        |(k, v)|
            num_key = Str.drop_prefix(k, prefix) |> Str.to_u16 |> unwrap
            bit =
                when v is
                    V val -> val
                    _ -> crash "error, not a value"
            (num_key, bit),
    )
    |> List.sort_with(|(k1, _), (k2, _)| Num.compare k2 k1)
    |> List.map(|(_k, bit)| bit)

swap : Dict Str Operand, (Str, Str) -> Dict Str Operand
swap = |d, (s1, s2)|
    v1 = Dict.get(d, s1) |> unwrap
    v2 = Dict.get(d, s2) |> unwrap
    d |> Dict.insert(s2, v1) |> Dict.insert(s1, v2)

make_wire_key : Str, U16 -> Str
make_wire_key = |ch, num|
    when num is
        n if n < 10 -> ch |> Str.concat("0") |> Str.concat(Num.to_str(num))
        n if n < 100 -> ch |> Str.concat(Num.to_str(num))
        _ -> crash "invalid parameters to make_wire_key"

find_wire1 : Dict Str Operand, OpCode, Str -> Expr
find_wire1 = |d, op, key|
    res =
        d
        |> Dict.values
        |> List.find_first
            |wire|
                when wire is
                    V _ -> Bool.false
                    E expr -> expr.op_code == op and (expr.op1 == key or expr.op2 == key)
    when res is
        Ok (E expr) -> expr
        _ ->
            dbg (op, key)
            crash "key not found in find_wire_1"

find_wire2 : Dict Str Operand, OpCode, Str, Str -> Result Expr [NotFound]
find_wire2 = |d, op_code, key1, key2|
    res =
        d
        |> Dict.values
        |> List.find_first
            |wire|
                when wire is
                    V _ -> Bool.false
                    E expr ->
                        (expr.op_code == op_code)
                        and ((expr.op1 == key1 and expr.op2 == key2) or (expr.op1 == key2 and expr.op2 == key1))
    when res is
        Ok (E expr) -> Ok expr
        _ -> Err NotFound

fix_bit_n : Dict Str Operand, U16 -> Result (Dict Str Operand, (Str, Str)) [NotFound]
fix_bit_n = |d, n|
    prev_wx = make_wire_key("x", n - 1)
    prev_wy = make_wire_key("y", n - 1)
    prev_and = find_wire2(d, AND, prev_wx, prev_wy)?
    prev_xor = find_wire2(d, XOR, prev_wx, prev_wy)?
    m2 = find_wire1(d, AND, prev_xor.out)
    m1 = find_wire2(d, OR, m2.out, prev_and.out)?
    n_xor = find_wire2(d, XOR, make_wire_key("x", n), make_wire_key("y", n))?
    to_swap =
        wz = make_wire_key("z", n)
        when find_wire2(d, XOR, n_xor.out, m1.out) is
            Ok zn -> (wz, zn.out)
            _ ->
                zn =
                    when Dict.get(d, wz) is
                        Ok (E expr) -> expr
                        _ -> crash "expr not found in fix_bit_n"
                set1 = Set.from_list([zn.op1, zn.op2])
                set2 = Set.from_list([n_xor.out, m1.out])
                diff = Set.union(Set.difference(set1, set2), Set.difference(set2, set1))
                when diff |> Set.to_list is
                    [k1, k2] -> (k1, k2)
                    _ -> crash "Set.difference resultrs is wrong"

    Ok (swap(d, to_swap), to_swap)

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
expected_part2 = 57488782206064 # [(frn, z05), (wnf, vtj), (z21, gmq), (wtt, z39)] => frn,gmq,vtj,wnf,wtt,z05,z21,z39

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    d = parse(in_str)
    zs =
        Dict.keys(d)
        |> List.keep_if(|s| Str.starts_with(s, "z"))
        |> List.walk(d, |acc_d, id| (eval(id, acc_d)).1)
        |> Dict.keep_if(|(k, _v)| Str.starts_with(k, "z"))
        |> Dict.to_list
        |> operands_to_u8s("z")
    z = zs |> bits_to_num
    Ok z

# part 2 => find the bad bits
#                   5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#                             4                   3                   2                   1
#
#  proper carry in: 1 1 1 1 0 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0
#               xs: 0 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1 1 0 0 1 0 1 1 0 1 0 0 0 1 0 1 1 0 1 0 0 0 1 1
#               ys: 0 1 0 1 0 0 0 1 0 1 0 0 1 0 1 1 0 0 1 0 1 0 1 0 1 0 0 1 0 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1 0 1
#        proper zs: 1 1 0 1 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 0 0 1 0 0 1 0 0 1 0 0 0 1 1 1 0 0 0 0
#    calculated zs: 1 1 0 0 1 1 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1 0 1 0 0 1 0 0 0 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0 0
#                         * * * *                               * * *         *                 * * *
#                               ^                                   ^         ^                     ^
#    bad indexes = 39, 21, 16 and 5

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    d = parse(in_str)
    fix_bit_indexes = [5, 16, 21, 39]
    (d_after_swaps, swaps) =
        fix_bit_indexes
        |> List.walk(
            (d, []),
            |(acc_d, acc_pairs), num|
                (d_new, pair) = fix_bit_n(acc_d, num) |> unwrap
                (d_new, List.append(acc_pairs, pair)),
        )

    z_after_swaps =
        Dict.keys(d_after_swaps)
        |> List.keep_if(|s| Str.starts_with(s, "z"))
        |> List.walk(d_after_swaps, |acc_d, id| (eval(id, acc_d)).1)
        |> Dict.keep_if(|(k, _v)| Str.starts_with(k, "z"))
        |> Dict.to_list
        |> operands_to_u8s("z")
        |> bits_to_num

    # checks
    x = d |> Dict.keep_if(|(k, _v)| Str.starts_with(k, "x")) |> Dict.to_list |> operands_to_u8s("x") |> bits_to_num
    y = d |> Dict.keep_if(|(k, _v)| Str.starts_with(k, "y")) |> Dict.to_list |> operands_to_u8s("y") |> bits_to_num
    expect x + y == z_after_swaps

    (xs, ys) = Util.unzip(swaps)
    sorted_swaps =
        List.concat(xs, ys)
        |> Util.sort_list_str_asc_unsafe
        |> Str.join_with(",")
    expect sorted_swaps == "frn,gmq,vtj,wnf,wtt,z05,z21,z39"
    Ok z_after_swaps

example_str1 : Str
example_str1 =
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

expect part1 example_str1 == Ok 2024
expect part1 input_str == Ok expected_part1

t_xs = [1, 0, 1, 1]
t_ys = [1, 1, 0, 1]
t_zs = [1, 1, 0, 0, 0]

expect
    t_x = bits_to_num(t_xs)
    t_y = bits_to_num(t_ys)
    t_z = bits_to_num(t_zs)
    t_x + t_y == t_z

expect make_wire_key("x", 3) == "x03"
expect make_wire_key("x", 11) == "x11"
expect
    d = parse(input_str)
    b1 = find_wire1(d, XOR, "wkb") == { op1: "wkb", op2: "drd", op_code: XOR, out: "z04" }
    b2 = find_wire1(d, AND, "wkb") == { op1: "drd", op2: "wkb", op_code: AND, out: "rhr" }
    b3 = find_wire1(d, AND, "tfv") == { op1: "wkq", op2: "tfv", op_code: AND, out: "grv" }
    b1 and b2 and b3
