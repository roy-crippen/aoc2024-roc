module [solution_day_17]

import Util exposing [Solution, unwrap]
import "../data/day_17.txt" as input_str : Str
import Bool exposing [true, false]

# part 2 is a port of https://github.com/hyperneutrino/advent-of-code/blob/main/2024/day17p2.py

Reg : { a : U64, b : U64, c : U64 }
Inst : [Avd, Bxl, Bst, Jnz, Bxc, Out, Bvd, Cvd]
Program : List (Inst, U64)

reg_value : Str -> U64
reg_value = |s| Str.split_first(s, ": ") |> unwrap |> .after |> Str.to_u64 |> unwrap

get_inst : U64 -> Inst
get_inst = |v|
    when v is
        0 -> Avd
        1 -> Bxl
        2 -> Bst
        3 -> Jnz
        4 -> Bxc
        5 -> Out
        6 -> Bvd
        7 -> Cvd
        _ -> crash("invalid instruction code")

parse : Str -> (Program, Reg, List U64)
parse = |s|
    (reg_s, p_s) =
        when Str.split_on s "\n\n" is
            [s1, s2] -> (s1, s2)
            _ -> crash("bad input data for day 17")
    reg =
        when Str.split_on reg_s "\n" is
            [a_s, b_s, c_s] ->
                { a: reg_value(a_s), b: reg_value(b_s), c: reg_value(c_s) }

            _ -> crash("bad register data for day 17")

    prog_s = Str.split_first(p_s, ": ") |> unwrap |> .after |> Str.trim
    prog_values = Str.split_on(prog_s, ",") |> List.map(|v_s| Str.to_u64 v_s |> unwrap)
    program =
        List.chunks_of(prog_values, 2)
        |> List.map(
            |vs|
                when vs is
                    [code, operand] -> (get_inst(code), operand)
                    _ -> crash("parse error"),
        )
    (program, reg, prog_values)

combo : U64, Reg -> U64
combo = |operand, reg|
    when operand is
        v if v >= 0 and v <= 3 -> operand
        4 -> reg.a
        5 -> reg.b
        6 -> reg.c
        _ -> crash("invalid operand in combo")

run : Program, Reg -> U64
run = |program, register|
    go : (List U64, U64, Reg) -> (List U64, U64, Reg)
    go = |(out, ptr, reg)|
        if ptr >= prog_len then
            (out, ptr, reg)
        else
            (instruction, operand) = List.get(program, ptr) |> unwrap
            next =
                when instruction is
                    Avd -> (out, ptr + 1, { reg & a: Num.shift_right_zf_by(reg.a, combo(operand, reg) |> Num.to_u8) })
                    Bst -> (out, ptr + 1, { reg & b: combo(operand, reg) % 8 })
                    Jnz -> (out, if reg.a == 0 then ptr + 1 else operand, reg)
                    Bxl -> (out, ptr + 1, { reg & b: Num.bitwise_xor(reg.b, operand) })
                    Bxc -> (out, ptr + 1, { reg & b: Num.bitwise_xor(reg.b, reg.c) })
                    Out -> (List.append(out, combo(operand, reg) % 8), ptr + 1, reg)
                    Bvd -> (out, ptr + 1, { reg & b: Num.shift_right_zf_by(reg.a, combo(operand, reg) |> Num.to_u8) })
                    Cvd -> (out, ptr + 1, { reg & c: Num.shift_right_zf_by(reg.a, combo(operand, reg) |> Num.to_u8) })
            go next

    prog_len = List.len program
    (out_list, _ptr, _r) = go(([], 0, register))
    out_str = out_list |> Inspect.to_str |> Str.replace_each("[", "") |> Str.replace_each("]", "") |> Str.replace_each(" ", "")
    Str.replace_each(out_str, ",", "") |> Str.to_u64 |> unwrap

find_a : Program, List U64 -> U64
find_a = |full_program, rev_prog_vals|
    program = List.drop_last(full_program, 1)
    go : List U64, U64, U64 -> (List U64, U64)
    go = |target, ans, t|
        if t > 8 or List.len(target) == 0 then
            (target, ans)
        else
            a = Num.shift_left_by(ans, 3) |> Num.bitwise_or(t)
            # dbg (target, ans, t, a)
            reg = { a, b: 0u64, c: 0u64 }
            desired = List.last(target) |> Util.unwrap

            (found, new_reg) =
                List.walk_until program (false, reg) |(_b, r), (ins, op)|
                    when ins is
                        Avd -> Continue (false, r)
                        Bxl -> Continue (false, { r & b: Num.bitwise_xor(r.b, op) })
                        Bst -> Continue (false, { r & b: combo(op, r) % 8 })
                        Jnz -> crash("program has JNZ inside expected loop body")
                        Bxc -> Continue (false, { r & b: Num.bitwise_xor(r.b, r.c) })
                        Out ->
                            output = combo(op, r) % 8
                            if output == desired then
                                (_, sub) = go(List.drop_last(target, 1), r.a, 0)
                                Break (true, { r & a: sub })
                            else
                                Continue (false, r)

                        Bvd -> Continue (false, { r & b: Num.shift_right_zf_by(r.a, combo(op, r) |> Num.to_u8) })
                        Cvd -> Continue (false, { r & c: Num.shift_right_zf_by(r.a, combo(op, r) |> Num.to_u8) })
            if found then (target, new_reg.a) else go(target, ans, t + 1)

    (_, res) = go(rev_prog_vals, 0, 0)
    res

solution_day_17 : Solution
solution_day_17 = {
    day: 17,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 461421316

expected_part2 : U64
expected_part2 = 202366627359274

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (program, reg, _) = parse(in_str)
    Ok run(program, reg)

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (program, _reg, prog_vals) = parse(in_str)
    Ok find_a(program, prog_vals)

example_str : Str
example_str =
    """
    Register A: 729
    Register B: 0
    Register C: 0

    Program: 0,1,5,4,3,0
    """

example_str2 : Str
example_str2 =
    """
    Register A: 117440
    Register B: 0
    Register C: 0

    Program: 0,3,5,4,3,0
    """

# tests

expect part1 example_str == Ok 4635635210
expect part1 input_str == Ok expected_part1
expect part2 example_str2 == Ok 14680
expect part2 input_str == Ok expected_part2
