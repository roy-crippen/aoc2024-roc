module [solution_day_17]

import Util exposing [Solution, unwrap]
import "../data/day_17.txt" as input_str : Str

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

parse : Str -> (Program, Reg)
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
    (program, reg)

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
                    Bxl -> (out, ptr + 1, { reg & b: Num.bitwise_xor(reg.b, operand) })
                    Bst -> (out, ptr + 1, { reg & b: combo(operand, reg) % 8 })
                    Jnz -> (out, if reg.a == 0 then ptr + 1 else operand, reg)
                    Bxc -> (out, ptr + 1, { reg & b: Num.bitwise_xor(reg.b, reg.c) })
                    Out -> (List.append(out, combo(operand, reg) % 8), ptr + 1, reg)
                    Bvd -> (out, ptr + 1, { reg & b: Num.shift_right_zf_by(reg.a, combo(operand, reg) |> Num.to_u8) })
                    Cvd -> (out, ptr + 1, { reg & c: Num.shift_right_zf_by(reg.a, combo(operand, reg) |> Num.to_u8) })
            go next

    prog_len = List.len program
    (out_list, _, _) = go(([], 0, register))
    out_str = out_list |> Inspect.to_str |> Str.replace_each("[", "") |> Str.replace_each("]", "") |> Str.replace_each(" ", "")
    # dbg out_str
    Str.replace_each(out_str, ",", "") |> Str.to_u64 |> unwrap

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
expected_part2 = 42

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (program, reg) = parse(in_str)
    Ok run(program, reg)

part2 : Str -> [Err Str, Ok U64]
part2 = |_in_str| Ok 42

example_str : Str
example_str =
    """
    Register A: 729
    Register B: 0
    Register C: 0

    Program: 0,1,5,4,3,0   
    """

# tests

expect part1 example_str == Ok 4635635210
expect part1 input_str == Ok expected_part1
# expect part2 example_str == Ok 42
# expect part2 input_str == Ok expected_part2

