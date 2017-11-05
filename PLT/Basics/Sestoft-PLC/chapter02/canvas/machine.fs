module Machine

type Instr =
    | Cst of int 
    | Add 
    | Sub 
    | Mul 
    | Div 
    | Mod 
    | Equ 
    | Neq 
    | Max 
    | Min 
    | Dup 
    | Swp

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)
    | Let of (string * Expr * Expr)

let rec stack_machine instructions stack =
    match (instructions, stack) with
        | ([], top::_) -> top
        | ([], []) -> failwith "STACK MACHINE: no results on stack!"
        | (Cst i :: instr, stk) -> 
            stack_machine instr (i::stk)
        | (Add::instr, i1::i2::stk) -> 
            let value = i1 + i2 in
                stack_machine instr (value::stk)
        | (Sub::instr, i1::i2::stk) -> 
            let value = i2 - i1 in
                stack_machine instr (value::stk)
        | (Mul::instr, i1::i2::stk) -> 
            let value = i1 * i2 in
                stack_machine instr (value::stk)
        | (Div::instr, i1::i2::stk) -> 
            let value = i2 / i1 in
                stack_machine instr (value::stk)
        | (Mod::instr, i1::i2::stk) -> 
            let value = i2 % i1 in
                stack_machine instr (value::stk)
        | (Equ::instr, i1::i2::stk) -> 
            let value = if i1 = i2 then 1 else 0 in
                stack_machine instr (value::stk)
        | (Neq::instr, i1::i2::stk) -> 
            let value = if i1 <> i2 then 1 else 0 in
                stack_machine instr (value::stk)
        | (Max::instr, i1::i2::stk) -> 
            let value = if i1 > i2 then i1 else i2 in
                stack_machine instr (value::stk)
        | (Min::instr, i1::i2::stk) -> 
            let value = if i1 < i2 then i1 else i2 in
                stack_machine instr (value::stk)
        | (Dub::instr, i::stk) -> 
            stack_machine instr (i::i::stk)
        | (Swp::instr, i1::i2::stk) -> 
            stack_machine instr (i2::i1::stk)

let compile expr =
    let rec compile_aux expr =
        match expr with
            | ConsInt i -> [Cst i]
            | Primitive(op, el, er) -> compile_aux el @ compile_aux er @ [instr_of_primitive op]
            | _ -> failwith "Unable to compile"

    and instr_of_primitive primitive =
        match primitive with
            | "+" -> Add
            | "-" -> Sub
            | "*" -> Mul 
            | "/" -> Div
            | "%" -> Mod
            | "==" -> Equ
            | "!=" -> Neq
            | "max" -> Max
            | "min" -> Min
            | _  -> failwith "unknown primitive" 

    in compile_aux expr

let e1 = Primitive("-", ConsInt 5, ConsInt 4)
let c1 = compile e1
let v1 = stack_machine c1 []

let e2 = Primitive("*", ConsInt(2), Primitive("-", ConsInt(5),  Primitive("+", ConsInt(9),  ConsInt(1))))
let c2 = compile e2
let v2 = stack_machine c2 []