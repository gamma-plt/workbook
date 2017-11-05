module Compiler

type Instr =
    | Cst of int
    | Var of int 
    | Add 
    | Sub 
    | Mul 
    | Div 
    | Mod 
    | Equ 
    | Neq 
    | Max 
    | Min 
    | Pop 
    | Swp

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)
    | Let of (string * Expr * Expr)

type stackvalue =
    | Value                               (* A computed value *)
    | Bound of string                     (* A bound variable *)

let getindex v vars =
    let rec getindex_aux vars idx =
        match vars with
            | var::vs -> if v = var then idx else getindex_aux vs (idx + 1)
            | [] -> failwith "not found"
    in getindex_aux vars 0

let compile expr values =
    let rec compile_aux expr values =
        match expr with
            | ConsInt i -> [Cst i]
            | Variable v -> [Var (getindex (Bound v) values)]
            | Let(x, erhs, ebdy) ->
                let compile_erhs = compile_aux erhs values in
                let compile_ebdy = compile_aux ebdy (Bound x :: values) in
                    compile_erhs @ compile_ebdy @ [Swp; Pop]
            | Primitive(op, el, er) -> 
                let compile_el = compile_aux el values in
                let compile_er = compile_aux er (Value :: values) in
                let compile_op = instr_of_primitive op in 
                    compile_el @ compile_er @ [compile_op]
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

    in compile_aux expr values

let rec stack_machine instructions stack =
    match (instructions, stack) with
        | ([], v :: _) -> v 
        | ([], []) -> failwith "MACHINE: no result on stack" 
        | (Cst i :: instr, stk) -> stack_machine instr (i :: stk) 
        | (Var v :: instr, stk) -> stack_machine instr (List.nth stack v::stack) 
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

        | (Pop::instr, _::stk) -> stack_machine instr stk 
        | (Swp::instr, i1::i2::stk) ->  stack_machine instr (i2::i1::stk)


let write_asm instrs fname = 
    let text = String.concat "\n" (List.map string instrs)
    System.IO.File.WriteAllText(fname, text)

let e1 = Let("z", ConsInt(8), Primitive("+", Variable("z"), ConsInt(9)))
let c1 = compile e1 []
let v1 = stack_machine c1 []

let e2 = Let("y", ConsInt(7), Primitive("+", Variable("x"), Variable("y")))
let c2 = compile e2 [Bound("x")]
let v2 = stack_machine c2 [2]

(* x -> 10 y -> 20 z -> 30 *)
(* 2 + max(x, max(y, z)) + (let x = 5 in x * (7 - 5)) *)
(* 2 + max(10, max(20, 30)) + (5 * (7 - 5)) *)
(* 2 + max(10, 30) + (5 * 2) *)
(* 2 + 30 + 10 *)
(* 43 *)
let e3_max = Primitive("max", Variable("x"), Primitive("max", Variable("y"), Variable("z")))
let e3_let = Let("x", ConsInt(5), Primitive("*", Variable("x"), Primitive("-", ConsInt(7), ConsInt(5))))
let e3 = Primitive("+", ConsInt(2), Primitive("+", e3_max, e3_let))
let c3 = compile e3 [Bound("x"); Bound("y"); Bound("z")]
let v3 = stack_machine c3 [10; 20; 30]

write_asm c1 "chapter02/canvas/compiled/expression1.vm"
write_asm c2 "chapter02/canvas/compiled/expression2.vm"
write_asm c3 "chapter02/canvas/compiled/expression3.vm"
