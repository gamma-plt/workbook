module Compiler

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)
    | Let of (string * Expr * Expr)
    | If of (Expr * Expr * Expr)

type TExpr =
    | TConsInt of int
    | TVariable of int
    | TPrimitive of (string * TExpr * TExpr)
    | TLet of (TExpr * TExpr)
    | TIf of (TExpr * TExpr * TExpr)

type Env = (string * int) list

let mem x xs = List.exists (fun y -> y = x) xs

let getindex v vars =
    let rec getindex_aux vars idx =
        match vars with
            | var::vs -> if v = var then idx else getindex_aux vs (idx + 1)
            | [] -> failwith (v + " not found")

    in getindex_aux vars 0

(* vars is the compile time env *)
let rec compile expr (vars : string list) =
    match expr with
        | ConsInt i -> TConsInt i 
        | Variable v -> TVariable (getindex v vars)
        | Primitive(op, el, er) -> TPrimitive(op, compile el vars, compile er vars)
        | If(e1, e2, e3) -> TIf(compile e1 vars, compile e2 vars, compile e3 vars)
        | Let(x, erhs, ebdy) -> 
            let extended_vars = x :: vars in
                TLet(compile erhs vars, compile ebdy extended_vars)

(* renv is the run-time env *)
let rec teval texpr renv =
    let rec teval_aux texpr env =
        match texpr with
            | TConsInt i -> i
            | TVariable v -> List.nth env v
            | TPrimitive(_, _, _) -> eval_tprimitive texpr env
            | TLet(erhs, ebdy) -> 
                let xval = teval_aux erhs env in 
                let extended_renv = xval :: renv in 
                    teval_aux ebdy extended_renv 
            | TIf(e1, e2, e3) ->
                let flag = (teval_aux e1 env) in 
                    if flag = 1 then (teval_aux e2 env) else (teval_aux e3 env)

    and eval_tprimitive tprimitive env =
        let compute primitive =
            match primitive with
                | "+" -> (fun x y -> x + y)
                | "-" -> (fun x y -> x - y)
                | "*" -> (fun x y -> x * y)
                | "/" -> (fun x y -> x / y)
                | "%" -> (fun x y -> x % y)
                | "==" -> (fun x y -> if x = y then 1 else 0)
                | "!=" -> (fun x y -> if x <> y then 1 else 0)
                | "max" -> (fun x y -> if x > y then x else y)
                | "min" -> (fun x y -> if x < y then x else y)
                | _  -> failwith "unknown primitive" 

        in match tprimitive with 
                | TPrimitive(op, el, er) -> 
                    let foo = compute op in 
                        foo (teval_aux el env) (teval_aux er env)

    in teval_aux texpr renv

let initial_env = [("y", 3); ("x", 4)]

let vars = ["y"; "x"]
let vals = [3; 4]

(* if x != 3 then x else 9 *)
let e1 = If(Primitive("!=", Variable("x"), ConsInt(3)), Variable("y"), ConsInt(9))

(* let z = 8 in (if (z % 2) == 0 then 2 else 1) *)
let e2 = Let("z", ConsInt(8), If(Primitive("==", Primitive("%", Variable("z"), ConsInt(2)), ConsInt(0)), ConsInt(2), ConsInt(1)))

let te1 = compile e1 vars

let te2 = compile e2 vars

let ve1 = teval te1 vals
let ve2 = teval te2 vals