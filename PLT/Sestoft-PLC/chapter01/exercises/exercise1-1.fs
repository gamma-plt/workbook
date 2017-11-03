module Exercises

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)
    | If of (Expr * Expr * Expr)

type Env = (string * int) list

let lookup (var : string) (env : Env) =
    let rec lookup_aux (env : Env) =
        match env with
            | [] -> failwith (var + " not found")
            | (n, v)::xs -> if n = var then v
                            else (lookup_aux xs)

    in lookup_aux env

let eval (expr : Expr) (env : Env) =
    let rec eval_aux expr =
        match expr with
            | ConsInt i -> i
            | Variable v -> lookup v env
            | Primitive("+", el, er) -> eval_aux el + eval_aux er
            | Primitive("-", el, er) -> eval_aux el - eval_aux er
            | Primitive("*", el, er) -> eval_aux el * eval_aux er
            | Primitive("/", el, er) -> eval_aux el / eval_aux er
            | Primitive("%", el, er) -> eval_aux el % eval_aux er
            | Primitive("max", el, er) -> 
                (let vl, vr = eval_aux el, eval_aux er in 
                    if vl > vr then vl else vr)
            | Primitive("min", el, er) -> 
                (let vl, vr = eval_aux el, eval_aux er in 
                    if vl < vr then vl else vr)
            | Primitive("==", el, er) -> 
                (let vl, vr = eval_aux el, eval_aux er in 
                    if vl = vr then 1 else 0)
            | Primitive("!=", el, er) -> 
                (let vl, vr = eval_aux el, eval_aux er in 
                    if vl <> vr then 1 else 0)
            | Primitive _             -> failwith "unknown primitive"
            | If(e1, e2, e3) -> 
                (let flag = eval_aux e1 in
                    if flag = 0 then eval_aux e3 else eval_aux e2)

    in eval_aux expr


let initial_env = [("a", 5); ("b", 8); ("c", 9)]

let e1 = If(Primitive("!=", Variable("a"), ConsInt(3)), Variable("c"), Variable("b"))
let v1 = eval e1 initial_env