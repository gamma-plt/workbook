module Interpreter

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)

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
            | Primitive _            -> failwith "unknown primitive"

    in eval_aux expr


let initial_env = [("a", 5); ("b", 8); ("c", 9)]