module Exercises

type Expr = 
    | ConsInt of int
    | Variable of string
    | Add of Expr * Expr 
    | Mul of Expr * Expr 
    | Sub of Expr * Expr 

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
            | Add(e1, e2) -> 
                (let v1, v2 = eval_aux e1, eval_aux e2 in
                    v1 + v2)
            | Mul(e1, e2) -> 
                (let v1, v2 = eval_aux e1, eval_aux e2 in
                    v1 * v2)
            | Sub(e1, e2) -> 
                (let v1, v2 = eval_aux e1, eval_aux e2 in
                    v1 - v2)

    in eval_aux expr

let simplify (expr : Expr) =
    let rec simplify_aux expr =
        match expr with
            | Add(e, ConsInt(0)) -> e
            | Add(ConsInt(0), e) -> e
            | Add(e1, e2) -> (let s1, s2 = simplify_aux e1, simplify_aux e2 in
                    match (s1, s2) with
                        | (_, ConsInt(0)) -> simplify_aux(Add(s1, s2))
                        | (ConsInt(0), _) -> simplify_aux(Add(s1, s2))
                        | _ -> Add(s1, s2))
            | Add(e1, e2) -> Add(simplify_aux e1, simplify_aux e2)
            | Sub(e, ConsInt(0)) -> e
            | Sub(e1, e2) when e1 = e2 -> ConsInt(0)
            | Sub(e1, e2) -> Sub(simplify_aux e1, simplify_aux e2)
            | Mul(e, ConsInt(0)) -> ConsInt(0)
            | Mul(ConsInt(0), e) -> ConsInt(0)
            | Mul(e, ConsInt(1)) -> e
            | Mul(ConsInt(1), e) -> e
            | Mul(e1, e2) -> (let s1, s2 = simplify_aux e1, simplify_aux e2 in
                    match (s1, s2) with
                        | (_, ConsInt(0)) -> simplify_aux(Mul(s1, s2))
                        | (ConsInt(0), _) -> simplify_aux(Mul(s1, s2))
                        | (_, ConsInt(1)) -> simplify_aux(Mul(s1, s2))
                        | (ConsInt(1), _) -> simplify_aux(Mul(s1, s2))
                        | _ -> Mul(s1, s2))
            | e -> e
    in simplify_aux expr

let fmt (expr : Expr) =
    let rec fmt_aux expr =
        match expr with
            | ConsInt i -> string i
            | Variable v -> v
            | Add(e1, e2) -> 
                (let f1, f2 = fmt_aux e1, fmt_aux e2 in
                    ("(" + f1 + " + " + f2 + ")"))
            | Mul(e1, e2) -> 
                (let f1, f2 = fmt_aux e1, fmt_aux e2 in
                    ("(" + f1 + " * " + f2 + ")"))
            | Sub(e1, e2) -> 
                (let f1, f2 = fmt_aux e1, fmt_aux e2 in
                    ("(" + f1 + " - " + f2 + ")"))
    in fmt_aux expr


let initial_env = [("x", 1); ("y", 2); ("z", 3); ("v", 4); ("w", 5)]

let e1 = Sub(Variable("v"), Add(Variable("w"), Variable("z")))

let e2 = Mul(ConsInt(2), Sub(Variable("v"),  Add(Variable("w"),  Variable("z"))))

let e3 = Add(Variable("x"), Add(Variable("y"), Add(Variable("z"), Variable("v"))))

let e4 = Add(Variable("x"), Add(Variable("y"), Add(Variable("z"), ConsInt(0))))

let e5 = Mul(Add(ConsInt(1), ConsInt(0)), Add(Variable("x"), ConsInt(0)))

let v1 = eval e1 initial_env
let v2 = eval e2 initial_env
let v3 = eval e3 initial_env

let f1 = fmt e1
let f2 = fmt e2
let f3 = fmt e3

let s1 = simplify e1
let s2 = simplify e2
let s3 = simplify e3
let s4 = simplify e4
let s5 = simplify e5
