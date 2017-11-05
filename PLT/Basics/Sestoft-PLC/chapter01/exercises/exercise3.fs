module Exercises

type Expr = 
    | ConsInt of int
    | Variable of string
    | Add of Expr * Expr 
    | Mul of Expr * Expr 
    | Sub of Expr * Expr

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

let precedence expr =
    match expr with
        | ConsInt(_) -> 0
        | Variable(_) -> 0
        | Add(_, _) -> 6
        | Sub(_, _) -> 6
        | Mul(_, _) -> 5

let sign expr =
    match expr with
        | ConsInt(_) -> ""
        | Variable(_) -> ""
        | Add(_, _) -> "+"
        | Sub(_, _) -> "-"
        | Mul(_, _) -> "*"

let fmt2 expr =
    let rec fmt_aux expr =
        match expr with
            | ConsInt i -> string i
            | Variable v -> v
            | Add(e1, e2) -> fmt_binop (precedence expr) e1 e2
            | Sub(e1, e2) -> fmt_binop (precedence expr) e1 e2
            | Mul(e1, e2) -> fmt_binop (precedence expr) e1 e2

    and fmt_binop p e1 e2 =
        let pe1, pe2 = precedence e1, precedence e2 in
            let s, f1, f2 = sign expr, fmt_aux e1, fmt_aux e2 in
                match (pe1, pe2) with
                    | (pl, pr) when pl > p && pr < p -> "(" + f1 + ")" + s + f2q
                    | (pl, pr) when pl < p && pr = p -> f1 + s + "(" + f2 + ")"
                    | _ -> f1 + s + f2

    in fmt_aux expr

let e1 = Sub(Variable("v"), Add(Variable("w"), Variable("z")))

let e2 = Mul(ConsInt(2), Sub(Variable("v"),  Add(Variable("w"),  Variable("z"))))

let e3 = Add(Variable("x"), Add(Variable("y"), Add(Variable("z"), Variable("v"))))

let e4 = Mul(Sub(Variable("a"), Variable("b")), Variable("c"))

let e5 = Sub(Mul(Variable("a"), Variable("b")), Variable("c"))

let e6 = Sub(Sub(Variable("a"), Variable("b")), Variable("c"))

let e7 = Sub(Variable("a"), Sub(Variable("b"), Variable("c")))

let f1 = fmt e1
let f2 = fmt e2
let f3 = fmt e3
let f4 = fmt e4
let f5 = fmt e5
let f6 = fmt e6
let f7 = fmt e7

let ft1 = fmt2 e1
let ft2 = fmt2 e2
let ft3 = fmt2 e3
let ft4 = fmt2 e4
let ft5 = fmt2 e5
let ft6 = fmt2 e6
let ft7 = fmt2 e7