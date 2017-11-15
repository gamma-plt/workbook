module Eval

open Syntax

type 'v env = (string * 'v) list

type value =
    | Int of int
    | TupV of value list
    | Closure of string * string list * expr * value env

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec eval expr (env : value env) =
    match expr with
        | CstI i -> Int i
        | CstB b -> if b then Int 1 else Int 0
        | Var v -> lookup env v
        | Tup exprs -> TupV (List.map (fun expr -> eval expr env) exprs)
        | Sel(i, expr) ->
            match eval expr env with
                | TupV exprs -> 
                    let length = List.length exprs
                    if i < 1 || i > length then failwith "operator and operand don't agree"
                    else List.item (i - 1) exprs
                | _ -> failwith "# should be applied to a tuple"
        | Prim(op, e1, e2) ->
            let val1 = eval e1 env
            let val2 = eval e2 env
            match (op, val1, val2) with
                | ("+", Int v1, Int v2) -> Int(v1 + v2)
                | ("-", Int v1, Int v2) -> Int(v1 - v2)
                | ("*", Int v1, Int v2) -> Int(v1 * v2)
                | ("/", Int v1, Int v2) -> Int(v1 / v2)
                | ("%", Int v1, Int v2) -> Int(v1 % v2)
                | ("=", Int v1, Int v2) -> if v1 = v2 then Int(1) else Int(0)
                | (">", Int v1, Int v2) -> if v1 > v2 then Int(1) else Int(0)
                | ("<", Int v1, Int v2) -> if v1 < v2 then Int(1) else Int(0)
                | ("<>", Int v1, Int v2) -> if v1 <> v2 then Int(1) else Int(0)
                | ("<=", Int v1, Int v2) -> if v1 <= v2 then Int(1) else Int(0)
                | (">=", Int v1, Int v2) -> if v1 >= v2 then Int(1) else Int(0)
                | _ -> failwith ("unknown primitive " + op)
        | Let(x, erhs, ebdy) ->
            let xval = eval erhs env
            let extendedEnv = (x, xval) :: env
            eval ebdy extendedEnv
        | If(e1, e2, e3) ->
            let flag = eval e1 env
            if flag = Int(1) then eval e2 env
            else eval e3 env
        | Letfun(f, xs, fbody, letbody) ->
            let bodyenv = (f, Closure(f, xs, fbody, env)) :: env
            eval letbody bodyenv
        | Call(Var f, args) ->
            let closure = lookup env f
            match closure with
                | Closure(f, xs, fbody, fprevenv) ->
                    if List.length xs = List.length args then
                        let xsvals = List.map (fun x -> eval x env) args
                        let zippedValues = List.zip xs xsvals
                        let fbodyenv = zippedValues @ ((f, closure) :: fprevenv)
                        eval fbody fbodyenv
                    else
                        failwith ("arity mismatch while calling " + f)
                | _ -> failwith "eval Call: not a function"
        | Call _ -> failwith "not a first.order function"

(* let max a b = if a > b then a else b in max 7 4 *)

let example1 = 
    Letfun(
        "max", 
        ["a"; "b"], 
        If(
            Prim(">", Var "a", Var "b"), 
            Var "a", 
            Var "b"), 
        Call(
            Var "max", 
            [CstI 7; CstI 4]))

(* let pow b n = if n = 1 then b else b * (pow b (n - 1)) in pow 2 3 *)

let example2 =
    Letfun(
        "pow", 
        ["b"; "n"], 
        If(
            Prim("=", Var "n", CstI 1), 
            Var "b", 
            Prim(
                "*",
                Var "b",
                Call(
                    Var "pow",
                    [Var "b"; Prim("-", Var "n", CstI 1)]))), 
        Call(
            Var "pow", 
            [CstI 2; CstI 3]))

(* (1, 2, 2 + 3, let pow b n = if n = 1 then b else b * (pow b (n - 1)) in pow 2 3) *)

let example3 =
    Tup [CstI 1; CstI 2; Prim("+", CstI 2, CstI 3); example2]

(* #1(1, 2, 2 + 3) *)

let example4 =
    Sel(1, Tup [CstI 1; CstI 2; Prim("+", CstI 2, CstI 3)])

(* let t = (1 + 2, false, 5 > 8) in if #3(t) then #1(t) else 14 end *)

let example5 =
    Let(
        "t",
        Tup [Prim("+", CstI 1, CstI 2); CstB false; Prim(">", CstI 5, CstI 8)],
        If(
            Sel(3, Var "t"),
            Sel(1, Var "t"),
            CstI 14))

(* let max xy = if #1(xy) > #2(xy) then #1(xy) else #2(xy) in max (3, 88) end *)

let example6 =
    Letfun(
        "max",
        ["xy"],
        If(
            Prim(">", Sel(1, Var "xy"), Sel(2, Var "xy")),
            Sel(1, Var "xy"),
            Sel(2, Var "xy")),
        Call(
            Var "max",
            [Tup [CstI 3; CstI 88]]))
