module Eval

open Syntax

type 'v env = (string * 'v) list

type value =
    | Int of int
    | Closure of string * string list * expr * value env

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec eval expr (env : value env) =
    match expr with
        | CstI i -> i
        | CstB b -> if b then 1 else 0
        | Var v -> 
            match lookup env v with
                | Int i -> i
                | _ -> failwith "eval var"
        | Prim(op, e1, e2) ->
            let v1 = eval e1 env
            let v2 = eval e2 env
            match op with
                | "+" -> v1 + v2
                | "-" -> v1 - v2
                | "*" -> v1 * v2
                | "/" -> v1 / v2
                | "%" -> v1 % v2
                | "=" -> if v1 = v2 then 1 else 0
                | ">" -> if v1 > v2 then 1 else 0
                | "<" -> if v1 < v2 then 1 else 0
                | "!=" -> if v1 <> v2 then 1 else 0
                | "<=" -> if v1 <= v2 then 1 else 0
                | ">=" -> if v1 >= v2 then 1 else 0
                | _ -> failwith ("unknown primitive " + op)
        | Let(x, erhs, ebdy) ->
            let xval = Int(eval erhs env)
            let extendedEnv = (x, xval) :: env
            eval ebdy extendedEnv
        | If(e1, e2, e3) ->
            let flag = eval e1 env
            if flag = 1 then eval e2 env
            else eval e3 env
        | Letfun(f, xs, fbody, letbody) ->
            let bodyenv = (f, Closure(f, xs, fbody, env)) :: env
            eval letbody bodyenv
        | Call(Var f, args) ->
            let closure = lookup env f
            match closure with
                | Closure(f, xs, fbody, fprevenv) ->
                    if List.length xs = List.length args then
                        let xsvals = List.map (fun x -> Int(eval x env)) args
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
