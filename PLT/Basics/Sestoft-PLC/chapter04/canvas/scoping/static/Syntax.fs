module Syntax

type expr =
    | CstI of int
    | CstB of bool
    | Var of string
    | Let of string * expr * expr
    | Prim of string * expr * expr
    | If of expr * expr * expr
    | Letfun of string * string * expr * expr
    | Call of expr * expr

type 'v env = (string * 'v) list

type value =
    | Int of int
    | Closure of string * string * expr * value env

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
                | "=" -> if v1 = v2 then 1 else 0
                | "<" -> if v1 < v2 then 1 else 0
                | _ -> failwith ("unknown primitive " + op)
        | Let(x, erhs, ebdy) ->
            let xval = Int(eval erhs env)
            let extendedEnv = (x, xval) :: env
            eval ebdy extendedEnv
        | If(e1, e2, e3) ->
            let flag = eval e1 env
            if flag = 1 then eval e2 env
            else eval e3 env
        | Letfun(f, x, fbody, letbody) ->
            let bodyenv = (f, Closure(f, x, fbody, env)) :: env
            eval letbody bodyenv
        | Call(Var f, arg) ->
            let closure = lookup env f
            match closure with
                | Closure(f, x, fbody, fprevenv) ->
                    let xval = Int(eval arg env)
                    let fbodyenv = (x, xval) :: (f, closure) :: fprevenv
                    eval fbody fbodyenv
                | _ -> failwith "eval Call: not a function"
        | Call _ -> failwith "not a first.order function"
