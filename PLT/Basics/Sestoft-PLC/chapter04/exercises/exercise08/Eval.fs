module Eval

open Syntax

type 'v env = (string * 'v) list

type value =
    | Int of int
    | TupV of value list
    | ListV of (value list)
    | Closure of string * string list * expr * value env

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec eval expr (env : value env) =
    match expr with
        | CstI i -> Int i
        | CstB b -> if b then Int 1 else Int 0
        | CstN -> ListV []
        | ConC(e1, CstN) -> ListV [eval e1 env]
        | ConC(e1, e2) -> 
            let v1 = eval e1 env
            let v2 = eval e2 env

            match (v1, v2) with
                | (Int _, ListV xs) -> ListV (v1::xs)
                | _ -> failwith "ConC should be applied to a list"

        | Match(e0, e1, (h, t, e2)) ->
            let v0 = eval e0 env

            match v0 with
                | ListV [] -> eval e1 env
                | ListV(x::xs) -> 
                    let extendedEnv = (h, x) :: (t, ListV xs) :: env
                    eval e2 extendedEnv
                | _ -> failwith "match expression, must be a list"

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
                | _ -> 
                    printf "unknown primitive %A applied to %A and %A\n" op val1 val2;
                    failwith ("unknown primitive " + op)
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

(* 
let len xs = 
    match xs with
        [] -> 0
        | h::t -> 1 + len t
    in len [1, 2, 3, 4, 5, 6] end
end
*)

let ex1 = 
    Letfun(
        "len",
        ["xs"],
        Match(
            Var "xs",
            CstI 0,
            (
                "h",
                "t",
                Prim(
                    "+", 
                    CstI 1, 
                    Call(
                        Var "len",
                        [Var "t"])))),
        Call(
            Var "len",
            [ConC(CstI 1,
                ConC(CstI 2,
                    ConC(CstI 3,
                        ConC(CstI 4,
                            ConC(CstI 5,
                                ConC(CstI 6, CstN))))))]))
