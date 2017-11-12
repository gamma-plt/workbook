module Interpreter

open Syntax

type env = (string * int) list

(* =================================== *)
(* The expression language interpreter *)
(* =================================== *)

let rec lookup x (env : env) =
    match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x = y then v else lookup x r;;

let eval (expr : Syntax.expr) (env : env) =
    let rec eval_aux expr env =
        match expr with
            | CstI i -> i
            | Var v -> lookup v env
            | If(e1, e2, e3) ->
                let flag = (eval_aux e1 env) = 1
                if flag then eval_aux e2 env else eval_aux e3 env
            | Prim(_, _, _) -> eval_primitive expr env
            | Let(x, erhs, ebody) -> 
                let xval = eval_aux erhs env
                let env1 = (x, xval) :: env
                eval_aux ebody env1

    and eval_primitive expr env =
        let compute primitive =
            match primitive with
                | "+" -> (fun x y -> x + y)
                | "-" -> (fun x y -> x - y)
                | "*" -> (fun x y -> x * y)
                | _  -> failwith "unknown primitive" 

        in match expr with 
            | Prim(op, el, er) -> 
                let foo = compute op 
                foo (eval_aux el env) (eval_aux er env)
            | _  -> failwith "not a primitive" 

    in eval_aux expr env

let e1 = Let("z", CstI 17, Prim("+", Var "z", Var "z"))

let e2 = Let("z", CstI 17, 
             Prim("+", Let("z", CstI 22, Prim("*", CstI 100, Var "z")),
                       Var "z"))

let e3 = Let("z", Prim("-", CstI 5, CstI 4), 
             Prim("*", CstI 100, Var "z"))

let e4 = Prim("+", Prim("+", CstI 20, Let("z", CstI 17, 
                                          Prim("+", Var "z", CstI 2))),
                   CstI 30)

let e5 = Prim("*", CstI 2, Let("x", CstI 3, Prim("+", Var "x", CstI 4)))

let v1 = eval e1 []
let v2 = eval e2 []
let v3 = eval e3 []
let v4 = eval e4 []
let v5 = eval e5 []

let values = [v1; v2; v3; v4; v5]