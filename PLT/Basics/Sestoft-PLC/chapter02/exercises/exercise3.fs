module Exercises

(* =================================== *)
(* The expression language interpreter *)
(* =================================== *)

type Expr = 
    | CstI of int
    | Var of string
    | Let of (string * Expr) list * Expr
    | Prim of string * Expr * Expr

type Env = (string * int) list

let rec lookup x (env : Env) =
    match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x = y then v else lookup x r;;

let eval (expr : Expr) (env : Env) =
    let rec eval_aux expr env =
        match expr with
            | CstI i -> i
            | Var v -> lookup v env
            | Prim(_, _, _) -> eval_primitive expr env
            | Let(defs, ebdy) ->
                let extended_env = extend_env_let defs @ env
                eval_aux ebdy extended_env

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

    and extend_env_let defs = 
        let rec extend_env_let_aux defs env = 
            match defs with 
                | [] -> env 
                | (x, body)::defss -> 
                    let xval = eval_aux body env 
                    let extended_env = (x, xval) :: env 
                    extend_env_let_aux defss extended_env 

        in extend_env_let_aux defs env

    in eval_aux expr env

(* =================================== *)
(* The expression language compiler    *)
(* =================================== *)

type TExpr =
    | TCstI of int
    | TVar of int                       
    | TLet of TExpr * TExpr               
    | TPrim of string * TExpr * TExpr

let rec getindex vs x = 
    match vs with 
        | []    -> failwith "Variable not found"
        | y::yr -> if x=y then 0 else 1 + getindex yr x

let tcomp expr (cenv : string list) =
    let rec tcomp_aux expr cenv =
        match expr with
            | CstI i -> TCstI i
            | Var x  -> TVar (getindex cenv x)
            | Prim(ope, e1, e2) -> TPrim(ope, tcomp_aux e1 cenv, tcomp_aux e2 cenv)
            | Let(defs, ebdy) ->
                tcomp_let defs ebdy cenv

    and tcomp_let defs ebdy cenv =
        let rec tcomp_let_aux defs extended_cenv =
            match defs with
                | (x, erhs)::[] -> 
                    TLet(tcomp_aux erhs extended_cenv, tcomp_aux ebdy (x::extended_cenv))
                | (x, erhs)::defss ->
                    TLet(tcomp_aux erhs extended_cenv, tcomp_let_aux defss (x::extended_cenv))

        in tcomp_let_aux defs cenv

    in tcomp_aux expr cenv

let rec teval texpr (renv : int list) =
    match texpr with
        | TCstI i -> i
        | TVar n  -> List.nth renv n
        | TLet(erhs, ebody) -> 
            let xval = teval erhs renv
            let renv1 = xval :: renv 
            teval ebody renv1 
        | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
        | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
        | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
        | TPrim _            -> failwith "unknown primitive"

(* =================================== *)
(* Testing the expression language     *)
(* =================================== *)

let env = [("z", 7)]

let e1 = Let([("x1", Prim("+", CstI(5), CstI(7))); ("x2", Prim("*", Var("x1"), CstI(2)))], Prim("+", Var("x1"), Var("x2")))
let i1 = eval e1 env
let te1 = tcomp e1 []
let vte1 = teval te1 []


let e2 = Let([("x1", Prim("+", Var("z"), CstI(7)))], Prim("+", Var("x1"), CstI(8)))
let i2 = eval e2 env
let te2 = tcomp e2 ["z"]
let vte2 = teval te2 [7]


let e3 = Let([("x1", CstI 2); ("x2", Var("x1")); ("x3", CstI 4)], Prim("+", Var("x1"), Prim("+", Var("x2"), Var("x3"))))
let i3 = eval e3 env
let te3 = tcomp e3 []
let vte3 = teval te3 []