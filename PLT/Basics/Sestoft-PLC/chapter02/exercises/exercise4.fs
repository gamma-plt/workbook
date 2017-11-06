module Exercises

(* =================================== *)
(* The expression language interpreter *)
(* =================================== *)

type Expr = 
    | CstI of int
    | Var of string
    | Let of string * Expr * Expr
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

    in eval_aux expr env

(* =================================== *)
(* The expression language VM          *)
(* =================================== *)

type sinstr =
    | SCstI of int                        (* push integer           *)
    | SVar of int                         (* push variable from env *)
    | SAdd                                (* pop args, push sum     *)
    | SSub                                (* pop args, push diff.   *)
    | SMul                                (* pop args, push product *)
    | SPop                                (* pop value/unbind var   *)
    | SSwap                               (* exchange top and next  *)

let mem x xs = List.exists (fun y -> y = x) xs

let rec getindex vs x = 
    match vs with 
        | []    -> failwith "Variable not found"
        | y::yr -> if x=y then 0 else 1 + getindex yr x;;
 
let rec seval (inss : sinstr list) (stack : int list) =
    match (inss, stack) with
        | ([], v :: _) -> v
        | ([], [])     -> failwith "seval: no result on stack"
        | (SCstI i :: insr,          stk) -> seval insr (i :: stk) 
        | (SVar i  :: insr,          stk) -> seval insr (List.nth stk i :: stk) 
        | (SAdd    :: insr, i2::i1::stkr) -> seval insr (i1+i2 :: stkr)
        | (SSub    :: insr, i2::i1::stkr) -> seval insr (i1-i2 :: stkr)
        | (SMul    :: insr, i2::i1::stkr) -> seval insr (i1*i2 :: stkr)
        | (SPop    :: insr,    _ :: stkr) -> seval insr stkr
        | (SSwap   :: insr, i2::i1::stkr) -> seval insr (i1::i2::stkr)
        | _ -> failwith "seval: too few operands on stack"

type stackvalue = 
    | Value                               (* A computed value *)
    | Bound of string;;                   (* A bound variable *)

let rec scomp (e : Expr) (cenv : stackvalue list) : sinstr list =
    match e with
        | CstI i -> [SCstI i]
        | Var x  -> [SVar (getindex cenv (Bound x))]
        | Let(x, erhs, ebody) -> 
            scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
        | Prim("+", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd] 
        | Prim("-", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub] 
        | Prim("*", e1, e2) -> 
            scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul] 
        | Prim _ -> failwith "scomp: unknown operator"

(* =================================== *)
(* Testing the expression assembler    *)
(* =================================== *)

let rec assemble instructions =
    match instructions with
        | [] -> []
        | SCstI i :: instr -> [0; i] @ (assemble instr)
        | SVar v :: instr -> [1; v] @ assemble instr
        | SAdd :: instr -> [2] @ assemble instr
        | SSub :: instr -> [3] @ assemble instr 
        | SMul :: instr -> [4] @ assemble instr
        | SPop :: instr -> [5] @ assemble instr 
        | SSwap :: instr -> [6] @ assemble instr

(* =================================== *)
(* Testing the expression language     *)
(* =================================== *)

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

let c1 = scomp e1 []
let exec1 = seval c1 []

let c2 = scomp e2 []
let exec2 = seval c2 []

let c3 = scomp e3 []
let exec3 = seval c3 []

let c4 = scomp e4 []
let exec4 = seval c4 []

let c5 = scomp e5 []
let exec5 = seval c5 []

let values = [v1; v2; v3; v4; v5]