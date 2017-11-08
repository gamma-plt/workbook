module Compiler

open Syntax

type env = (string * int) list

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
        | y::yr -> if x = y then 0 else 1 + getindex yr x

(* =================================== *)
(* The expression language compiler    *)
(* =================================== *)

type stackvalue = 
    | Value                               (* A computed value *)
    | Bound of string                     (* A bound variable *)

let rec scomp (e : Syntax.expr) (cenv : stackvalue list) : sinstr list =
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

let write_bte (bytes : int list) fname = 
    let text = String.concat "\n" (List.map string bytes)
    System.IO.File.WriteAllText(fname, text)