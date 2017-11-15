module Syntax

type expr =
    | CstI of int
    | CstB of bool
    | CstN
    | ConC of expr * expr
    | Match of expr * expr * (string * string * expr)
    | Var of string
    | Tup of (expr list)
    | Sel of int * expr
    | Let of string * expr * expr
    | Prim of string * expr * expr
    | If of expr * expr * expr
    | Letfun of string * (string list) * expr * expr
    | Call of expr * (expr list)