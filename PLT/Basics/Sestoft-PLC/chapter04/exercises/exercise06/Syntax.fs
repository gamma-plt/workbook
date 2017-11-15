module Syntax

type expr =
    | CstI of int
    | CstB of bool
    | Var of string
    | Tup of (expr list)
    | Sel of int * expr
    | If of expr * expr * expr
    | Let of string * expr * expr
    | Prim of string * expr * expr
    | Letfun of string * (string list) * expr * expr
    | Call of expr * (expr list)