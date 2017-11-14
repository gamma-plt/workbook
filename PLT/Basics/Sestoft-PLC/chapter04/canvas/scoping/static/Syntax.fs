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
