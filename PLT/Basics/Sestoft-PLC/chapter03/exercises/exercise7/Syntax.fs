module Syntax

type expr = 
    | CstI of int
    | Var of string
    | If of expr * expr * expr
    | Let of string * expr * expr
    | Prim of string * expr * expr