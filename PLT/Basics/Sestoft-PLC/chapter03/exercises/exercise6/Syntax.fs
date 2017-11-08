module Syntax

type expr = 
    | CstI of int
    | Var of string
    | Let of string * expr * expr
    | Prim of string * expr * expr