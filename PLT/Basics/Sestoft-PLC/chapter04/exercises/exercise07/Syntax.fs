module Syntax

type typ = 
    | TypI
    | TypB
    | TypF of (typ list) * typ

type tyexpr =
    | CstI of int
    | CstB of bool
    | Var of string
    | Let of string * tyexpr * tyexpr
    | Prim of string * tyexpr * tyexpr
    | If of tyexpr * tyexpr * tyexpr
    | Letfun of string * (string * typ) list * typ * tyexpr * tyexpr
    | Call of tyexpr * (tyexpr list)