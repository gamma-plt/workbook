module Syntax

type tau = 
    | TypI
    | TypB
    | TypF of tau * tau

type tyexpr =
    | CstI of int
    | CstB of bool
    | Var of string
    | Let of string * tyexpr * tyexpr
    | Prim of string * tyexpr * tyexpr
    | If of tyexpr * tyexpr * tyexpr
    | Letfun of string * string * tau * tyexpr * tau * tyexpr
    | Call of tyexpr * tyexpr