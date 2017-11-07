module Syntax

type Expr =
    | ConstrInt of int
    | Variable of string
    | Let of string * Expr * Expr
    | Primitive of string * Expr * Expr