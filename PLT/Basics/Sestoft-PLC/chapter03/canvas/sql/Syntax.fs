module Syntax

type constant =
    | CstI of int                         (* Integer constant               *)
    | CstB of bool                        (* Boolean constant               *)
    | CstS of string                      (* String constant                *)

type stmt =
    | Select of expr list * string list   (* SELECT a, b, c FROM A, B, C    *)

and column =
    | Column of string                    (* A column name: c               *)
    | TableColumn of string * string      (* A qualified column: t.c        *)

and expr = 
    | Star                                (* Select all                     *)
    | Cst of constant                     (* Constant                       *)
    | ColumnExpr of column                (* Column                         *)
    | Prim of string * expr list          (* Built-in function              *)