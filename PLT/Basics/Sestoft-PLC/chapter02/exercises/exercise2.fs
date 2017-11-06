module Exercises

type Expr = 
    | CstI of int
    | Var of string
    | Let of (string * Expr) list * Expr
    | Prim of string * Expr * Expr

let mem x xs = List.exists (fun y -> y = x) xs

let rec freevars expr =
    let rec union xs ys =
        match xs with 
            | [] -> ys
            | x::xxs -> if mem x ys then union xxs ys else x :: (union xxs ys)

    let rec minus xs ys =
        match xs with 
            | [] -> []
            | x::xxs -> if mem x ys then minus xxs ys else x :: (minus xxs ys)

    let rec freevars_aux expr =
        match expr with
            | CstI i -> []
            | Var x  -> [x]
            | Let(defs, ebody) -> freevars_let defs ebody
            | Prim(ope, e1, e2) -> union (freevars e1) (freevars e2)

    and freevars_let defs ebody =
        let rec freevars_let_aux defs vars curr =
            match defs with 
                | [] -> vars
                | (x, rhs)::defss ->
                    let free_rhs = freevars_aux rhs (* freevars in the definition of x *)
                    let free_bdy = freevars_aux ebody (* freevars in the body *)
                    let free_exp = union free_rhs (minus free_bdy [x]) (* freevars of the whole expression *)
                    let free = minus free_exp curr (* taking into account prev defined vars in let *)
                    freevars_let_aux defss free (x::curr) (* add x to be used while freevaring pending defs *)

        in freevars_let_aux defs [] []

    in freevars_aux expr


let e1 = Let([("x1", Prim("+", CstI(5), CstI(7))); ("x2", Prim("*", Var("x1"), CstI(2)))], Prim("+", Var("x1"), Var("x2")))
let f1 = freevars e1

let e2 = Let([("x1", Prim("+", Var("x1"), CstI(7)))], Prim("+", Var("x1"), CstI(8)))
let f2 = freevars e2
