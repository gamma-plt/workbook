module Exercises

type Expr = 
    | CstI of int
    | Var of string
    | Let of (string * Expr) list * Expr
    | Prim of string * Expr * Expr

type Env = (string * int) list

let rec lookup x (env : Env) =
    match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x = y then v else lookup x r;;

let eval (expr : Expr) (env : Env) =
    let rec eval_aux expr env =
        match expr with
            | CstI i -> i
            | Var v -> lookup v env
            | Prim(_, _, _) -> eval_primitive expr env
            | Let(defs, ebdy) ->
                let extended_env = (extend_env_let defs env) @ env
                eval_aux ebdy extended_env

    and eval_primitive expr env =
        let compute primitive =
            match primitive with
                | "+" -> (fun x y -> x + y)
                | "-" -> (fun x y -> x - y)
                | "*" -> (fun x y -> x * y)
                | _  -> failwith "unknown primitive" 

        in match expr with 
            | Prim(op, el, er) -> 
                let foo = compute op 
                foo (eval_aux el env) (eval_aux er env)

    and extend_env_let defs env = 
        let rec extend_env_let_aux defs env = 
            match defs with 
                | [] -> env 
                | (x, body)::defss -> 
                    let xval = eval_aux body env 
                    let extended_env = (x, xval) :: env 
                    extend_env_let_aux defss extended_env 

        in extend_env_let_aux defs env

    in eval_aux expr env

let e1 = Let([("x1", Prim("+", CstI(5), CstI(7))); ("x2", Prim("*", Var("x1"), CstI(2)))], Prim("+", Var("x1"), Var("x2")))
let v1 = eval e1 []