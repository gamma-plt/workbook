module Interpreter

type Expr = 
    | ConsInt of int
    | Variable of string
    | Primitive of (string * Expr * Expr)
    | Let of (string * Expr * Expr)
    | If of (Expr * Expr * Expr)

type Env = (string * int) list

let mem x xs = List.exists (fun y -> y = x) xs

let lookup (var : string) (env : Env) =
    let rec lookup_aux (env : Env) =
        match env with
            | [] -> failwith (var + " not found")
            | (n, v)::xs -> if n = var then v
                            else (lookup_aux xs)

    in lookup_aux env

let eval (expr : Expr) (env : Env) =
    let rec eval_aux expr env =
        match expr with
            | ConsInt i -> i
            | Variable v -> lookup v env
            | Primitive(_, _, _) -> eval_primitive expr env
            | If(e1, e2, e3) -> 
                (let flag = (eval_aux e1 env) in
                    if flag = 1 then (eval_aux e2 env) else (eval_aux e3 env))
            | Let(x, erhs, ebdy) ->
                let xval = eval_aux erhs env
                let extended_env = (x, xval) :: env
                in eval_aux ebdy extended_env

    and eval_primitive expr env =
        let compute primitive =
            match primitive with
                | "+" -> (fun x y -> x + y)
                | "-" -> (fun x y -> x - y)
                | "*" -> (fun x y -> x * y)
                | "/" -> (fun x y -> x / y)
                | "%" -> (fun x y -> x % y)
                | "==" -> (fun x y -> if x = y then 1 else 0)
                | "!=" -> (fun x y -> if x <> y then 1 else 0)
                | "max" -> (fun x y -> if x > y then x else y)
                | "min" -> (fun x y -> if x < y then x else y)
                | _  -> failwith "unknown primitive" 

        in match expr with 
                | Primitive(op, el, er) -> 
                    let foo = compute op in 
                        foo (eval_aux el env) (eval_aux er env)

    in eval_aux expr env

let closedin expr vars =
    let rec closedin_aux expr vars = 
        match expr with
            | ConsInt i -> true
            | Variable v -> mem v vars
            | Primitive(_, el, er) -> (closedin_aux el vars) && (closedin_aux er vars)
            | If(e1, e2, e3) -> 
                (closedin_aux e1 vars) && (closedin_aux e2 vars) && (closedin_aux e3 vars)
            | Let(x, erhs, ebdy) ->
                let extended_vars = x :: vars in 
                    (closedin_aux erhs vars) && (closedin_aux ebdy extended_vars)

    in closedin_aux expr vars

let freevars expr =
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
            | ConsInt i -> []
            | Variable v -> [v]
            | Primitive(_, el, er) -> union (freevars_aux el) (freevars_aux er)
            | If(e1, e2, e3) -> 
                union (freevars_aux e1) (union (freevars_aux e2) (freevars_aux e3))
            | Let(x, erhs, ebdy) ->
                union (freevars_aux erhs) (minus (freevars_aux ebdy) [x])

    in freevars_aux expr

let subst expr (env : (string * Expr) list) =

    let new_var =
        let n = ref 0
        let var_maker x = (n := 1 + !n; x + string (!n))
        var_maker

    let rec look_or_self env x = 
        match env with
            | []        -> Variable x
            | (y, e)::r -> if x = y then e else look_or_self r x

    let rec remove env x =
        match env with
            | []        -> []
            | (y, e)::r -> if x = y then r else (y, e)::(remove r x)

    let rec nsubst_aux expr env =
        match expr with
            | ConsInt i -> expr
            | Variable v -> look_or_self env v
            | Primitive(s, el, er) -> Primitive(s, (nsubst_aux el env), (nsubst_aux er env))
            | If(e1, e2, e3) -> 
                If((nsubst_aux e1 env), (nsubst_aux e2 env), (nsubst_aux e3 env))
            | Let(x, erhs, ebdy) ->
                let new_env = remove env x in
                    Let(x, nsubst_aux erhs env, nsubst_aux ebdy new_env)

    let rec subst_aux expr env =
        match expr with
            | ConsInt i -> expr
            | Variable v -> look_or_self env v
            | Primitive(s, el, er) -> Primitive(s, (subst_aux el env), (subst_aux er env))
            | If(e1, e2, e3) -> 
                If((subst_aux e1 env), (subst_aux e2 env), (subst_aux e3 env))
            | Let(x, erhs, ebdy) ->
                let new_x = new_var x in
                let new_env = (x, Variable new_x) :: remove env x in

                    Let(new_x, subst_aux erhs env, subst_aux ebdy new_env)

    in subst_aux expr env


let closed1 expr = closedin expr []
let closed2 expr = (freevars expr) = []


let initial_env = [("y", 3); ("x", 4)]

(* if x != 3 then x else 9 *)
let e1 = If(Primitive("!=", Variable("y"), ConsInt(3)), Variable("y"), ConsInt(9))

(* let x = 8 in (if (x % 2) == 0 then 2 else 0) *)
let e2 = Let("x", ConsInt(7), If(Primitive("==", Primitive("%", Variable("x"), ConsInt(2)), ConsInt(0)), ConsInt(2), ConsInt(1)))


let v1 = eval e1 initial_env
let v2 = eval e2 initial_env

let f1 = freevars e1
let f2 = freevars e2

let c11 = closed1 e1
let c12 = closed1 e2

let c21 = closed2 e1
let c22 = closed2 e2

let e3 = Let("y", ConsInt(7), Primitive("+", Variable("x"), ConsInt(4)))

let s3 = subst e3 [("x", Variable("y"))]
let v3 = eval e3 initial_env

let e4 = Primitive("+", Variable "y", Variable "z")

let s4 = subst e4 [("z", Primitive("-", ConsInt 5, ConsInt 4))]
let v4 = eval e3 initial_env

let e5 = Let("z", ConsInt 22, Primitive("/", Variable "y", Variable "z"))

let s5 = subst e5 [("z", Primitive("-", ConsInt 5, ConsInt 4))]
let v5 = eval e5 initial_env

let var_capture = subst e5 [("y", Variable "z")]