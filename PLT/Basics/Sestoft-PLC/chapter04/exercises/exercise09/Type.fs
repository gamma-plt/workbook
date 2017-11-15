module Type

open Syntax

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec typ expr (env : typ env) =
    match expr with
        | CstI i -> TypI
        | CstB b -> TypB
        | List (x::xs) ->
            let xtyp = typ x env
            let types = Set.ofList (List.map (fun x -> typ x env) (x::xs))
            if Set.count types = 1 then TypL xtyp
            else failwithf "This expression was expected to have type %A\n" xtyp
        | Var x -> lookup env x
        | Prim(op, e1, e2) ->
            let t1 = typ e1 env
            let t2 = typ e2 env
            match (op, t1, t2) with
                | ("+", TypI, TypI) -> TypI
                | ("*", TypI, TypI) -> TypI
                | ("-", TypI, TypI) -> TypI
                | ("%", TypI, TypI) -> TypI
                | ("=", TypI, TypI) -> TypB
                | ("<>", TypI, TypI) -> TypB
                | ("<", TypI, TypI) -> TypB
                | ("<=", TypI, TypI) -> TypB
                | (">", TypI, TypI) -> TypB
                | (">=", TypI, TypI) -> TypB
                | ("&&", TypB, TypB) -> TypB
                | ("||", TypB, TypB) -> TypB
                | _ -> failwith ("unknown primitive " + op + ", or type error")
        | Let(x, erhs, ebdy) ->
            let xtyp = typ erhs env
            let extendedEnv = (x, xtyp) :: env
            typ ebdy extendedEnv
        | If(e1, e2, e3) ->
            let te1 = typ e1 env
            match te1 with 
                | TypB -> 
                    let te2, te3 = typ e2 env, typ e3 env
                    match (te2, te3) with
                        | (a, b) when a = b -> a
                        | _ -> failwith ("error while typing if expression, branch types mismatch")
                | _ -> failwith ("error while typing if expression, flag type must be boolean")
        | Letfun(f, typedxs, tb, fbody, letbody) ->
            let typesxs = (List.map (fun x -> snd x) typedxs)
            let ft = TypF(typesxs, tb)
            let fBodyEnv = typedxs @ ((f, ft) :: env)
            let letBodyEnv = (f, ft) :: env
            let fCheckedType = typ fbody fBodyEnv

            if fCheckedType = tb then typ letbody letBodyEnv
            else failwith ("error while typing function definition")

        | Call(Var f, args) ->
            let targs = (List.map (fun x -> typ x env) args)
            match lookup env f with
                | TypF(txs, tb) ->
                    if List.length targs = List.length txs then 
                        if targs = txs then tb
                        else failwith ("wrong type of argument: " + f)
                    else failwith ("arity missmatch while calling: " + f)
                | _ -> failwith ("unknown function" + f)
        | Call _ -> failwith "not a first.order function"


let typeCheck e = typ e []

let ex1 = List [CstI 2; CstI 3; CstI 4; Prim("+", CstI 2, CstI 6)]

let ex2 = List [CstB true; CstI 3; CstI 4; Prim("+", CstI 2, CstI 6)]