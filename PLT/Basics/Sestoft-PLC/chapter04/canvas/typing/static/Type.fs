module Type

open Syntax

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec typ expr (env : tau env) =
    match expr with
        | CstI i -> TypI
        | CstB b -> TypB
        | Var x -> lookup env x
        | Prim(op, e1, e2) ->
            let t1 = typ e1 env
            let t2 = typ e2 env
            match (op, t1, t2) with
                | ("+", TypI, TypI) -> TypI
                | ("*", TypI, TypI) -> TypI
                | ("-", TypI, TypI) -> TypI
                | ("=", TypI, TypI) -> TypB
                | ("<", TypI, TypI) -> TypB
                | ("&", TypB, TypB) -> TypB
                | _ -> failwith ("unknown primitive " + op + " ,or type error")
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
        | Letfun(f, x, tx, fbody, tb, letbody) ->
            let ft = TypF(tx, tb)
            let fBodyEnv = (x, tx) :: (f, ft) :: env
            let letBodyEnv = (f, ft) :: env
            let fCheckedType = typ fbody fBodyEnv

            if fCheckedType = tb then typ letbody letBodyEnv
            else failwith ("error while typing function definition")

        | Call(Var f, arg) ->
            match lookup env f with
                | TypF(tx, tb) ->
                    if typ arg env = tx then tb
                    else failwith ("wrong type of argument: " + f)
                | _ -> failwith ("unknown function" + f)
        | Call _ -> failwith "not a first.order function"


let typeCheck e = typ e []


(* Examples of successful type checking *)

let ex1 = Letfun("f1", "x", TypI, Prim("+", Var "x", CstI 1), TypI,
                 Call(Var "f1", CstI 12))

(* Factorial *)

let ex2 = Letfun("fac", "x", TypI,
                 If(Prim("=", Var "x", CstI 0),
                    CstI 1,
                    Prim("*", Var "x", 
                              Call(Var "fac", 
                                   Prim("-", Var "x", CstI 1)))),
                 TypI,
                 Let("n", CstI 7, Call(Var "fac", Var "n")))

let ex3 = Let("b", Prim("=", CstI 1, CstI 2),
              If(Var "b", CstI 3, CstI 4))

let ex4 = Let("b", Prim("=", CstI 1, CstI 2),
              If(Var "b", Var "b", CstB false))

let ex5 = If(Prim("=", CstI 11, CstI 12), CstI 111, CstI 666)

let ex6 = Letfun("inf", "x", TypI, Call(Var "inf", Var "x"), TypI,
                 Call(Var "inf", CstI 0))

let types = List.map typeCheck [ex1; ex2; ex3; ex4; ex5; ex6]

(* Examples of type errors; should throw exception when run: *)

let exErr1 = Let("b", Prim("=", CstI 1, CstI 2),
                 If(Var "b", Var "b", CstI 6))

let exErr2 = Letfun("f", "x", TypB, If(Var "x", CstI 11, CstI 22), TypI,
                    Call(Var "f", CstI 0))

let exErr3 = Letfun("f", "x", TypB, Call(Var "f", CstI 22), TypI,
                    Call(Var "f", CstB true))

let exErr4 = Letfun("f", "x", TypB, If(Var "x", CstI 11, CstI 22), TypB,
                    Call(Var "f", CstB true))