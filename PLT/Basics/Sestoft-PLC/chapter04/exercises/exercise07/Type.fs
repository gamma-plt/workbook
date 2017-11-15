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

(* let max (a : int) (b : int) : int = if a > b then a else b in max 2 3 end *)

let ex1 = 
    Letfun(
        "max",
        [("a", TypI); ("b", TypI)],
        TypI,
        If(
            Prim(">", Var "a", Var "b"),
            Var "a",
            Var "b"),
        Call(
            Var "max",
            [CstI 2; CstI 3]))

(* let fact (n : int) : int = if n < 2 then 1 else n * fact (n - 1) in fact 5 end *)

let ex2 =
    Letfun(
        "fact",
        [("n", TypI)],
        TypI,
        If(
            Prim("<", Var "n", CstI 2),
            CstI 1,
            Prim("*", Var "n", Call(Var "fact", [Prim("-", Var "n", CstI 1)]))),
        Call(
            Var "fact",
            [CstI 5]))

(* let max (a : int) (b : int) : int = 
    if a > b then a else b 
    in let max3 (a : int) (b : int) (c : int) : int = max a (max b c)
    in max3 25 6 43 end
end *)

let ex3 =
    Letfun(
        "max",
        [("a", TypI); ("b", TypI)],
        TypI,
        If(
            Prim(">", Var "a", Var "b"),
            Var "a",
            Var "b"),
        Letfun(
            "max3",
            [("a", TypI); ("b", TypI); ("c", TypI)],
            TypI,
            Call(
                Var "max",
                [Var "a"; Call(Var "max", [Var "b"; Var "c"])]),
            Call(
                Var "max3",
                [CstI 25; CstI 6; CstI 43])))


(* let notDivisible (d : int) (n : int) : bool = n % d <> 0 in
    let test (a : int) (b : int) (c : int) : bool =
        if a <= b then (notDivisible a c) && (test (a + 1) b c)
        else notDivisible b c in
            let prime (n : int) : bool = (n = 2) || (test 2 (n - 1) n) in
                prime 11
            end
    end
end
*)

let ex4 = 
    Letfun(
        "notDivisible",
        [("d", TypI); ("n", TypI)],
        TypB,
        Prim("<>", Prim("%", Var "n", Var "d"), CstI 0),
        Letfun(
            "test",
            [("a", TypI); ("b", TypI); ("c", TypI)],
            TypB,
            If(
                Prim("<=", Var "a", Var "b"),
                Prim(
                    "&&",
                    Call(
                        Var "notDivisible",
                        [Var "a"; Var "c"]),
                    Call(
                        Var "test",
                        [Prim("+", Var "a", CstI 1); Var "b"; Var "c"])),
                Call(
                    Var "notDivisible",
                    [Var "b"; Var "c"])),
            Letfun(
                "prime",
                [("n", TypI)],
                TypB,
                Prim(
                    "||",
                    Prim("=", Var "n", CstI 2),
                    Call(
                        Var "test",
                        [CstI 2; Prim("-", Var "n", CstI 1); Var "n"])),
                Call(
                    Var "prime",
                    [CstI 11]))))

let types = List.map typeCheck [ex1; ex2; ex3; ex4]