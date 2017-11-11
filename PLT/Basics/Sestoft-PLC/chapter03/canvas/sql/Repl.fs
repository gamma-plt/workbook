(* fsharpi -r FSharp.PowerPack.dll Syntax.fs Parser.fs Lexer.fs Repl.fs *)

module Repl

open System
open System.IO
open Microsoft.FSharp.Text.Lexing
open Syntax

(* Plain parsing from a string, with poor error reporting *)

let fromString (str : string) : stmt =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try 
        Parser.Main Lexer.Token lexbuf
    with 
        | exn -> 
            let pos = lexbuf.EndPos in 
                failwithf "%s near line %d, column %d\n" 
                    (exn.Message) (pos.Line+1) pos.Column

(* Exercise it *)

let e1 = fromString "SELECT name, salary * (1 - taxrate) FROM Employee"

let e2 = fromString "SELECT department, AVG(salary * (1 - taxrate)) FROM Employee"