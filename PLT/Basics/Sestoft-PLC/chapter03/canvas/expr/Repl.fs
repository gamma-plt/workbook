module Repl

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Syntax

(* Plain parsing from a string, with poor error reporting *)

let fromString (str : string) : Expr =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try 
        Parser.Main Lexer.Token lexbuf
    with 
        | exn -> 
            let pos = lexbuf.EndPos in
                failwithf "%s near line %d, column %d\n" 
                    (exn.Message) (pos.Line + 1) pos.Column

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
          Parser.Main Lexer.Token lexbuf
    with 
        | exn -> 
            let pos = lexbuf.EndPos in 
                failwithf "%s in file %s near line %d, column %d\n" 
                    (exn.Message) filename (pos.Line + 1) pos.Column

let read_line () =
    let input = System.Console.ReadLine()
    input

(* fsharpi -r FSharp.PowerPack.dll Syntax.fs Parser.fs Lexer.fs Repl.fs *)
let rec repl () =
    let x = read_line ()
    let expression = fromString x in
        printf "\n\n%A\n\n" expression
    if x = "#q" then () else repl ()

(* fsharpc -r FSharp.PowerPack.dll Syntax.fs Parser.fs Lexer.fs Repl.fs *)
(* mono Repl.exe *)
[<EntryPoint>]
let main args =
    let expression = fromFile args.[0]
    printfn "%A\n" expression
    0