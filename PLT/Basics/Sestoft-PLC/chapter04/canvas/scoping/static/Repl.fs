(* fsharpi -r FSharp.PowerPack.dll Syntax.fs Interpreter.fs Parser.fs Lexer.fs Repl.fs *)

module Repl

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Syntax
open Interpreter

(* Plain parsing from a file, with poor error reporting *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
        eval (Parser.Main Lexer.Token lexbuf) []
    with 
        | exn -> 
            let pos = lexbuf.EndPos in 
                failwithf "%s in file %s near line %d, column %d\n" 
                    (exn.Message) filename (pos.Line + 1) pos.Column