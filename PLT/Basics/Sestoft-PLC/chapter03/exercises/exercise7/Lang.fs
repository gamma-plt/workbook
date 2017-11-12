module Lang

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open Syntax
open Interpreter

(* Plain parsing from a string, with poor error reporting *)

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

(* fsharpi -r FSharp.PowerPack.dll Syntax.fs Parser.fs Lexer.fs Interpreter.fs Lang.fs *)
(* Lang.main "examples/ex1.exp" *)

let main filename =
    let expression = fromFile filename
    Interpreter.eval expression []
