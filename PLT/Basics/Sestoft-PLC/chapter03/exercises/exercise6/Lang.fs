module Lang

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open Syntax
open Compiler

(* Plain parsing from a string, with poor error reporting *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
          (Parser.Main Lexer.Token lexbuf, filename + "vm")
    with 
        | exn -> 
            let pos = lexbuf.EndPos in 
                failwithf "%s in file %s near line %d, column %d\n" 
                    (exn.Message) filename (pos.Line + 1) pos.Column

(* fsharpi -r FSharp.PowerPack.dll Syntax.fs Parser.fs Lexer.fs Compiler.fs Lang.fs *)
(* Lang.compile "program.exp" *)

let compile filename =
    let expression, finalfile = fromFile filename
    let compiled = Compiler.scomp expression []
    let bytecode = Compiler.assemble compiled

    in Compiler.write_bte bytecode finalfile
