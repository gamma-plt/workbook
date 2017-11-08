// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 3 "Parser.fsy"
 open Syntax 
# 8 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAR
  | RPAR
  | END
  | IN
  | LET
  | PLUS
  | MINUS
  | TIMES
  | EQUAL
  | NAME of (string)
  | CONSTR_INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_END
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_EQUAL
    | TOKEN_NAME
    | TOKEN_CONSTR_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAR  -> 1 
  | RPAR  -> 2 
  | END  -> 3 
  | IN  -> 4 
  | LET  -> 5 
  | PLUS  -> 6 
  | MINUS  -> 7 
  | TIMES  -> 8 
  | EQUAL  -> 9 
  | NAME _ -> 10 
  | CONSTR_INT _ -> 11 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAR 
  | 2 -> TOKEN_RPAR 
  | 3 -> TOKEN_END 
  | 4 -> TOKEN_IN 
  | 5 -> TOKEN_LET 
  | 6 -> TOKEN_PLUS 
  | 7 -> TOKEN_MINUS 
  | 8 -> TOKEN_TIMES 
  | 9 -> TOKEN_EQUAL 
  | 10 -> TOKEN_NAME 
  | 11 -> TOKEN_CONSTR_INT 
  | 14 -> TOKEN_end_of_input
  | 12 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_Expr 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_Expr 
    | 9 -> NONTERM_Expr 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 14 
let _fsyacc_tagOfErrorTerminal = 12

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | END  -> "END" 
  | IN  -> "IN" 
  | LET  -> "LET" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | EQUAL  -> "EQUAL" 
  | NAME _ -> "NAME" 
  | CONSTR_INT _ -> "CONSTR_INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CONSTR_INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 7us; 65535us; 0us; 2us; 8us; 9us; 13us; 14us; 15us; 16us; 21us; 18us; 22us; 19us; 23us; 20us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 4us; 1us; 7us; 8us; 9us; 1us; 1us; 1us; 2us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 5us; 4us; 5us; 7us; 8us; 9us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 6us; 4us; 6us; 7us; 8us; 9us; 1us; 6us; 4us; 6us; 7us; 8us; 9us; 1us; 6us; 4us; 7us; 7us; 8us; 9us; 4us; 7us; 8us; 8us; 9us; 4us; 7us; 8us; 9us; 9us; 1us; 7us; 1us; 8us; 1us; 9us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 9us; 11us; 13us; 15us; 17us; 19us; 21us; 26us; 28us; 30us; 32us; 34us; 39us; 41us; 46us; 48us; 53us; 58us; 63us; 65us; 67us; |]
let _fsyacc_action_rows = 24
let _fsyacc_actionTableElements = [|5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 0us; 49152us; 4us; 32768us; 0us; 3us; 6us; 22us; 7us; 23us; 8us; 21us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 1us; 32768us; 11us; 7us; 0us; 16388us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 4us; 32768us; 2us; 10us; 6us; 22us; 7us; 23us; 8us; 21us; 0us; 16389us; 1us; 32768us; 10us; 12us; 1us; 32768us; 9us; 13us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 4us; 32768us; 4us; 15us; 6us; 22us; 7us; 23us; 8us; 21us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 4us; 32768us; 3us; 17us; 6us; 22us; 7us; 23us; 8us; 21us; 0us; 16390us; 0us; 16391us; 1us; 16392us; 8us; 21us; 1us; 16393us; 8us; 21us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; 5us; 32768us; 1us; 8us; 5us; 11us; 7us; 6us; 10us; 4us; 11us; 5us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 6us; 7us; 12us; 13us; 14us; 15us; 17us; 18us; 24us; 29us; 30us; 32us; 34us; 40us; 45us; 51us; 56us; 57us; 58us; 60us; 62us; 68us; 74us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 2us; 3us; 7us; 3us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 16387us; 65535us; 16388us; 65535us; 65535us; 16389us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 140 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 149 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 20 "Parser.fsy"
                                _1
                   )
# 20 "Parser.fsy"
                 : Syntax.Expr));
# 160 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                   Variable _1
                   )
# 23 "Parser.fsy"
                 : 'Expr));
# 171 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "Parser.fsy"
                                          ConstrInt _1
                   )
# 24 "Parser.fsy"
                 : 'Expr));
# 182 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "Parser.fsy"
                                               ConstrInt (- _2)
                   )
# 25 "Parser.fsy"
                 : 'Expr));
# 193 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                             _2
                   )
# 26 "Parser.fsy"
                 : 'Expr));
# 204 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                                          Let(_2, _4, _6)
                   )
# 27 "Parser.fsy"
                 : 'Expr));
# 217 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                             Primitive("*", _1, _3)
                   )
# 28 "Parser.fsy"
                 : 'Expr));
# 229 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                            Primitive("+", _1, _3)
                   )
# 29 "Parser.fsy"
                 : 'Expr));
# 241 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                             Primitive("-", _1, _3)
                   )
# 30 "Parser.fsy"
                 : 'Expr));
|]
# 254 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 15;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Syntax.Expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))