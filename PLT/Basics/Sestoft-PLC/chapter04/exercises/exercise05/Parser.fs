// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 3 "Parser.fsy"
 open Syntax; 
# 8 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | AND
  | OR
  | LPAR
  | RPAR
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | ELSE
  | END
  | FALSE
  | IF
  | IN
  | LET
  | NOT
  | THEN
  | TRUE
  | CSTBOOL of (bool)
  | NAME of (string)
  | CSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_ELSE
    | TOKEN_END
    | TOKEN_FALSE
    | TOKEN_IF
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_NOT
    | TOKEN_THEN
    | TOKEN_TRUE
    | TOKEN_CSTBOOL
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_AppExpr
    | NONTERM_ParamExpr
    | NONTERM_ArgExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | AND  -> 1 
  | OR  -> 2 
  | LPAR  -> 3 
  | RPAR  -> 4 
  | EQ  -> 5 
  | NE  -> 6 
  | GT  -> 7 
  | LT  -> 8 
  | GE  -> 9 
  | LE  -> 10 
  | PLUS  -> 11 
  | MINUS  -> 12 
  | TIMES  -> 13 
  | DIV  -> 14 
  | MOD  -> 15 
  | ELSE  -> 16 
  | END  -> 17 
  | FALSE  -> 18 
  | IF  -> 19 
  | IN  -> 20 
  | LET  -> 21 
  | NOT  -> 22 
  | THEN  -> 23 
  | TRUE  -> 24 
  | CSTBOOL _ -> 25 
  | NAME _ -> 26 
  | CSTINT _ -> 27 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_AND 
  | 2 -> TOKEN_OR 
  | 3 -> TOKEN_LPAR 
  | 4 -> TOKEN_RPAR 
  | 5 -> TOKEN_EQ 
  | 6 -> TOKEN_NE 
  | 7 -> TOKEN_GT 
  | 8 -> TOKEN_LT 
  | 9 -> TOKEN_GE 
  | 10 -> TOKEN_LE 
  | 11 -> TOKEN_PLUS 
  | 12 -> TOKEN_MINUS 
  | 13 -> TOKEN_TIMES 
  | 14 -> TOKEN_DIV 
  | 15 -> TOKEN_MOD 
  | 16 -> TOKEN_ELSE 
  | 17 -> TOKEN_END 
  | 18 -> TOKEN_FALSE 
  | 19 -> TOKEN_IF 
  | 20 -> TOKEN_IN 
  | 21 -> TOKEN_LET 
  | 22 -> TOKEN_NOT 
  | 23 -> TOKEN_THEN 
  | 24 -> TOKEN_TRUE 
  | 25 -> TOKEN_CSTBOOL 
  | 26 -> TOKEN_NAME 
  | 27 -> TOKEN_CSTINT 
  | 30 -> TOKEN_end_of_input
  | 28 -> TOKEN_error
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
    | 10 -> NONTERM_Expr 
    | 11 -> NONTERM_Expr 
    | 12 -> NONTERM_Expr 
    | 13 -> NONTERM_Expr 
    | 14 -> NONTERM_Expr 
    | 15 -> NONTERM_Expr 
    | 16 -> NONTERM_Expr 
    | 17 -> NONTERM_Expr 
    | 18 -> NONTERM_Expr 
    | 19 -> NONTERM_AtExpr 
    | 20 -> NONTERM_AtExpr 
    | 21 -> NONTERM_AtExpr 
    | 22 -> NONTERM_AtExpr 
    | 23 -> NONTERM_AtExpr 
    | 24 -> NONTERM_AppExpr 
    | 25 -> NONTERM_AppExpr 
    | 26 -> NONTERM_ParamExpr 
    | 27 -> NONTERM_ParamExpr 
    | 28 -> NONTERM_ArgExpr 
    | 29 -> NONTERM_ArgExpr 
    | 30 -> NONTERM_Const 
    | 31 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 30 
let _fsyacc_tagOfErrorTerminal = 28

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EQ  -> "EQ" 
  | NE  -> "NE" 
  | GT  -> "GT" 
  | LT  -> "LT" 
  | GE  -> "GE" 
  | LE  -> "LE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | ELSE  -> "ELSE" 
  | END  -> "END" 
  | FALSE  -> "FALSE" 
  | IF  -> "IF" 
  | IN  -> "IN" 
  | LET  -> "LET" 
  | NOT  -> "NOT" 
  | THEN  -> "THEN" 
  | TRUE  -> "TRUE" 
  | CSTBOOL _ -> "CSTBOOL" 
  | NAME _ -> "NAME" 
  | CSTINT _ -> "CSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NE  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | CSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 23us; 65535us; 0us; 2us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 32us; 14us; 33us; 15us; 34us; 16us; 35us; 17us; 36us; 18us; 37us; 19us; 38us; 20us; 39us; 21us; 40us; 22us; 41us; 23us; 42us; 24us; 43us; 25us; 44us; 26us; 49us; 27us; 50us; 28us; 53us; 29us; 54us; 30us; 56us; 31us; 26us; 65535us; 0us; 4us; 4us; 60us; 5us; 60us; 6us; 4us; 8us; 4us; 10us; 4us; 12us; 4us; 32us; 4us; 33us; 4us; 34us; 4us; 35us; 4us; 36us; 4us; 37us; 4us; 38us; 4us; 39us; 4us; 40us; 4us; 41us; 4us; 42us; 4us; 43us; 4us; 44us; 4us; 49us; 4us; 50us; 4us; 53us; 4us; 54us; 4us; 56us; 4us; 60us; 60us; 23us; 65535us; 0us; 5us; 6us; 5us; 8us; 5us; 10us; 5us; 12us; 5us; 32us; 5us; 33us; 5us; 34us; 5us; 35us; 5us; 36us; 5us; 37us; 5us; 38us; 5us; 39us; 5us; 40us; 5us; 41us; 5us; 42us; 5us; 43us; 5us; 44us; 5us; 49us; 5us; 50us; 5us; 53us; 5us; 54us; 5us; 56us; 5us; 3us; 65535us; 4us; 58us; 5us; 59us; 60us; 61us; 2us; 65535us; 48us; 52us; 62us; 63us; 26us; 65535us; 0us; 45us; 4us; 45us; 5us; 45us; 6us; 45us; 8us; 45us; 10us; 45us; 12us; 45us; 32us; 45us; 33us; 45us; 34us; 45us; 35us; 45us; 36us; 45us; 37us; 45us; 38us; 45us; 39us; 45us; 40us; 45us; 41us; 45us; 42us; 45us; 43us; 45us; 44us; 45us; 49us; 45us; 50us; 45us; 53us; 45us; 54us; 45us; 56us; 45us; 60us; 45us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 27us; 54us; 78us; 82us; 85us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 14us; 1us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 1us; 2us; 2us; 24us; 2us; 3us; 25us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 4us; 14us; 4us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 1us; 5us; 14us; 5us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 13us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 14us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 15us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 16us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 17us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 18us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 21us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 21us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 22us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 22us; 14us; 6us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 23us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 2us; 21us; 22us; 2us; 21us; 22us; 1us; 21us; 1us; 21us; 1us; 21us; 1us; 22us; 1us; 22us; 1us; 22us; 1us; 22us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 25us; 2us; 26us; 27us; 1us; 27us; 2us; 28us; 29us; 1us; 29us; 1us; 30us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 19us; 21us; 24us; 27us; 29us; 44us; 46us; 61us; 63us; 78us; 80us; 95us; 110us; 125us; 140us; 155us; 170us; 185us; 200us; 215us; 230us; 245us; 260us; 275us; 290us; 305us; 320us; 335us; 350us; 365us; 367us; 369us; 371us; 373us; 375us; 377us; 379us; 381us; 383us; 385us; 387us; 389us; 391us; 393us; 395us; 398us; 401us; 403us; 405us; 407us; 409us; 411us; 413us; 415us; 417us; 419us; 421us; 423us; 426us; 428us; 431us; 433us; 435us; |]
let _fsyacc_action_rows = 66
let _fsyacc_actionTableElements = [|7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 49152us; 14us; 32768us; 0us; 3us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 0us; 16385us; 5us; 16386us; 3us; 56us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 5us; 16387us; 3us; 56us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 23us; 8us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 16us; 10us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 13us; 16388us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 5us; 16389us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 5us; 16390us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 5us; 16391us; 1us; 43us; 2us; 44us; 13us; 34us; 14us; 35us; 15us; 36us; 0us; 16392us; 0us; 16393us; 0us; 16394us; 11us; 16395us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16396us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16397us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16398us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16399us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 11us; 16400us; 1us; 43us; 2us; 44us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 3us; 16401us; 13us; 34us; 14us; 35us; 15us; 36us; 3us; 16402us; 13us; 34us; 14us; 35us; 15us; 36us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 20us; 50us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 17us; 51us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 20us; 54us; 14us; 32768us; 1us; 43us; 2us; 44us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 17us; 55us; 14us; 32768us; 1us; 43us; 2us; 44us; 4us; 57us; 5us; 37us; 6us; 38us; 7us; 39us; 8us; 40us; 9us; 41us; 10us; 42us; 11us; 32us; 12us; 33us; 13us; 34us; 14us; 35us; 15us; 36us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 16403us; 0us; 16404us; 1us; 32768us; 26us; 48us; 2us; 32768us; 5us; 49us; 26us; 62us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 16405us; 1us; 32768us; 5us; 53us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 16406us; 7us; 32768us; 3us; 56us; 12us; 12us; 19us; 6us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 16407us; 0us; 16408us; 0us; 16409us; 5us; 16410us; 3us; 56us; 21us; 47us; 25us; 65us; 26us; 46us; 27us; 64us; 0us; 16411us; 1us; 16412us; 26us; 62us; 0us; 16413us; 0us; 16414us; 0us; 16415us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 8us; 9us; 24us; 25us; 31us; 37us; 45us; 60us; 68us; 83us; 91us; 105us; 113us; 119us; 125us; 131us; 132us; 133us; 134us; 146us; 158us; 170us; 182us; 194us; 206us; 210us; 214us; 229us; 244us; 259us; 274us; 289us; 297us; 305us; 313us; 321us; 329us; 337us; 345us; 353us; 361us; 369us; 377us; 385us; 393us; 394us; 395us; 397us; 400us; 408us; 416us; 417us; 419us; 427us; 435us; 436us; 444us; 445us; 446us; 447us; 453us; 454us; 456us; 457us; 458us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 6us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; 7us; 8us; 3us; 2us; 2us; 1us; 2us; 1us; 2us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16403us; 16404us; 65535us; 65535us; 65535us; 65535us; 16405us; 65535us; 65535us; 65535us; 16406us; 65535us; 16407us; 16408us; 16409us; 65535us; 16411us; 65535us; 16413us; 16414us; 16415us; |]
let _fsyacc_reductions ()  =    [| 
# 263 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 272 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                                                 _1                     
                   )
# 33 "Parser.fsy"
                 : Syntax.expr));
# 283 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                                 _1                     
                   )
# 37 "Parser.fsy"
                 : Syntax.expr));
# 294 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                                _1                     
                   )
# 38 "Parser.fsy"
                 : Syntax.expr));
# 305 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                If(_2, _4, _6)         
                   )
# 39 "Parser.fsy"
                 : Syntax.expr));
# 318 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                                                Prim("-", CstI 0, _2)  
                   )
# 40 "Parser.fsy"
                 : Syntax.expr));
# 329 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                                Prim("+",  _1, _3)     
                   )
# 41 "Parser.fsy"
                 : Syntax.expr));
# 341 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                                Prim("-",  _1, _3)     
                   )
# 42 "Parser.fsy"
                 : Syntax.expr));
# 353 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                                Prim("*",  _1, _3)     
                   )
# 43 "Parser.fsy"
                 : Syntax.expr));
# 365 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                                Prim("/",  _1, _3)     
                   )
# 44 "Parser.fsy"
                 : Syntax.expr));
# 377 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                                Prim("%",  _1, _3)     
                   )
# 45 "Parser.fsy"
                 : Syntax.expr));
# 389 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                Prim("=",  _1, _3)     
                   )
# 46 "Parser.fsy"
                 : Syntax.expr));
# 401 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                                Prim("<>", _1, _3)     
                   )
# 47 "Parser.fsy"
                 : Syntax.expr));
# 413 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                                                Prim(">",  _1, _3)     
                   )
# 48 "Parser.fsy"
                 : Syntax.expr));
# 425 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                                Prim("<",  _1, _3)     
                   )
# 49 "Parser.fsy"
                 : Syntax.expr));
# 437 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                                Prim(">=", _1, _3)     
                   )
# 50 "Parser.fsy"
                 : Syntax.expr));
# 449 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                                                Prim("<=", _1, _3)     
                   )
# 51 "Parser.fsy"
                 : Syntax.expr));
# 461 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                                 If(_1, _3, CstB(false))
                   )
# 52 "Parser.fsy"
                 : Syntax.expr));
# 473 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                                 If(_1, CstB(true), _3) 
                   )
# 53 "Parser.fsy"
                 : Syntax.expr));
# 485 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                    _1                     
                   )
# 57 "Parser.fsy"
                 : Syntax.expr));
# 496 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                                   Var _1                 
                   )
# 58 "Parser.fsy"
                 : Syntax.expr));
# 507 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                                   Let(_2, _4, _6)        
                   )
# 59 "Parser.fsy"
                 : Syntax.expr));
# 520 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                                   Letfun(_2, _3, _5, _7) 
                   )
# 60 "Parser.fsy"
                 : Syntax.expr));
# 534 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                                   _2                     
                   )
# 61 "Parser.fsy"
                 : Syntax.expr));
# 545 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                                                   Call(_1, _2)           
                   )
# 65 "Parser.fsy"
                 : Syntax.expr));
# 557 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Parser.fsy"
                                                                  Call(_1, _2)           
                   )
# 66 "Parser.fsy"
                 : Syntax.expr));
# 569 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                                                   [_1]                   
                   )
# 70 "Parser.fsy"
                 : Syntax.expr list));
# 580 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.expr list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                                                   _1 :: _2               
                   )
# 71 "Parser.fsy"
                 : Syntax.expr list));
# 592 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                                                                 [_1]                   
                   )
# 75 "Parser.fsy"
                 : string list));
# 603 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                                                 _1 :: _2               
                   )
# 76 "Parser.fsy"
                 : string list));
# 615 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                                                 CstI(_1)               
                   )
# 80 "Parser.fsy"
                 : Syntax.expr));
# 626 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Parser.fsy"
                                                                 CstB(_1)               
                   )
# 81 "Parser.fsy"
                 : Syntax.expr));
|]
# 638 "Parser.fs"
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
    numTerminals = 31;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Syntax.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))