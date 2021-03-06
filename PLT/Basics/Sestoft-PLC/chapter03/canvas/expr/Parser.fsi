// Signature file for parser generated by fsyacc
module Parser
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
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Syntax.Expr) 
