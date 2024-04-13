module Lexer = Lexer.Lexer
module Token = Token

let report line where message =
  Fmt.pr "[line %d] Error%s: %s" line where message

let error line message = report line "" message
