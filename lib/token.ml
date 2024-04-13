type t =
  (* Single character tokens *)
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two character tokens*)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literlas *)
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of float
  (* Keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | ILLEGAL
  | EOF
[@@deriving show { with_path = false }]
