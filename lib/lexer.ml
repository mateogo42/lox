module Lexer = struct
  include Token

  type t = {
    input : string;
    tokens : Token.t list;
    pos : int;
    ch : char option;
    line : int;
  }

  let init source =
    {
      input = source;
      tokens = [];
      pos = 0;
      ch = Some (String.get source 0);
      line = 1;
    }

  let add_token lexer token = { lexer with tokens = lexer.tokens @ [ token ] }

  let read_char lexer =
    if lexer.pos >= String.length lexer.input - 1 then { lexer with ch = None }
    else
      let pos = lexer.pos + 1 in
      { lexer with pos; ch = Some (String.get lexer.input pos) }

  let peek_char lexer =
    if lexer.pos >= String.length lexer.input - 1 then None
    else Some (String.get lexer.input (lexer.pos + 1))

  let is_ident ch =
    match ch with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

  let is_digit ch = match ch with '0' .. '9' -> true | _ -> false

  let is_whitespace ch =
    match ch with
    | None -> false
    | Some c -> c == ' ' || c == '\t' || c == '\r' || c == '\n'

  let lookup_ident ident =
    match ident with
    | "and" -> AND
    | "class" -> CLASS
    | "else" -> ELSE
    | "false" -> FALSE
    | "for" -> FOR
    | "fun" -> FUN
    | "if" -> IF
    | "nil" -> NIL
    | "or" -> OR
    | "print" -> PRINT
    | "return" -> RETURN
    | "super" -> SUPER
    | "this" -> THIS
    | "true" -> TRUE
    | "var" -> VAR
    | "while" -> WHILE
    | _ -> IDENTIFIER ident

  let read_while lexer cond =
    let rec loop lexer =
      if cond lexer then loop @@ read_char lexer else lexer
    in
    let lexer = loop lexer in
    lexer

  let read_string lexer =
    let lexer = read_char lexer in
    let pos = lexer.pos in
    let lexer =
      read_while lexer (fun l ->
          match l.ch with
          | Some ch -> ( match ch with '"' -> false | _ -> true)
          | None -> false)
    in
    let str = String.sub lexer.input pos (lexer.pos - pos) in
    add_token lexer @@ STRING str

  let read_ident lexer =
    let pos = lexer.pos in
    let lexer =
      read_while lexer (fun l ->
          match l.ch with Some ch -> is_ident ch | None -> false)
    in
    let token = lookup_ident (String.sub lexer.input pos (lexer.pos - pos)) in
    add_token lexer token

  let read_number lexer =
    let parse_number lexer =
      let pos = lexer.pos in
      let lexer =
        read_while lexer (fun l ->
            match peek_char l with Some ch -> is_digit ch | None -> false)
      in
      (lexer, String.sub lexer.input pos (lexer.pos - pos + 1))
    in
    let lexer, decimal = parse_number lexer in
    let decimal = String.trim decimal in
    match peek_char lexer with
    | Some '.' ->
        let lexer = read_char lexer in
        let lexer, fractional = parse_number lexer in
        let fractional = String.trim fractional in
        let lexer = read_char lexer in
        let number = float_of_string @@ Fmt.str "%s%s" decimal fractional in
        add_token lexer @@ NUMBER number
    | _ ->
        let len = String.length decimal in
        let number = float_of_string @@ String.sub decimal 0 (len - 0) in
        add_token lexer @@ NUMBER number

  let next_token (lexer : t) : t * bool =
    match lexer.ch with
    | None -> (lexer, true)
    | Some ch -> (
        match ch with
        | ' ' | '\r' -> (read_char lexer, false)
        | '\n' -> (read_char { lexer with line = lexer.line + 1 }, false)
        | '=' -> (
            match peek_char lexer with
            | Some '=' ->
                (read_char @@ read_char @@ add_token lexer EQUAL_EQUAL, false)
            | _ -> (read_char @@ add_token lexer EQUAL, false))
        | '+' -> (read_char @@ add_token lexer PLUS, false)
        | '-' -> (read_char @@ add_token lexer MINUS, false)
        | '!' -> (
            match peek_char lexer with
            | Some '=' ->
                (read_char @@ read_char @@ add_token lexer BANG_EQUAL, false)
            | _ -> (read_char @@ add_token lexer BANG, false))
        | '*' -> (read_char @@ add_token lexer STAR, false)
        | '/' -> (read_char @@ add_token lexer SLASH, false)
        | '>' -> (read_char @@ add_token lexer GREATER, false)
        | '<' -> (read_char @@ add_token lexer LESS, false)
        | '{' -> (read_char @@ add_token lexer LBRACE, false)
        | '}' -> (read_char @@ add_token lexer RBRACE, false)
        | '(' -> (read_char @@ add_token lexer LPAREN, false)
        | ')' -> (read_char @@ add_token lexer RPAREN, false)
        | ',' -> (read_char @@ add_token lexer COMMA, false)
        | ';' -> (read_char @@ add_token lexer SEMICOLON, false)
        | '"' -> (read_char @@ read_string lexer, false)
        | ch when is_ident ch -> (read_ident lexer, false)
        | ch when is_digit ch -> (read_char @@ read_number lexer, false)
        | _ -> (read_char @@ add_token lexer ILLEGAL, false))

  let scan_tokens lexer =
    let rec loop lexer =
      let lexer, is_eof = next_token lexer in
      match is_eof with true -> lexer | false -> loop lexer
    in
    let lexer = loop lexer in
    lexer
end
