open Olox

let run source =
  let lexer = Lexer.init source in
  let lexer = Lexer.scan_tokens lexer in
  List.iter (fun t -> print_endline (Token.show t)) lexer.tokens;
  false

let run_prompt () =
  let rec loop () =
    print_string "> ";
    flush stdout;
    let code = try Some (input_line stdin) with End_of_file -> None in
    let _ = match code with None -> exit 0 | Some source -> run source in
    loop ()
  in
  loop ()

let run_file _filename =
  let ic = open_in _filename in
  let content = In_channel.input_all ic in
  let had_error = run content in
  if had_error then exit 1

let main () =
  let args = Sys.argv in
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64

let () = main ()
