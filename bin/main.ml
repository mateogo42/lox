let run source = print_endline source

let run_prompt () =
  let rec loop () =
    print_string "> ";
    flush stdout;
    let code = try Some (input_line stdin) with End_of_file -> None in
    (match code with None -> exit 0 | Some source -> run source);
    loop ()
  in
  loop ()

let run_file _filename =
  let ic = open_in _filename in
  let content = In_channel.input_all ic in
  run content

let main () =
  let args = Sys.argv in
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64

let () = main ()
