let (let*) result f = match result with
  | Ok x -> f x
  | Error e -> Error e

let run_inner src =
    let chars = List.init (String.length src) (String.get src) in
    let line = ref 1 in

    let* tokens = Olox.Lex.scan chars [] [] line in
    List.iter (fun tok -> Printf.printf "%s, " @@ Olox.Lex.show_token tok) tokens;
    print_char '\n';

    let* ast = Olox.Parse.parse tokens in
    print_string @@ Olox.Parse.expr_to_str ast;
    print_char '\n';

    let* value = Olox.Eval.eval ast in
    print_string @@ Olox.Eval.val_to_str value;
    print_char '\n';
    Ok value

let run src = match run_inner src with
  | Error e ->
    print_string e;
    print_char '\n';
    1
  | Ok _ -> 0

let rec run_repl () =
    try
        let line = read_line () in
        ignore @@ run line;
        run_repl ()
    with End_of_file -> 0

let run_file fname =
    try
        let src = In_channel.with_open_text fname In_channel.input_all in
        run src
    with Sys_error msg ->
        Printf.eprintf "Could not read file: %s\n" msg;
        1

let main () =
    match Array.length Sys.argv with
    | 1 -> run_repl ()
    | 2 -> run_file Sys.argv.(1)
    | _ -> print_endline "Usage: olox [script]"; 1

let () = exit @@ main ()
