let run src = print_endline src; 0

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
