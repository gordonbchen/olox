type value =
  | Bool of bool
  | Num of float
  | String of string
  | Nil

let val_to_str l = match l with
  | Bool b -> string_of_bool b
  | Num x -> string_of_float x
  | String s -> s
  | Nil -> "nil"

let (let*) result f = match result with
  | Ok x -> f x
  | Error e -> Error e


let environment : (string, value) Hashtbl.t = Hashtbl.create 128  (* var name : value pairs. *)


let rec eval_expr expr = match expr with
  | Parse.Literal l -> Ok (eval_literal l)
  | Parse.Var var_name -> (match Hashtbl.find_opt environment var_name with
    | Some value -> Ok value
    | None -> Error ("Variable \"" ^ var_name ^ "\" does not exist, cannot be accessed."))
  | Parse.Unary {op; right} -> eval_unary op right
  | Parse.Binary {left; op; right} -> eval_binary left op right
  | Parse.Grouping g -> eval_expr g
  | Parse.Assignment (var_name, expr) ->
    if Hashtbl.mem environment var_name
    then
      let* value = eval_expr expr in
      Hashtbl.replace environment var_name value;
      Ok value
    else Error ("Undefined variable \"" ^ var_name ^ "\".")

and eval_literal l = match l with
  | Parse.Bool b -> Bool b
  | Parse.Num x -> Num x
  | Parse.String s-> String s
  | Parse.Nil -> Nil

and eval_unary op right =
  let* rval = eval_expr right in
  match (op, rval) with
    | (Parse.Negative, Num x) -> Ok (Num (-.x))
    | (Parse.Not, Bool b) -> Ok (Bool (not b))
    | (Parse.Not, Nil) -> Ok (Bool true)
    | (Parse.Not, _) -> Ok (Bool false)
    | _ -> Error (Printf.sprintf "Unexpected unary op types: %s %s."
                   (Parse.show_unary_op op) (val_to_str rval))

and eval_binary left op right =
  let* lval = eval_expr left in
  let* rval = eval_expr right in
  match (lval, op, rval) with
    | (Num x, Parse.Plus, Num y) -> Ok (Num (x +. y))
    | (Num x, Parse.Minus, Num y) -> Ok (Num (x -. y))
    | (Num x, Parse.Multiply, Num y) -> Ok (Num (x *. y))
    | (Num x, Parse.Divide, Num y) -> Ok (Num (x /. y))
    | (Num x, Parse.Less, Num y) -> Ok (Bool (x < y))
    | (Num x, LessEqual, Num y) -> Ok (Bool (x <= y))
    | (Num x, Parse.Greater, Num y) -> Ok (Bool (x > y))
    | (Num x, Parse.GreaterEqual, Num y) -> Ok (Bool (x >= y))
    | (x, Parse.EqualEqual, y) -> Ok (Bool (x = y))  (*TODO: check behavior.*)
    | (x, Parse.NotEqual, y) -> Ok (Bool (x <> y))
    | (String x, Parse.Plus, String y) -> Ok (String (x ^ y))
    | _ -> Error (Printf.sprintf "Unexpected binary op types: %s %s %s."
                   (val_to_str lval) (Parse.show_binary_op op) (val_to_str rval))


let eval_statement statement = match statement with
  | Parse.PrintStmt expr ->
    let* v = eval_expr expr in
    print_string (val_to_str v ^ "\n");
    Ok Nil
  | Parse.ExprStmt expr ->
    let* v = eval_expr expr in
    Ok Nil

let eval_opt_expr opt_expr = match opt_expr with
  | None -> Ok Nil
  | Some expr -> eval_expr expr

let eval_declaration decl = match decl with
  | Parse.Stmt statement -> eval_statement statement
  | Parse.VarDecl (var_name, init_expr) ->
    let* v = eval_opt_expr init_expr in
    Hashtbl.replace environment var_name v;
    Ok Nil

let rec eval (Parse.Program decls) = match decls with
  | decl :: decls ->
    let* v = eval_declaration decl in
    eval (Parse.Program decls)
  | [] -> Ok Nil
