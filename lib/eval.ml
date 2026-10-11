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

type environment = {vars: (string, value) Hashtbl.t; parent: environment option}
let rec get_var_val env var_name = match Hashtbl.find_opt env.vars var_name with
  | Some value -> Ok value
  | None -> (match env.parent with
    | Some parent_env -> get_var_val parent_env var_name
    | None -> Error ("Variable \"" ^ var_name ^ "\" undefined."))

let (let*) result f = match result with
  | Ok x -> f x
  | Error e -> Error e


let rec eval_expr expr env = match expr with
  | Parse.Literal l -> Ok (eval_literal l)
  | Parse.Var var_name -> get_var_val env var_name
  | Parse.Unary {op; right} -> eval_unary op right env
  | Parse.Binary {left; op; right} -> eval_binary left op right env
  | Parse.Grouping g -> eval_expr g env
  | Parse.Assignment (var_name, expr) ->
    let* _ = get_var_val env var_name in
    let* value = eval_expr expr env in
    Hashtbl.replace env.vars var_name value;
    Ok value

and eval_literal l = match l with
  | Parse.Bool b -> Bool b
  | Parse.Num x -> Num x
  | Parse.String s-> String s
  | Parse.Nil -> Nil

and eval_unary op right env =
  let* rval = eval_expr right env in
  match (op, rval) with
    | (Parse.Negative, Num x) -> Ok (Num (-.x))
    | (Parse.Not, Bool b) -> Ok (Bool (not b))
    | (Parse.Not, Nil) -> Ok (Bool true)
    | (Parse.Not, _) -> Ok (Bool false)
    | _ -> Error (Printf.sprintf "Unexpected unary op types: %s %s."
                   (Parse.show_unary_op op) (val_to_str rval))

and eval_binary left op right env =
  let* lval = eval_expr left env in
  let* rval = eval_expr right env in
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


let eval_opt_expr opt_expr env = match opt_expr with
  | None -> Ok Nil
  | Some expr -> eval_expr expr env

let rec eval_statement statement env = match statement with
  | Parse.PrintStmt expr ->
    let* v = eval_expr expr env in
    print_string (val_to_str v ^ "\n");
    Ok Nil
  | Parse.ExprStmt expr ->
    let* v = eval_expr expr env in
    Ok Nil
  | Parse.VarDecl (var_name, init_expr) ->
    let* v = eval_opt_expr init_expr env in
    Hashtbl.replace env.vars var_name v;
    Ok Nil
  | Parse.Block statements ->
    let inner_env = {vars = (Hashtbl.create 16); parent = Some env} in
    eval_statements statements inner_env

and eval_statements statements env = match statements with
  | stmt :: stmts ->
    let* _ = eval_statement stmt env in
    eval_statements stmts env
  | [] -> Ok Nil

let eval (Parse.Program statements) =
  let env = {vars = (Hashtbl.create 128); parent = None} in
  eval_statements statements env
