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


let rec eval expr = match expr with
  | Parse.Literal l -> Ok (eval_literal l)
  | Parse.Unary {op; right} -> eval_unary op right
  | Parse.Binary {left; op; right} -> eval_binary left op right
  | Parse.Grouping g -> eval g

and eval_literal l = match l with
  | Parse.Bool b -> Bool b
  | Parse.Num x -> Num x
  | Parse.String s-> String s
  | Parse.Nil -> Nil

and eval_unary op right =
  let* rval = eval right in
  match (op, rval) with
    | (Parse.Negative, Num x) -> Ok (Num (-.x))
    | (Parse.Not, Bool b) -> Ok (Bool (not b))
    | (Parse.Not, Nil) -> Ok (Bool true)
    | (Parse.Not, _) -> Ok (Bool false)
    | _ -> Error (Printf.sprintf "Unexpected unary op types: %s %s."
                   (Parse.show_unary_op op) (val_to_str rval))

and eval_binary left op right =
  let* lval = eval left in
  let* rval = eval right in
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
