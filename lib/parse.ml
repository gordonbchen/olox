type literal =
  | Bool of bool
  | Num of float
  | String of string
  | Nil

let literal_to_str l = match l with
  | Bool b -> string_of_bool b
  | Num x -> string_of_float x
  | String s -> s
  | Nil -> "nil"

type unary_op =
  | Negative
  | Not
  [@@deriving show]

type binary_op =
  | Plus | Minus
  | Multiply | Divide
  | Less | LessEqual | Greater | GreaterEqual
  | EqualEqual | NotEqual
  [@@deriving show]

type expr =
  | Literal of literal
  | Unary of {op: unary_op; right: expr}
  | Binary of {left: expr; op: binary_op; right: expr}
  | Grouping of expr

let rec expr_to_str expr = match expr with
  | Literal l -> literal_to_str l
  | Unary {op; right} -> "(" ^ show_unary_op op ^ " " ^ expr_to_str right ^ ")"
  | Binary {left; op; right} -> "(" ^ expr_to_str left ^ " " ^ show_binary_op op
                                                       ^ " " ^ expr_to_str right ^ ")"
  | Grouping expr -> "(" ^ expr_to_str expr ^ ")"


(*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

let (let*) (result, tokens) f = match result with
  | Ok expr -> f (expr, tokens)
  | Error e -> (Error e, tokens)

let get_key_value kv key = List.find_opt (fun (f, _) -> f = key) kv

let rec parse_star tokens_ops parse_func left tokens = match tokens with
  | t :: toks -> (
      match get_key_value tokens_ops t with
        | Some (_, op) ->
            let* (right, rem) = parse_func toks in
            let left = Binary {left; op; right} in
            parse_star tokens_ops parse_func left rem
        | None -> (Ok left, tokens)
  )
  | _ -> (Ok left, tokens)

let uncurry f (x, y) = f x y
let parse_binary parse_func ops tokens =
  let* (left, rem) = parse_func tokens in
  parse_star ops parse_func left rem

let token_to_literal t = match t with
  | Lex.TTrue -> Some (Bool true)
  | Lex.TFalse -> Some (Bool false)
  | Lex.TNum x -> Some (Num x)
  | Lex.TString s -> Some (String s)
  | Lex.TNil -> Some Nil
  | _ -> None


let rec parse_expr tokens = parse_equality tokens

and parse_equality tokens = parse_binary parse_comparison
  [(Lex.TBangEqual, NotEqual); (Lex.TEqualEqual, EqualEqual)] tokens 

and parse_comparison tokens = parse_binary parse_term
  [(Lex.TLess, Less); (Lex.TLessEqual, LessEqual);
   (Lex.TGreaterEqual, GreaterEqual); (Lex.TGreater, Greater)] tokens

and parse_term tokens = parse_binary parse_factor
  [(Lex.TMinus, Minus); (Lex.TPlus, Plus)] tokens

and parse_factor tokens = parse_binary parse_unary
  [(Lex.TSlash, Divide); (Lex.TStar, Multiply)] tokens

and parse_unary tokens = match tokens with
  | (Lex.TBang | Lex.TMinus) as t :: toks ->
    let* (right, rem) = parse_unary toks in
    let op = if t = Lex.TBang then Not else Negative in
    (Ok (Unary {op; right}), rem)
  | _ -> parse_primary tokens

and parse_primary tokens = match tokens with
  | Lex.TLeftParen :: toks ->
    let* (expr, toks) = parse_expr toks in (
      match toks with
        | Lex.TRightParen :: rem -> (Ok (Grouping expr), rem)
        | _ -> (Error "Expected ')' after expression.", toks)
    )
  | t :: rem -> (
    match token_to_literal t with
      | Some l -> (Ok (Literal l), rem)
      | _ -> (Error ("Expected to read a primary but found a " ^ Lex.show_token t), tokens)
  )
  | [] -> (Error "Expected to read a primary", [])


let parse tokens = match parse_expr tokens with
    | (Error e, _) -> Error e
    | (Ok ast, []) -> Ok ast
    | (_, t :: ts) -> Error ("Parser did not read all tokens. Next token: " ^ Lex.show_token t)
