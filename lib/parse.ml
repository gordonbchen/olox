type expr =
  | Literal of Lox.token  (* TODO: should this be a new literal type? *)
  | Unary of {op: Lox.token; right: expr}
  | Binary of {left: expr; op: Lox.token; right: expr}
  | Primary of Lox.token
  | Grouping of expr

let rec expr_to_str expr = match expr with
  | Literal l -> Lox.show_token l
  | Unary {op; right} -> Lox.show_token op ^ " " ^ expr_to_str right
  | Binary {left; op; right} -> expr_to_str left ^ " " ^ Lox.show_token op
                                                 ^ " " ^ expr_to_str right
  | Primary p -> Lox.show_token p
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

let rec parse_star ops parse_func left tokens = match tokens with
  | [] -> (Ok left, [])
  | op :: toks when List.mem op ops ->
      let* (inner_left, toks1) = parse_func toks in
      let* (right, rem) = parse_star ops parse_func inner_left toks1 in
      (Ok (Binary {left; op; right}), rem)
  | _ -> (Ok left, tokens)

let uncurry f (x, y) = f x y
let parse_binary parse_func ops tokens =
  let* (left, rem) = parse_func tokens in
  parse_star ops parse_func left rem


let rec parse tokens = parse_equality tokens

and parse_equality tokens = parse_binary parse_comparison
  [Lox.TBangEqual; Lox.TEqualEqual] tokens 

and parse_comparison tokens = parse_binary parse_term
  [Lox.TLess; Lox.TLessEqual; Lox.TGreaterEqual; Lox.TGreater] tokens

and parse_term tokens = parse_binary parse_factor [Lox.TMinus; Lox.TPlus] tokens

and parse_factor tokens = parse_binary parse_unary [Lox.TSlash; Lox.TStar] tokens

and parse_unary tokens = match tokens with
  | (Lox.TBang | Lox.TMinus) as op :: toks ->
    let* (right, rem) = parse_unary toks in (Ok (Unary {op; right}), rem)
  | _ -> parse_primary tokens

and parse_primary tokens = match tokens with
  | (Lox.TNum _ | Lox.TString _ | Lox.TTrue | Lox.TFalse | Lox.TNil) as t :: rem ->
    (Ok (Primary t), rem)
  | Lox.TLeftParen :: toks ->
    let* (expr, toks1) = parse toks in (
      match toks1 with
        | Lox.TRightParen :: rem -> (Ok (Grouping expr), rem)
        | _ -> (Error "Expected ')' after expression.", toks1)
    )
  | t :: rem -> (Error ("Expected to read a primary but found a " ^ Lox.show_token t), tokens)
  | [] -> (Error "Expected to read a primary", [])
