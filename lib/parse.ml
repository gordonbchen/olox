type expr =
  | Literal of Lex.token  (* TODO: should this be a new literal type? *)
  | Unary of {op: Lex.token; right: expr}
  | Binary of {left: expr; op: Lex.token; right: expr}
  | Primary of Lex.token
  | Grouping of expr

let rec expr_to_str expr = match expr with
  | Literal l -> Lex.show_token l
  | Unary {op; right} -> Lex.show_token op ^ " " ^ expr_to_str right
  | Binary {left; op; right} -> expr_to_str left ^ " " ^ Lex.show_token op
                                                 ^ " " ^ expr_to_str right
  | Primary p -> Lex.show_token p
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
      let* (right, toks1) = parse_func toks in
      let left_expr = Binary {left; op; right} in
      parse_star ops parse_func left_expr toks1
  | _ -> (Ok left, tokens)

let uncurry f (x, y) = f x y
let parse_binary parse_func ops tokens =
  let* (left, rem) = parse_func tokens in
  parse_star ops parse_func left rem


let rec parse tokens = parse_equality tokens

and parse_equality tokens = parse_binary parse_comparison
  [Lex.TBangEqual; Lex.TEqualEqual] tokens 

and parse_comparison tokens = parse_binary parse_term
  [Lex.TLess; Lex.TLessEqual; Lex.TGreaterEqual; Lex.TGreater] tokens

and parse_term tokens = parse_binary parse_factor [Lex.TMinus; Lex.TPlus] tokens

and parse_factor tokens = parse_binary parse_unary [Lex.TSlash; Lex.TStar] tokens

and parse_unary tokens = match tokens with
  | (Lex.TBang | Lex.TMinus) as op :: toks ->
    let* (right, rem) = parse_unary toks in (Ok (Unary {op; right}), rem)
  | _ -> parse_primary tokens

and parse_primary tokens = match tokens with
  | (Lex.TNum _ | Lex.TString _ | Lex.TTrue | Lex.TFalse | Lex.TNil) as t :: rem ->
    (Ok (Primary t), rem)
  | Lex.TLeftParen :: toks ->
    let* (expr, toks1) = parse toks in (
      match toks1 with
        | Lex.TRightParen :: rem -> (Ok (Grouping expr), rem)
        | _ -> (Error "Expected ')' after expression.", toks1)
    )
  | t :: rem -> (Error ("Expected to read a primary but found a " ^ Lex.show_token t), tokens)
  | [] -> (Error "Expected to read a primary", [])
