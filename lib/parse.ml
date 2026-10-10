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
  | Var of string
  | Unary of {op: unary_op; right: expr}
  | Binary of {left: expr; op: binary_op; right: expr}
  | Grouping of expr
  | Assignment of string * expr

let rec expr_to_str expr = match expr with
  | Literal l -> literal_to_str l
  | Var var_name -> var_name
  | Unary {op; right} -> "(" ^ show_unary_op op ^ " " ^ expr_to_str right ^ ")"
  | Binary {left; op; right} -> "(" ^ expr_to_str left ^ " " ^ show_binary_op op
                                                       ^ " " ^ expr_to_str right ^ ")"
  | Grouping expr -> "(" ^ expr_to_str expr ^ ")"
  | Assignment (var_name, expr) -> var_name ^ " = " ^ expr_to_str expr


type statement = ExprStmt of expr | PrintStmt of expr

let statement_to_str statement = match statement with
  | ExprStmt expr -> "ExprStmt " ^ expr_to_str expr
  | PrintStmt expr -> "PrintStmt " ^ expr_to_str expr

type declaration = Stmt of statement | VarDecl of string * (expr option)

let declaration_to_str declaration = match declaration with
  | Stmt s -> statement_to_str s
  | VarDecl (var_name, Some expr) -> var_name ^ " = " ^ expr_to_str expr ^ ";"
  | VarDecl (var_name, None) -> var_name ^ ";"

type block = Decl of declaration | Block of declaration list

let rec decls_to_str decls = match decls with
  | d :: decls -> declaration_to_str d ^ "\n" ^ decls_to_str decls
  | [] -> ""

let rec block_to_str block = match block with
  | Decl d -> declaration_to_str d
  | Block decls -> "{\n" ^ decls_to_str decls ^ "}"

type program = Program of block list

let rec program_to_str (Program blocks) = match blocks with
  | b :: blocks -> block_to_str b ^ "\n" ^ program_to_str (Program blocks)
  | [] -> ""

(*
program -> block* EOF;
block -> "{" declaration* "}" | declaration;

declaration -> var_decl | statement;
var_decl -> "var" identifier ("=" expression)? ";";
statement -> "print"? expression ";";

expression     → assignment;
assignment     -> identifier "=" expression ";" | equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
                | "(" expression ")"
                | identifier;
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

let read_expect tokens expected error = match tokens with
  | tok :: rem when tok = expected -> (Ok tok, rem)
  | _ -> (Error error, tokens)


let rec parse_expr tokens = parse_assignment tokens

and parse_assignment tokens =
  let* (expr, toks) = parse_equality tokens in
  match toks with
    | Lex.TEqual :: toks -> (match expr with
      | Var var_name ->
        let* (rvalue, toks) = parse_expr toks in
        (Ok (Assignment (var_name, rvalue)), toks)
      | _ -> (Error "Assignment lvalue must be a variable identifier.", toks))
    | toks -> (Ok expr, toks)

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
    let* (expr, toks) = parse_expr toks in
    let* (_, toks) = read_expect toks Lex.TRightParen "Expected ')' after expression." in
    (Ok (Grouping expr), toks)
  | Lex.TId var_name :: toks -> (Ok (Var var_name), toks)
  | t :: rem -> (
    match token_to_literal t with
      | Some l -> (Ok (Literal l), rem)
      | _ -> (Error ("Expected to read a primary but found a " ^ Lex.show_token t), tokens)
  )
  | [] -> (Error "Expected to read a primary", [])


let parse_statement tokens = match tokens with
  | Lex.TPrint :: toks ->
    let* (expr, toks) = parse_expr toks in
    let* (_, toks) = read_expect toks Lex.TSemiColon "Expected ';' after print statement." in
    (Ok (PrintStmt expr), toks)
  | toks ->
    let* (expr, toks) = parse_expr toks in
    let* (_, toks) = read_expect toks Lex.TSemiColon "Expected ';' after statement." in
    (Ok (ExprStmt expr), toks)

let read_id tokens = match tokens with
  | Lex.TId var_name :: toks -> (Ok var_name, toks)
  | toks -> (Error "Expected var id.", toks)

let parse_decl tokens = match tokens with
  | Lex.TVar :: toks ->
    let* (var_name, toks) = read_id toks in
    let* (init_expr, toks) = (match toks with
      | Lex.TEqual :: toks ->
        let* (expr, toks) = parse_expr toks in
        (Ok (Some expr), toks)
      | toks -> (Ok None, toks)) in
    let* (_, toks) = read_expect toks Lex.TSemiColon "Expected ';' after var declaration." in
    (Ok (VarDecl (var_name, init_expr)), toks)
  | toks ->
    let* (statment, toks) = parse_statement toks in
    (Ok (Stmt statment), toks)

let rec parse_decls tokens = match tokens with
  | Lex.TRightBrace :: toks -> (Ok [], tokens)
  | toks ->
    let* (decl, toks) = parse_decl toks in
    let* (decls, toks) = parse_decls toks in
    (Ok (decl :: decls), toks)

let parse_block tokens = match tokens with
  | Lex.TLeftBrace :: toks ->
    let* (decls, toks) = parse_decls toks in
    let* (_, toks) = read_expect toks Lex.TRightBrace "Expected '}' to end block." in
    (Ok (Block decls), toks)
  | toks ->
    let* (decl, toks) = parse_decl toks in
    (Ok (Decl decl), toks)

let rec parse_program tokens = match tokens with
  | [Lex.TEOF] -> (Ok (Program []), [])
  | toks ->
    let* (block, toks) = parse_block toks in
    let* (Program blocks, toks) = parse_program toks in
    (Ok (Program (block :: blocks)), toks)


let parse tokens = match parse_program tokens with
    | (Error e, _) -> Error e
    | (Ok ast, []) -> Ok ast
    | (_, t :: ts) -> Error ("Parser did not read all tokens. Next token: " ^ Lex.show_token t)
