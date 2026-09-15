type token_type =
    TLeftParen | TRightParen | TLeftBrace | TRightBrace |
    TComma | TDot | TSemiColon |

    TPlus | TMinus | TStar | TSlash |

    TBangEqual | TEqual | TEqualEqual |
    TGreater | TGreaterEqual | TLess | TLessEqual |

    TId | TString | TNum |

    TBang | TAnd | TOr | TFalse | TTrue |

    TNil | TVar |

    TIf | TElse | TFor | TWhile |

    TFun | TReturn |
    TClass | TSuper | TThis |

    TPrint |

    TWspace | TComment | TEOF
    [@@deriving show]

type literal =
    | LBool of bool
    | LNum of float
    | LString of char list

let chars_to_str cs = String.of_seq @@ List.to_seq cs

let literal_to_str l = match l with
    | None -> ""
    | Some lit -> (match lit with
        | LBool x -> string_of_bool x
        | LNum x -> string_of_float x
        | LString x -> chars_to_str x)

type token = {
    ttype : token_type;
    tliteral : literal option;
}

let make_token ttype = Ok {ttype = ttype; tliteral = None}

let token_to_str t = show_token_type t.ttype ^ " " ^ literal_to_str t.tliteral


let get_error line msg =
    let error_msg = Printf.sprintf "[line %d] Error: %s\n" line msg in
    Error error_msg

let match_next chars x t1 t2 = match chars with
    | [] -> (make_token t1, chars)
    | c :: cs -> if c == x then (make_token t1, cs) else (make_token t2, chars)

let inc_line c (line : int ref) = if c == '\n' then incr line

let rec skip_after chars x (line : int ref) = match chars with
    | [] -> get_error !line "Did not find char to skip after."
    | c :: cs ->
        inc_line c line;
        if c == x then Ok cs else skip_after cs x line

let rec match_string chars buf (line : int ref) = match chars with
    | [] -> (get_error !line "String did not terminate", chars)
    | '"' :: cs ->
        let tok = {ttype = TString; tliteral = Some (LString (List.rev buf))} in
        (Ok tok, cs)
    | c :: cs ->
        inc_line c line;
        match_string cs (c :: buf) line

let is_digit c = c >= '0' && c <= '9'

let int_chars_to_token str =
    let lit = LNum (float_of_string @@ chars_to_str @@ str)
    in {ttype = TNum; tliteral = Some lit}

let rec match_num chars dot buf line =
    let return () = (Ok (int_chars_to_token (List.rev buf)), chars) in
    match chars with
        | [] -> return ()
        | c :: cs -> (match c with
            | n when is_digit n -> match_num cs dot (n :: buf) line
            | '.' when not dot -> match_num cs true ('.' :: buf) line
            | _ -> return ())

let rec scan_one chars (line : int ref) = match chars with
    | [] -> (make_token TEOF, [])
    | c :: cs -> match c with
        | '(' -> (make_token TLeftParen, cs)
        | ')' -> (make_token TRightParen, cs)
        | '{' -> (make_token TLeftBrace, cs)
        | '}' -> (make_token TRightBrace, cs)

        | ',' -> (make_token TComma, cs)
        | '.' -> (make_token TDot, cs)
        | ';' -> (make_token TSemiColon, cs)

        | '+' -> (make_token TPlus, cs)
        | '-' -> (make_token TMinus, cs)
        | '*' -> (make_token TStar, cs)

        | '!' -> match_next cs '=' TBangEqual TBang
        | '=' -> match_next cs '=' TEqualEqual TEqual
        | '<' -> match_next cs '=' TLessEqual TLess
        | '>' -> match_next cs '=' TGreaterEqual TGreater

        | '/' -> (match cs with
            | '/' :: rem -> (match skip_after rem '\n' line with
                | Ok rem1 -> (make_token TComment, rem1)
                | Error e -> (Error e, [])
                )
            | _ -> (make_token TSlash, cs))

        | ' ' -> (make_token TWspace, cs)
        | '\r' -> (make_token TWspace, cs)
        | '\t' -> (make_token TWspace, cs)
        | '\n' ->
            inc_line '\n' line;
            (make_token TWspace, cs)

        | '"' -> match_string cs [] line
        | n when is_digit n -> match_num chars false [] line

        | _ ->
            let error_msg = Printf.sprintf "Failed to match char: '%c'." c in
            (get_error !line error_msg, cs)

let rec scan chars tokens errors (line : int ref) = match scan_one chars line with
    | (Ok tok, rem) -> (match tok.ttype with
        | TEOF -> (List.rev @@ tok :: tokens, List.rev errors)
        | TWspace -> scan rem tokens errors line
        | TComment -> scan rem tokens errors line
        | _ -> scan rem (tok :: tokens) errors line)
    | (Error e, rem) -> scan rem tokens (e :: errors) line
