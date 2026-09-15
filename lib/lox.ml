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

    TEOF
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

let make_token ?tliteral ttype = Some {ttype = ttype; tliteral = tliteral}

let token_to_str t = show_token_type t.ttype ^ " " ^ literal_to_str t.tliteral


let report_error line msg = Printf.eprintf "[line %d] Error: %s" line, msg

let match_next chars x t1 t2 = match chars with
    | [] -> (make_token t1, chars)
    | c :: cs -> if c == x then (make_token t1, cs) else (make_token t2, chars)

let rec skip_after chars x = match chars with
    | [] -> []
    | c :: cs -> if c == x then cs else skip_after cs x

let rec match_string chars buf = match chars with
    | [] -> (None, [])
    | '"' :: cs -> (
        Some {ttype = TString; tliteral = Some (LString (List.rev buf))},
        cs)
    | c :: cs -> match_string cs (c :: buf)

let is_digit c = c >= '0' && c <= '9'

let int_chars_to_token str =
    let lit = LNum (float_of_string @@ chars_to_str @@ List.rev str)
    in {ttype = TNum; tliteral = Some lit}

let rec match_num chars dot buf =
    let return () = if (List.is_empty buf) then (None, chars)
        else (Some (int_chars_to_token buf), chars) in
    match chars with
        | [] -> return ()
        | c :: cs -> (match c with
            | n when is_digit n -> match_num cs dot (n :: buf)
            | '.' when not dot -> match_num cs true ('.' :: buf)
            | _ -> return ())

(* TODO: make line a ref? *)
let rec scan_one chars = match chars with
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
        | '<' -> match_next cs '=' TLess TLessEqual
        | '>' -> match_next cs '=' TGreater TGreaterEqual

        | '/' -> (match cs with
            | '/' :: rem -> (None, skip_after rem '\n')
            | _ -> (make_token TSlash, cs))

        | ' ' -> (None, cs)
        | '\r' -> (None, cs)
        | '\t' -> (None, cs)
        | '\n' -> (None, cs)

        (* String and num matching needs error handling. *)
        | '"' -> match_string cs []
        | n when is_digit n -> match_num chars false []

        (* TODO: placeholder for error stuff. *)
        | _ -> (None, cs)

let rec scan chars tokens = match scan_one chars with
    | (Some tok, rem) -> (match tok with
        | {ttype = TEOF; tliteral = _} -> List.rev @@ tok :: tokens
        | _ -> scan rem (tok :: tokens))
    | (None, rem) -> scan rem tokens
