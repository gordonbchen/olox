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
    | LString of string

let literal_to_str l = match l with
    | None -> ""
    | Some i -> match i with
        | LBool x -> string_of_bool x
        | LNum x -> string_of_float x
        | LString x -> x

type token = {
    ttype : token_type;
    tliteral : literal option;
}

let make_token ?tliteral ttype = Some {ttype = ttype; tliteral = tliteral}

let token_to_str t = show_token_type t.ttype ^ " " ^ literal_to_str t.tliteral


let report_error line msg = Printf.eprintf "[line %d] Error: %s" line, msg

let match_next src match_c t1 t2 = match src with
    | [] -> (make_token t1, src)
    | c :: cs -> if c == match_c then (make_token t1, src) else (make_token t2, cs)

let rec forward_until xs x = match xs with
    | [] -> []
    | c :: cs -> if c == x then cs else forward_until cs x

let match_string xs line = match xs with
    | [] -> report_error line
    (* TODO: here *)

(* TODO: make line a ref? *)
let rec scan_one src = match src with
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
            | '/' :: rem -> scan_one (forward_until rem '\n')
            | _ -> (make_token TSlash, cs))

        | ' ' -> (None, cs)
        | '\r' -> (None, cs)
        | '\t' -> (None, cs)
        | '\n' -> (None, cs)

        | '"' -> match_string cs

        (* TODO: placeholder for error stuff. *)
        | _ -> (make_token TEOF, [])
