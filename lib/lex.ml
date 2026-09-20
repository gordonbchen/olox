type token =
    TLeftParen | TRightParen | TLeftBrace | TRightBrace |
    TComma | TDot | TSemiColon |

    TPlus | TMinus | TStar | TSlash |

    TBangEqual | TEqual | TEqualEqual |
    TGreater | TGreaterEqual | TLess | TLessEqual |

    TId of string |
    TString of string | TNum of float |

    TBang | TAnd | TOr | TFalse | TTrue |

    TNil | TVar |

    TIf | TElse | TFor | TWhile |

    TFun | TReturn |
    TClass | TSuper | TThis |

    TPrint |

    TWspace | TComment | TEOF
    [@@deriving show]


let match_next chars x t1 t2 = match chars with
    | [] -> (Ok t1, chars)
    | c :: cs -> if c == x then (Ok t1, cs) else (Ok t2, chars)


let get_error line msg =
    let error_msg = Printf.sprintf "[line %d] Error: %s\n" line msg in
    Error error_msg

let inc_line c (line : int ref) = if c == '\n' then incr line

let rec skip_after chars x (line : int ref) = match chars with
    | [] -> []
    | c :: cs ->
        inc_line c line;
        if c == x then cs else skip_after cs x line


let list_to_str l = String.of_seq @@ List.to_seq l

let rec match_string chars buf (line : int ref) = match chars with
    | [] -> (get_error !line "String did not terminate", chars)
    | '"' :: cs ->
        let tok = TString (list_to_str @@ List.rev buf) in (Ok tok, cs)
    | c :: cs ->
        inc_line c line;
        match_string cs (c :: buf) line


let is_digit c = c >= '0' && c <= '9'

let chars_to_str cs = String.of_seq @@ List.to_seq cs

let int_chars_to_token str = TNum (float_of_string @@ chars_to_str @@ str)

let rec match_num chars dot buf line =
    let return () = (Ok (int_chars_to_token (List.rev buf)), chars) in
    match chars with
        | [] -> return ()
        | c :: cs -> (match c with
            | n when is_digit n -> match_num cs dot (n :: buf) line
            | '.' when not dot -> match_num cs true ('.' :: buf) line
            | _ -> return ())


let is_alphanum c = is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let keyword_table =
    let symbols = [
        ("and", TAnd); ("or", TOr);
        ("false", TFalse); ("true", TTrue);

        ("nil", TNil); ("var", TVar);

        ("if", TIf); ("else", TElse);
        ("for", TFor); ("while", TWhile);

        ("fun", TFun); ("return", TReturn);
        ("class", TClass); ("super", TSuper); ("this", TThis);

        ("print", TPrint)
    ] in
    Hashtbl.of_seq (List.to_seq symbols)

let classify_id chars =
    let str = chars_to_str chars in
    match Hashtbl.find_opt keyword_table str with
      | Some keyword_type -> keyword_type
      | None -> TId str

let rec match_id chars buf =
    let token () = classify_id (List.rev buf) in
    let return () = (Ok (token ()), chars) in
    match chars with
        | [] -> return ()
        | c :: cs -> if c == '_' || is_alphanum c
            then match_id cs (c :: buf)
            else return ()


let rec scan_one chars (line : int ref) = match chars with
    | [] -> (Ok TEOF, [])
    | c :: cs -> match c with
        | '(' -> (Ok TLeftParen, cs)
        | ')' -> (Ok TRightParen, cs)
        | '{' -> (Ok TLeftBrace, cs)
        | '}' -> (Ok TRightBrace, cs)

        | ',' -> (Ok TComma, cs)
        | '.' -> (Ok TDot, cs)
        | ';' -> (Ok TSemiColon, cs)

        | '+' -> (Ok TPlus, cs)
        | '-' -> (Ok TMinus, cs)
        | '*' -> (Ok TStar, cs)

        | '!' -> match_next cs '=' TBangEqual TBang
        | '=' -> match_next cs '=' TEqualEqual TEqual
        | '<' -> match_next cs '=' TLessEqual TLess
        | '>' -> match_next cs '=' TGreaterEqual TGreater

        | '/' -> (
          match cs with
            | '/' :: rem -> let rem1 = skip_after rem '\n' line in (Ok TComment, rem1)
            | _ -> (Ok TSlash, cs)
        )

        | ' ' | '\r' | '\t' -> (Ok TWspace, cs)
        | '\n' ->
            inc_line '\n' line;
            (Ok TWspace, cs)

        | '"' -> match_string cs [] line
        | n when is_digit n -> match_num chars false [] line

        | c when c == '_' || is_alphanum c -> match_id chars []

        | _ ->
            let error_msg = Printf.sprintf "Failed to match char: '%c'." c in
            (get_error !line error_msg, cs)

let rec scan chars tokens errors (line : int ref) = match scan_one chars line with
    | (Ok tok, rem) -> (match tok with
        | TEOF -> (List.rev @@ tok :: tokens, List.rev errors)
        | TWspace -> scan rem tokens errors line
        | TComment -> scan rem tokens errors line
        | _ -> scan rem (tok :: tokens) errors line)
    | (Error e, rem) -> scan rem tokens (e :: errors) line
