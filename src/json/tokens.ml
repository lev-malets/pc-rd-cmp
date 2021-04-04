type token =
    | Bool of bool
    | ObjectBegin
    | ArrayBegin
    | ObjectEnd
    | ArrayEnd
    | Colon
    | String of string
    | Int of int
    | Float of float
    | Null
    | Comma
    | Error of int

let is_ws = function
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false
let is_num = function
    | '0' .. '9' -> true
    | '.' | '-' -> true
    | _ -> false

let tokens str: token list =
    let len = String.length str in
    let char = String.get str in
    let rec skip p pos =
        if pos == len then pos
        else if p @@ char pos then skip p @@ pos + 1
        else pos
    in
    let check_rue pos = char pos = 'r' && char (pos + 1) = 'u' && char (pos + 2) = 'e' in
    let check_alse pos = char pos = 'a' && char (pos + 1) = 'l' && char (pos + 2) = 's' && char (pos + 3) = 'e' in
    let check_ull pos = char pos = 'u' && char (pos + 1) = 'l' && char (pos + 2) = 'l' in
    let rec next pos =
        let pos = skip is_ws pos in
        if pos == len then []
        else
            let c = char pos in
            match c with
            | '{' -> ObjectBegin :: (next @@ pos + 1)
            | '}' -> ObjectEnd :: (next @@ pos + 1)
            | '[' -> ArrayBegin :: (next @@ pos + 1)
            | ']' -> ArrayEnd :: (next @@ pos + 1)
            | ':' -> Colon :: (next @@ pos + 1)
            | ',' -> Comma :: (next @@ pos + 1)
            | 't' ->
                if check_rue @@ pos + 1 then
                    Bool true :: (next @@ pos + 4)
                else
                    [Error pos]
            | 'f' ->
                if check_alse @@ pos + 1 then
                    Bool false :: (next @@ pos + 5)
                else
                    [Error pos]
            | 'n' ->
                if check_ull @@ pos + 1 then
                    Null :: (next @@ pos + 4)
                else
                    [Error pos]
            | '"' ->
                let quote = skip (fun i -> i <> '"') (pos + 1) in
                String (String.sub str (pos + 1) (quote - pos - 1)) :: (next @@ quote + 1)
            | _ ->
                let next_pos = skip is_num pos in
                let literal = String.sub str pos (next_pos - pos) in
                match int_of_string_opt literal with
                | Some x -> Int x :: (next next_pos)
                | None -> match float_of_string_opt literal with
                    | Some x -> Float x :: (next next_pos)
                    | None -> [Error pos]
    in
    next 0
