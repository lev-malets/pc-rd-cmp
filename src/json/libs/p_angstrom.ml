open Angstrom
open P_base

type 'a parser = 'a Angstrom.t

let ws = skip_while (function ' ' | '\n' | '\t' | '\r' -> true | _ -> false)

module Json = struct
    let ( => ) a b = a *> return b

    let lchar c = char c <* ws

    let _false = string "false" => `Bool false
    let _true = string "true" => `Bool true

    let bool =
        let+ v = _false <|> _true in
        `Bool v

    let null = string "null" => `Null

    let _number = take_while1 @@ function
        | '-' | '.' -> true
        | '0'..'9' -> true
        | _ -> false
    let number =
        let* str = _number in
        match int_of_string_opt str with
        | Some a -> return @@ `Int a
        | None -> match float_of_string_opt str with
            | Some a -> return @@ `Float a
            | None -> fail "invalid float"

    let _string = char '"' *> take_while (fun i -> i <> '"') <* advance 1

    let string =
        let+ str = _string in
         `String str

    let json : json parser =
    fix (fun json ->
        let mem =
            let* key = _string <* ws in
            let+ value = lchar ':' *> json in
            (key, value)
        in
        let obj =
            let+ ms = lchar '{' *> sep_by (lchar ',') mem <* lchar '}' in
            `Assoc ms
        in
        let arr =
            let+ vs = lchar '[' *> sep_by (lchar ',') json <* lchar ']' in
            `List vs
        in
        (peek_char_fail >>= function
        | 'n' -> null
        | 't' -> _true
        | 'f' -> _false
        | '"' -> string
        | '{' -> obj
        | '[' -> arr
        | '-' | '.' | '0' .. '9' -> number
        | _ -> fail "inv char") <* ws) <?> "json"
end

let parse f str =
    match parse_string ~consume:All f str with
    | Ok a -> Some a
    | Error msg ->
        Printf.eprintf "%s\n" msg;
        None

module Parser = struct
    let parse_json = parse (ws *> Json.json)

    let name = "angstrom"
end
