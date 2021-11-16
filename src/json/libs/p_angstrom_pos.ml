module Angstrom = Angstrom_pos.Make(struct type s = unit end)

open Angstrom
open P_base

let ws = whitespace

module Json = struct
    let lchar c: char Parser.t = char c << ws

    let _false: json Parser.t = string "false" >>$ `Bool false
    let _true: json Parser.t = string "true" >>$ `Bool true

    let bool = (_false <|> _true)

    let null = string "null" >>$ `Null

    let _number = take_while1 @@ function
        | '-' | '.' -> true
        | '0'..'9' -> true
        | _ -> false
    let number = _number >>= fun str ->
        match int_of_string_opt str with
        | Some a -> return @@ `Int a
        | None -> match float_of_string_opt str with
            | Some a -> return @@ `Float a
            | None -> fail "invalid float"

    let _string = char '"' >> take_while (fun i -> i <> '"') << advance 1

    let string: json Parser.t = map ~f:(fun s -> `String s)
        _string

    let json: json Parser.t = fix (fun json ->
        let mem = map2 ~f:(fun key value -> (key, value))
            (_string << ws)
            (lchar ':' >> json)
        in
        let obj = map ~f:(fun ms -> `Assoc ms)
            (lchar '{' >> sep_by (lchar ',') mem << lchar '}')
        in
        let arr = map ~f:(fun vs -> `List vs)
            (lchar '[' >> sep_by (lchar ',') json << lchar ']')
        in
        (peek_char_fail >>= function
        | 'n' -> null
        | 't' -> _true
        | 'f' -> _false
        | '"' -> string
        | '{' -> obj
        | '[' -> arr
        | '-' | '.' | '0' .. '9' -> number
        | _ -> fail "inv char") << ws) <?> "json"
end

let parse f str =
    match parse_string f () str with
    | Ok a -> Some a
    | Error msg ->
        Printf.eprintf "%s\n" msg;
        None

module Parser = struct
    let parse_json = parse (ws >> Json.json)

    let name = "angstrom-pos"
end
