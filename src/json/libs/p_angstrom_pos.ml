module Angstrom = Angstrom_pos.Make(struct type t end)

open Base
open Angstrom
open P_base

let ws = whitespace

module Json = struct
    let lchar c: char t = char c << ws

    let _false: json t = string "false" >>$ `Bool false
    let _true: json t = string "true" >>$ `Bool true

    let bool = (_false <|> _true)

    let null = string "null" >>$ `Null

    let _number = take_while1 @@ function
        | '-' | '.' -> true
        | '0'..'9' -> true
        | _ -> false
    let number = _number >>= fun str ->
        let open Angstrom in
        match Caml.int_of_string_opt str with
        | Some a -> return @@ `Int a
        | None -> match Caml.float_of_string_opt str with
            | Some a -> return @@ `Float a
            | None -> fail "invalid float"

    let _string = char '"' >> take_while (fun i -> Char.(i <> '"')) << advance 1

    let string: json t = _string >>| fun s -> `String s

    let json: json t = fix (fun json ->
        let mem =
            mapping (fun key value -> (key, value))
            <*> _string << ws << lchar ':' <*> json
        in
        let obj =
            (lchar '{' >> seq ~n:1 ~sep:(lchar ',') mem << lchar '}')
            >>|
            fun ms -> `Assoc ms
        in
        let arr =
            (lchar '[' >> seq ~n:1 ~sep:(lchar ',') json << lchar ']')
            >>|
            fun vs -> `List vs
        in
        peek_first
        [ null
        ; _true
        ; _false
        ; string
        ; obj
        ; arr
        ; number ] << ws
    )
end

let parse f str = parse_string f str

module Parser = struct
    let parse_json = parse (ws >> Json.json)

    let name = "angstrom-pos"
end
