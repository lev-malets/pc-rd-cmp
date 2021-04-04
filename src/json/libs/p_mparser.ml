open P_base
open MParser

type 'a parser = ('a, unit) MParser.parser

let (let*) = (>>=)
let (let+) = (|>>)

let _false = string "false" >>$ `Bool false
let _true = string "true" >>$ `Bool true

let bool: json parser = _false <|> _true
let null: json parser = string "null" >>$ `Null

let _number = many1_chars (digit <|> char '.' <|> char '-')
let number =
    let* str = _number in
    match int_of_string_opt str with
    | Some a -> return @@ `Int a
    | None -> match float_of_string_opt str with
        | Some a -> return @@ `Float a
        | None -> fail "invalid float"

let quote = char '"'
let _string: string parser = quote >> many_chars_until any_char quote
let string: json parser = let+ str = _string in `String str

let comma = char ','
let bound_by a b = between (char a << spaces) (char b)
let sequence a = sep_by a (comma << spaces)

let rec json_item input = (choice [ number; bool; string; null; assoc; array ] << spaces) input
    and assoc input =
        let p =
            let+ l = bound_by '{' '}' @@ sequence object_pair in
            `Assoc l
        in
        p input
    and array input =
        let p =
            let+ l = bound_by '[' ']' @@ sequence json_item in
            `List l
        in
        p input
    and object_pair input =
        let p =
            let* key = _string << spaces in
            let+ value = char ':' >> spaces >> json_item in
            (key, value)
        in
        p input

let parse f str = match MParser.parse_string (spaces >> f) str () with
    | Success e -> Some e
    | Failed (msg, _) -> print_endline msg; None

module Parser = struct
    let parse_json = parse json_item
    let name = "mparser"
end
