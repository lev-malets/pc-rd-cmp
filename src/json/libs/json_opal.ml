open P_base
open Tokens
open Opal

type 'a parser = (token, 'a) Opal.parser

let (>>|) = (=>)
let (let+) = (>>|)
let (=>) a b = a >> return b

let number: json parser = any >>= function
    | Int x -> return @@ `Int x
    | Float x -> return @@ `Float x
    | _ -> mzero

let bool: json parser = any >>= function
    | Bool x -> return @@ `Bool x
    | _ -> mzero

let string = any >>= function
    | String x -> return x
    | _ -> mzero
let json_string = let+ str = string in `String str
let null = exactly Null => `Null

let object_begin = exactly ObjectBegin
let object_end = exactly ObjectEnd
let array_begin = exactly ArrayBegin
let array_end = exactly ArrayEnd
let colon = exactly Colon
let comma = exactly Comma
let sequence a = sep_by a comma

let rec json_item i = choice [ null; bool; number; json_string; assoc; array ] i
and object_pair i =
    let p =
        let* key = string in
        let+ value = colon >> json_item in
        (key, value)
    in
    p i
and assoc i =
    let p =
        let+ l = object_begin >> sequence object_pair << object_end in
        `Assoc l
    in
    p i
and array i =
    let p =
        let+ l = array_begin >> sequence json_item << array_end in
        `List l
    in
    p i
