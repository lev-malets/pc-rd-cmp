open P_base
open Bark
open Bark.Syntax

type 'a parser = (string, string, 'a) Bark.parser

let token s = Token (s, String.concat " " ["expected token"; s])
let symbol s = symbol @@ Token (s, String.concat " " ["expected symbol"; s])
let keyword s = keyword @@ Token (s, String.concat " " ["expected keyword"; s])
let (=>) parser f = map f parser
let (=.) parser v = map (fun _ -> v) parser

module Json = struct
    let _number = get_chomped_string @@ chomp_while @@ function
        | '0' .. '9' -> true
        | '-' | '.' -> true
        | _ -> false
    let number =
        _number |> and_then @@ fun str ->
        match int_of_string_opt str with
        | Some a -> succeed @@ `Int a
        | None -> match float_of_string_opt str with
            | Some a -> succeed @@ `Float a
            | None -> problem "invalid float"

    let _true = keyword "true" =. `Bool true
    let _false = keyword "false" =. `Bool false
    let bool = one_of [_true; _false]
    let _chars = map_chomped_string (fun str _ -> str) @@ chomp_until @@ token "\""
    let string =
        let+ _ = symbol "\""
            and+ str = _chars
            and+ _ = symbol "\""
        in
        str
    let json_string = map (fun str -> `String str) string
    let null = keyword "null" =. `Null

    let rec json_item () = one_of [ null; bool; number; json_string; lazily assoc; lazily array ]
        and assoc () = sequence ~start:(token "{") ~separator:(token ",") ~endd:(token "}") ~spaces:spaces ~item:(lazily object_pair) ~trailing:Optional => fun l -> `Assoc l
        and array () = sequence ~start:(token "[") ~separator:(token ",") ~endd:(token "]") ~spaces:spaces ~item:(lazily json_item) ~trailing:Optional => fun l -> `List l
        and object_pair () = succeed (fun key value -> (key, value))
            |= string
            |. spaces
            |. symbol ":"
            |. spaces
            |= lazily json_item
    let json: json parser = succeed (fun i -> i)
        |. spaces
        |= lazily json_item
end

let parse f str = match Bark.run f str with
| Ok ans -> Some ans
| Error list -> List.iter (fun (l: (string, string) Bark.dead_end) -> Printf.eprintf "@[%d@ %d@ %s@\n" (l.col) (l.row) (l.problem)) list; None

module Parser = struct
    let parse_json = parse Json.json

    let name = "bark"
end
