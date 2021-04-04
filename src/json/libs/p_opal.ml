open Json_opal
open Opal

module Parser = struct
    let parse_json text =
        let tokens = Tokens.tokens text in
        let rec next = function
            | [] -> LazyStream.Nil
            | t :: ts -> LazyStream.Cons(t, lazy (next ts))
        in
        let input = next tokens in
        parse json_item input

    let name = "opal"
end
