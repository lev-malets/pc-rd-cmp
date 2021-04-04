open Json_opal
open Opal

module Parser = struct
    let parse_json text =
        let tokens =
            let lexbuf = Lexing.from_string text in
            Lexer.read lexbuf
        in
        parse json_item tokens

    let name = "opal_lex"
end
