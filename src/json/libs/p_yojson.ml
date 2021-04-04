
module Parser = struct
    let parse_json text = Some (Yojson.Basic.from_string text)

    let name = "yojson"
end
