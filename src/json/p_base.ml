
type json = Yojson.Basic.t

module type Parser = sig
    val parse_json: string -> json option

    val name: string
end

let read name = Std.input_all @@ open_in name

module Files = struct
    type set = {
        name : string;
        files : string list;
    }

    let complex = {
        name = "complex";
        files = [
            "1";
            "2";
            "3";
            "4";
            "5";
            "6";
        ]
    }

    let basic = {
        name = "basic";
        files = [
            "null";
            "true";
            "false";
            "integer";
            "negative";
            "float";
            "string";
        ]
    }

    let ws = {
        name = "ws";
        files = [
            "lines";
            "spaces";
            "mix";
        ]
    }

    let others = {
        name = "others";
        files = [
            "repeat";
            "repeat-compact";
        ]
    }

    let read_file set_name name : string * string = (Printf.sprintf "%s:%s" set_name name, read @@ Printf.sprintf "data/json/%s/%s" set_name name)
    let add_files files set = List.fold_left (fun ts name -> read_file set.name name :: ts) files set.files
    let read_files sets = List.fold_left add_files [] sets
end
