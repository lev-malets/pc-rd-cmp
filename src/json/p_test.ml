
open P_base

let sources = Files.(read_files [ basic; complex; ws; others ])

let pp ppf _v = Format.fprintf ppf "" (*"%s" @@ Yojson.Basic.pretty_to_string v*)
let testable: Yojson.Basic.t Alcotest.testable = let module M = struct
        type t = Yojson.Basic.t

        let pp: t Fmt.t = pp

        let equal = ( = )
  end in
  (module M)
let testable = Alcotest.option testable

let yojson_result = List.map (fun (name, src) -> name, src, Yojson.Basic.from_string src) sources

let check_ok msg e r = Alcotest.check testable msg (Some e) r

module Make (Parser : Parser) = struct
    open Parser

    let tests = List.map (fun (name, src, res) -> name, `Quick, fun () -> check_ok name res (parse_json src)) yojson_result

    let () = Alcotest.run "tests" [name, tests]
end
