open P_base
open Core
open Core_bench

type parser = (module Parser)

let sources = Files.(read_files [ complex; ws; others ])

let parsers: parser list = [
    (module P_angstrom.Parser);
    (module P_angstrom_pos.Parser);
    (module P_opal.Parser);
    (module P_opal_lex.Parser);
    (module P_mparser.Parser);
    (module P_bark.Parser);
    (module P_yojson.Parser);
]

let make_bench_json parser contents =
    let (module Parser : Parser) = parser in
    Bench.Test.create ~name:Parser.name (fun () ->
        match Parser.parse_json contents with
        | Some _ -> ()
        | None -> failwith "fail to parse"
    )

let () = Command.run @@
    Command.group ~summary:"various benchmarks" @@
        List.map sources ~f:(fun (name, src) -> name, Bench.make_command @@ List.map parsers ~f:(fun parser -> make_bench_json parser src))
