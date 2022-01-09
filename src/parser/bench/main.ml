open Core_kernel
open Run_common

let () =
    Arg.parse speclist anon_fun "";

    let parsers =
        [ "original", (module ParseRes: Pc_syntax.Sigs.PARSE)
(*
        ; "pure", (module Parse)
        ; "peek", (module ParsePeek)
*)
        ; "memo", mk_memoized Pc_syntax.Parser.memo_spec
        ; "memo+peek", mk_memoized ~peek Pc_syntax.Parser.memo_spec
        ]
    in

    let filename = !input in
    let src = Res_io.readFile ~filename in

    let test =
        if String.sub filename ~pos:(String.length filename - 4) ~len:4 = ".res" then
            List.map parsers
                ~f:begin fun (name, (module P: Pc_syntax.Sigs.PARSE)) ->
                    Core_bench.Bench.Test.create ~name @@ fun () -> Result.ok @@ P.parse_implementation ~src ~filename
                end
        else if String.sub filename ~pos:(String.length filename - 5) ~len:5 = ".resi" then
            List.map parsers
                ~f:begin fun (name, (module P: Pc_syntax.Sigs.PARSE)) ->
                    Core_bench.Bench.Test.create ~name @@ fun () -> Result.ok @@ P.parse_interface ~src ~filename
                end
        else
            failwith filename
    in

    Core_bench.Bench.bench test
