open Core_kernel
open Run_common

let () =
    Arg.parse speclist anon_fun "";

    let parsers =
        [ "original", (fun _ -> (module ParseRes: Pc_syntax.Sigs.PARSE))
        (*
        ; "pure", (module Parse)
        ; "peek", (module ParsePeek)
        ; "memo", (fun _ -> mk_memoized Pc_syntax.Parser.memo_spec)
        *)
        ; "memo+peek", (fun _ -> mk_parse ~peek:true ~memo:true ())
        ; "memo+peek+tokenize", (fun _ -> mk_parse ~peek:true ~memo:true ~tokenize:true ())
        ]
    in

    let filename = !input in
    let src = Res_io.readFile ~filename in

    let test =
        match Filename.extension filename with
        | ".res" ->
            List.map parsers
                ~f:begin fun (name, fn) ->
                    Core_bench.Bench.Test.create ~name @@ fun () ->
                        let (module P: Pc_syntax.Sigs.PARSE) = fn () in
                        Option.value_exn (P.parse_implementation ~src ~filename)
                end
        | ".resi" ->
            List.map parsers
                ~f:begin fun (name, fn) ->
                    Core_bench.Bench.Test.create ~name @@ fun () ->
                        let (module P: Pc_syntax.Sigs.PARSE) = fn () in
                        Option.value_exn (P.parse_interface ~src ~filename)
                end
        | _ ->
            failwith filename
    in

    Core_bench.Bench.bench (
        test
        @ [Core_bench.Bench.Test.create ~name:"init" @@ fun _ -> mk_parse ~peek:true ~memo:true ()]
        @ [Core_bench.Bench.Test.create ~name:"init+t" @@ fun _ -> mk_parse ~peek:true ~memo:true ~tokenize:true ()]);
