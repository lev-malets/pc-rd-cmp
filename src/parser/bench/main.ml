open Core_kernel
open Run_common

let () =
  Arg.parse speclist anon_fun "";

  let parsers =
    [
      ("original", fun _ : (module Pc_syntax.Sigs.PARSE) -> (module ParseRes));
      ( "pc",
        fun _ ->
          let (module Parse) = mk_parse () in
          (module Parse) );
      ( "pc+t",
        fun _ ->
          let (module Parse) = mk_parse ~tokenize:true () in
          (module Parse) );
    ]
  in

  let filename = !input in
  let src = Res_io.readFile ~filename in

  let test =
    match Filename.extension filename with
    | ".res" ->
        List.map parsers ~f:(fun (name, fn) ->
            Core_bench.Bench.Test.create ~name @@ fun () ->
            let (module P : Pc_syntax.Sigs.PARSE) = fn () in
            Option.value_exn (P.parse_implementation ~src ~filename))
    | ".resi" ->
        List.map parsers ~f:(fun (name, fn) ->
            Core_bench.Bench.Test.create ~name @@ fun () ->
            let (module P : Pc_syntax.Sigs.PARSE) = fn () in
            Option.value_exn (P.parse_interface ~src ~filename))
    | _ -> failwith filename
  in

  Core_bench.Bench.bench
    (test
    @ [ (Core_bench.Bench.Test.create ~name:"init" @@ fun _ -> mk_parse ()) ]
    @ [ (Core_bench.Bench.Test.create ~name:"init+t" @@ fun _ -> mk_parse ~tokenize:true ()) ])
