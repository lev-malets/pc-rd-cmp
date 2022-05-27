open Core_kernel
open Cmdliner
open Run_common

let run config input parser last_stage =
  let { filename; src } = mk_input input in

  let fn =
    match last_stage with
    | ParserInit ->
        fun () ->
          let _ = Parser.of_variant parser config in
          ()
    | _ ->
        let parse_fn =
          match Filename.extension filename with
          | ".res" ->
              fun (module Parse : Pc_syntax.Sigs.PARSE) ->
                let _ = Parse.parse_implementation ~filename ~src in
                ()
          | ".resi" ->
              fun (module Parse : Pc_syntax.Sigs.PARSE) ->
                let _ = Parse.parse_interface ~filename ~src in
                ()
          | _ -> failwith filename
        in
        fun () ->
          let _ = parse_fn @@ Parser.of_variant parser config in
          ()
  in

  let test = Core_bench.Bench.Test.create ~name:"test" fn in
  Core_bench.Bench.bench [ test ]

let cmd =
  let open Args in
  let doc = "" in
  let man = [ `S Manpage.s_description ] in
  ( Term.(const run $ config $ input $ parser $ last_stage),
    Term.info "bench" ~doc ~man )

let () = Term.exit @@ Term.eval cmd
