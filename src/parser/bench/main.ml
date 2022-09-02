open Core
open Cmdliner
open Run_common

let run config input parser last_stage quota =
  let { filename; src } = mk_input input in

  let fn =
    match last_stage with
    | ParserInit ->
        fun () ->
          let _ = Parser.of_variant parser config in
          ()
    | _ ->
        let parse_fn =
          match Filename.split_extension filename with
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
  let run_config =
    Core_bench.Bench.Run_config.create
      ~time_quota:(Core.Time.Span.of_sec quota)
      ()
  in
  Core_bench.Bench.bench ~run_config [ test ]

let cmd =
  let quota = Arg.(value & opt float 10. & info ~docv:"SECONDS" [ "quota" ]) in

  let open Args in
  let doc = "" in
  let man = [ `S Manpage.s_description ] in
  ( Term.(const run $ config $ input $ parser $ last_stage $ quota),
    Term.info "bench" ~doc ~man )

let () = Stdlib.exit @@ Cmd.eval cmd
