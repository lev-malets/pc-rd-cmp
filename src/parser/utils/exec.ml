open Core_kernel
open Run_common
open Cmdliner

let run config input output parser ignore_loc last_stage =
  let { filename; src } = mk_input input in
  let (module Parse) = Parser.of_variant parser config in
  if last_stage = ParserInit then ()
  else
    let pp =
      match Filename.extension filename with
      | ".res" ->
          Option.map (Parse.parse_implementation ~filename ~src)
            ~f:(fun x fmt ->
              let pt =
                if ignore_loc then
                  Pc_syntax.Parsetree_mapping.structure dump_loc_mapping
                    x.parsetree
                else x.parsetree
              in
              Printast.implementation fmt pt)
      | ".resi" ->
          Option.map (Parse.parse_interface ~filename ~src) ~f:(fun x fmt ->
              let pt =
                if ignore_loc then
                  Pc_syntax.Parsetree_mapping.signature dump_loc_mapping
                    x.parsetree
                else x.parsetree
              in
              Printast.interface fmt pt)
      | _ -> failwith filename
    in

    match pp with
    | None -> failwith "failed to parse"
    | Some pp ->
        if last_stage = Parse then ()
        else
          let output = mk_output output in

          let fmt = Format.formatter_of_out_channel output.channel in
          pp fmt;
          output.close ()

let cmd =
  let open Args in
  ( Term.(const run $ config $ input $ output $ parser $ ignore_loc $ last_stage),
    Term.info "exec" )

let () = Term.exit @@ Term.eval cmd
