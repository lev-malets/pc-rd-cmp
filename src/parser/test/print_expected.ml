open Core_kernel
open Run_common

let print_ast ~pp ~output x =
  let output = mk_output output in
  pp (Format.formatter_of_out_channel output.channel) x;
  output.close ()

let run input output ignore_loc =
  let (module Parse) = Parser.rescipt in
  let { filename; src } = mk_input input in

  match Filename.extension filename with
  | ".res" -> (
      let x = Parse.parse_implementation ~filename ~src in

      match x with
      | None -> failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.structure dump_loc_mapping x.parsetree
            else x.parsetree
          in
          print_ast ~pp:Printast.implementation ~output pt)
  | ".resi" -> (
      let x = Parse.parse_interface ~filename ~src in

      match x with
      | None -> failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.signature dump_loc_mapping x.parsetree
            else x.parsetree
          in
          print_ast ~pp:Printast.interface ~output pt)
  | _ -> failwith filename

open Cmdliner

let cmd =
  let open Args in
  let doc = "" in
  let man = [ `S Manpage.s_description ] in
  ( Term.(const run $ input $ output $ ignore_loc),
    Term.info "rm" ~version:"%%VERSION%%" ~doc ~man )

let () = Term.exit @@ Term.eval cmd
