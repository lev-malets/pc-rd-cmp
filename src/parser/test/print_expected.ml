open Core
open Run_common

let print_ast ~pp ~output x =
  let output = mk_output output in
  pp (Format.formatter_of_out_channel output.channel) x;
  output.close ()

let run input output ignore_loc =
  let (module Parse) = Parser.rescipt in
  let { filename; src } = mk_input input in

  match Filename.split_extension filename with
  | _, Some "res" -> (
      let x = Parse.parse_implementation ~filename ~src in

      match x with
      | None -> failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.structure dump_loc_mapping x.parsetree
            else x.parsetree
          in
          print_ast ~pp:Compilerlibs406.Printast.implementation ~output pt)
  | _, Some "resi" -> (
      let x = Parse.parse_interface ~filename ~src in

      match x with
      | None -> failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.signature dump_loc_mapping x.parsetree
            else x.parsetree
          in
          print_ast ~pp:Compilerlibs406.Printast.interface ~output pt)
  | _ -> failwith filename

open Cmdliner

let cmd =
  let open Args in
  Cmd.v
    (Cmd.info "print_expected")
    Term.(const run $ input $ output $ ignore_loc)

let () = Stdlib.exit @@ Cmd.eval cmd
