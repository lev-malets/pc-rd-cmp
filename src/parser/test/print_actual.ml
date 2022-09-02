open Core
open Run_common

let print_last_pos entries =
  let pos = Exec_info.last_pos entries in
  Caml.Printf.eprintf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let print_ast ~pp ~output x =
  let output = mk_output output in
  pp (Format.formatter_of_out_channel output.channel) x;
  output.close ()

let run config input output parser ignore_loc =
  let (module Parse) = Parser.pc parser config in
  let {filename; src} = mk_input input in
  match Filename.split_extension filename with
  | _, Some "res" -> (
      let x, entries =
        Parse.Comb.parse_string_with_trace Parse.structure_parser ~filename src
      in
      match x with
      | None -> print_last_pos entries; failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.structure dump_loc_mapping x
            else x
          in
          print_ast ~pp:Compilerlibs406.Printast.implementation ~output pt)
  | _, Some "resi" -> (
      let x, entries =
        Parse.Comb.parse_string_with_trace Parse.signature_parser ~filename src
      in
      match x with
      | None -> print_last_pos entries; failwith "x"
      | Some x ->
          let pt =
            if ignore_loc then
              Pc_syntax.Parsetree_mapping.signature dump_loc_mapping x
            else x
          in
          print_ast ~pp:Compilerlibs406.Printast.interface ~output pt)
  | _ -> failwith filename

open Cmdliner

let cmd =
  let open Args in
  Cmd.v (Cmd.info "print_actual")
    Term.(const run $ config $ input $ output $ parser_pc $ ignore_loc)

let () = Stdlib.exit @@ Cmd.eval cmd
