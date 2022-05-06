open Core_kernel
open Run_common

let print_ast ~pp x =
  let och = match !output with "" -> Stdio.stdout | file -> Out_channel.create file in
  pp (Format.formatter_of_out_channel och) x.Res_driver.parsetree;
  Out_channel.flush och

let () =
  Caml.Arg.parse speclist anon_fun "";

  let filename = !input in
  let src = read_file ~filename in

  match Filename.extension filename with
  | ".res" -> (
      let x = ParseRes.parse_implementation ~src ~filename in

      match x with
      | None -> failwith "x"
      | Some x ->
          print_ast ~pp:Printast.implementation
          @@ { x with parsetree = Pc_syntax.Parsetree_mapping.structure !mapping x.parsetree })
  | ".resi" -> (
      let x = ParseRes.parse_interface ~src ~filename in

      match x with
      | None -> failwith "x"
      | Some x ->
          print_ast ~pp:Printast.interface
          @@ { x with parsetree = Pc_syntax.Parsetree_mapping.signature !mapping x.parsetree })
  | _ -> failwith filename
