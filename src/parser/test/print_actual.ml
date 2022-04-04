open Core_kernel
open Run_common

let print_last_pos entries =
    let pos = Exec_info.last_pos entries in
    Caml.Printf.printf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_ast ~pp x =
    let och =
        match !output with
        | "" -> Stdio.stdout
        | file -> Out_channel.create file
    in
    pp (Format.formatter_of_out_channel och) x;
    Out_channel.flush och

let () =
    Caml.Arg.parse speclist anon_fun "";

    let (module Parse) = mk_parse ~tokenize:!tokenize () in
    let filename = !input in
    let src = read_file ~filename in

    match Filename.extension filename with
    | ".res" ->
        let x, entries = Parse.Comb.parse_string_with_trace Parse.structure_parser ~filename src in

        begin match x with
        | None -> print_last_pos entries; failwith "x"
        | Some x -> print_ast ~pp:Printast.implementation @@ Pc_syntax.Parsetree_mapping.structure !mapping x
        end
    | ".resi" ->
        let x, entries = Parse.Comb.parse_string_with_trace Parse.signature_parser ~filename src in

        begin match x with
        | None -> print_last_pos entries; failwith "x"
        | Some x -> print_ast ~pp:Printast.interface @@ Pc_syntax.Parsetree_mapping.signature !mapping x
        end
    | _ -> failwith filename
