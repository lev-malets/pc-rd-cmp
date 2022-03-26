open Core_kernel
open Run_common

let trace = ref None
let parser = ref ((module ParseRes): (module Pc_syntax.Sigs.PARSE))
let mapping = ref Pc_syntax.Parsetree_mapping.default

let speclist =
    [ "--parser",
        Arg.Symbol (["res"; "pc"],
            function
            | "res" -> parser := (module ParseRes)
            | "pc" ->
                let t, a, p = mk_traced ~peek:true () in
                trace := Some (t, a);
                parser := p
            | _ -> failwith "unreachable"
        ),
        ""
    ; "--simplified", Arg.Unit (fun _ -> mapping := dump_loc_mapping), ""
    ] @ speclist

let print_last_pos () =
    match !trace with
    | None -> ()
    | Some ((module Traced), (module APos)) ->
        let pos = Exec_info.last_pos @@ List.rev !Traced.entries in
        (*
        let names =
            List.map path ~f:APos.name_of_id
        in

        Caml.Printf.printf "%s\n" @@ String.concat ~sep:" |> " names;
        *)
        Caml.Printf.printf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_ast ~pp x =
    let och =
        match !output with
        | "" -> Stdio.stdout
        | file -> Out_channel.create file
    in
    pp (Format.formatter_of_out_channel och) x.Res_driver.parsetree;
(*
    Printf.fprintf och "----------------------------------------\n";
    Printf.fprintf och "----------------comments----------------\n";
    Printf.fprintf och "----------------------------------------\n";

    List.iter x.comments
        ~f:begin fun c ->
            Printf.fprintf och "\n%s\n" (Res_comment.toString c);
            let prev = Res_comment.prevTokEndPos c in
            Printf.fprintf och "prev loc: %d:%d\n" prev.pos_lnum (prev.pos_cnum - prev.pos_bol + 1);
            let loc = Res_comment.loc c in
            Printf.fprintf och "loc: %s " loc.loc_start.pos_fname;
            if loc.loc_ghost then Printf.fprintf och "ghost ";
            Printf.fprintf och "%d:%d - %d:%d\n"
                loc.loc_start.pos_lnum (loc.loc_start.pos_cnum - loc.loc_start.pos_bol + 1)
                loc.loc_end.pos_lnum (loc.loc_end.pos_cnum - loc.loc_end.pos_bol + 1)
        end;
*)
    Out_channel.flush och

let () =
    Caml.Arg.parse speclist anon_fun "";

    let (module Parse) = !parser in
    let filename = !input in
    let src = read_file ~filename in

    match Filename.extension filename with
    | ".res" ->
        let x = Parse.parse_implementation ~src ~filename in

        begin match x with
        | None -> print_last_pos (); failwith "x"
        | Some x -> print_ast ~pp:Printast.implementation @@
            {x with parsetree = Pc_syntax.Parsetree_mapping.structure !mapping x.parsetree}
        end
    | ".resi" ->
        let x = Parse.parse_interface ~src ~filename in

        begin match x with
        | None -> print_last_pos (); failwith "x"
        | Some x -> print_ast ~pp:Printast.interface @@
            {x with parsetree = Pc_syntax.Parsetree_mapping.signature !mapping x.parsetree}
        end
    | _ -> failwith filename
