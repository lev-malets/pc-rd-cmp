
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
                let t, p = mk_traced ~peek:true Pc_syntax.Parser.memo_spec in
                trace := Some t;
                parser := p
            | _ -> failwith "unreachable"
        ),
        ""
    ; "--simplified", Arg.Unit (fun _ -> mapping := dump_loc_mapping), ""
    ] @ speclist

let print_last_pos () =
    match !trace with
    | None -> ()
    | Some (module Trace) ->
        let path, pos = Exec_info.last_pos Trace.tt in
        Printf.printf "%s\n" @@ String.concat " |> " path;
        Printf.printf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_ast ~pp x =
    let och =
        match !output with
        | "" -> stdout
        | file -> open_out file
    in
    pp (Format.formatter_of_out_channel och) x;
    flush och

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = !parser in
    let filename = !input in
    let src = read_file ~filename in

    begin if String.sub filename (String.length filename - 4) 4 = ".res" then
        let x = Parse.parse_implementation ~src ~filename in

        match x with
        | Error x -> print_last_pos (); failwith x
        | Ok s -> print_ast ~pp: Printast.implementation @@
            Pc_syntax.Parsetree_mapping.structure !mapping s
    else if String.sub filename (String.length filename - 5) 5 = ".resi" then
        let x = Parse.parse_interface ~src ~filename in

        match x with
        | Error x -> print_last_pos (); failwith x
        | Ok s -> print_ast ~pp:Printast.interface @@
            Pc_syntax.Parsetree_mapping.signature !mapping s
    else
        failwith filename
    end;
