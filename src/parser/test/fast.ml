
open Core_kernel
open Run_common

let mapping = ref Pc_syntax.Parsetree_mapping.default
let stats = ref ""

let speclist =
    [ "--simplified", Arg.Unit (fun _ -> mapping := dump_loc_mapping), ""
    ] @ speclist

let files = ref []

let anon_fun str =
    files := str :: !files

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = mk_parse ~peek:true ~memo:true () in

    let files = List.rev !files in
    let max_len = List.fold_left files ~init:0 ~f:(fun acc x -> max acc @@ String.length x) in

    let rec loop =
        function
        | [] -> None
        | filename::files ->
            Printf.printf "Test %s" filename;
            Out_channel.flush Out_channel.stdout;
            let src = read_file ~filename in
            let ok =
                 match Filename.extension filename with
                | ".res" ->
                    let res = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.structure !mapping x.Res_driver.parsetree) @@
                        ParseRes.parse_implementation ~src ~filename
                    in
                    let pc = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.structure !mapping x.Res_driver.parsetree) @@
                        Parse.parse_implementation ~src ~filename
                    in

                    res = pc
                | ".resi" ->
                    let res = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.signature !mapping x.Res_driver.parsetree) @@
                        ParseRes.parse_interface ~src ~filename
                    in
                    let pc = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.signature !mapping x.Res_driver.parsetree) @@
                        Parse.parse_interface ~src ~filename
                    in

                    res = pc
                | _ -> failwith filename
            in

            let spaces = String.make (max_len - String.length filename + 1) ' ' in

            if ok then begin
                Printf.printf "%s| ok\n" spaces;
                Out_channel.flush Out_channel.stdout;
                loop files
            end else begin
                Printf.printf "%s| fail\n" spaces;
                Out_channel.flush Out_channel.stdout;
                Some filename
            end
    in

    match loop files with
    | Some filename ->
        Out_channel.write_all !output ~data:filename
    | None -> ()
