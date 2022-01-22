
open Core_kernel
open Run_common

let mapping = ref Pc_syntax.Parsetree_mapping.default
let stats = ref ""

let speclist =
    [ "--simplified", Arg.Unit (fun _ -> mapping := dump_loc_mapping), ""
    ] @ speclist

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = mk_memoized ~peek:true Pc_syntax.Parser.memo_spec in
    let filename = !input in
    let src = read_file ~filename in

    begin if String.sub filename ~pos:(String.length filename - 4) ~len:4 = ".res" then
        let res = Result.map ~f:(Pc_syntax.Parsetree_mapping.structure !mapping) @@ ParseRes.parse_implementation ~src ~filename in
        let pc = Result.map ~f:(Pc_syntax.Parsetree_mapping.structure !mapping) @@ Parse.parse_implementation ~src ~filename in

        assert (res = pc)
    else if String.sub filename ~pos:(String.length filename - 5) ~len:5 = ".resi" then
        let res = Result.map ~f:(Pc_syntax.Parsetree_mapping.signature !mapping) @@ ParseRes.parse_interface ~src ~filename in
        let pc = Result.map ~f:(Pc_syntax.Parsetree_mapping.signature !mapping) @@ Parse.parse_interface ~src ~filename in

        assert (res = pc)
    else
        failwith filename
    end;
