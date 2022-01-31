
open Core_kernel
open Run_common

let mapping = ref Pc_syntax.Parsetree_mapping.default
let stats = ref ""

let speclist =
    [ "--simplified", Arg.Unit (fun _ -> mapping := dump_loc_mapping), ""
    ] @ speclist

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = mk_parse ~peek:true ~memo:true () in
    let filename = !input in
    let src = read_file ~filename in

    begin if String.sub filename ~pos:(String.length filename - 4) ~len:4 = ".res" then
        let res = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.structure !mapping x.Res_driver.parsetree) @@
            ParseRes.parse_implementation ~src ~filename
        in
        let pc = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.structure !mapping x.Res_driver.parsetree) @@
            Parse.parse_implementation ~src ~filename
        in

        assert (res = pc)
    else if String.sub filename ~pos:(String.length filename - 5) ~len:5 = ".resi" then
        let res = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.signature !mapping x.Res_driver.parsetree) @@
            ParseRes.parse_interface ~src ~filename
        in
        let pc = Option.map ~f:(fun x -> Pc_syntax.Parsetree_mapping.signature !mapping x.Res_driver.parsetree) @@
            Parse.parse_interface ~src ~filename
        in

        assert (res = pc)
    else
        failwith filename
    end;
