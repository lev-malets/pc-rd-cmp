open Run_common

let memo_spec = ref []
let min_count = ref 0.
let min_time = ref 0.
let anon_fun _ = ()

let speclist = [
    "--input", Arg.Set_string input, "";
    "--output", Arg.Set_string output, "";

]

let speclist =
    [ "--memo",
        Arg.Unit (fun () -> memo_spec := Pc_syntax.Parser.memo_spec),
        ""
    ; "--filter-count", Arg.Set_float min_count, ""
    ; "--filter-time", Arg.Set_float min_time, ""
    ] @ speclist

let print_stats (module Trace: Angstrom_pos.Sigs.TRACED) =
    let stats = Exec_info.to_stats Trace.tt in
    let list = ref [] in

    let ch = open_out !output in

    Hashtbl.iter
        begin fun parser stats ->
            if parser.[0] != '\'' then
            list := (parser, stats) :: !list
        end
        stats;

    let list = List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !list in

    List.iter
        begin fun (parser, stats) ->
            if stats.Exec_info.count > !min_count && stats.time > !min_time then
                Printf.fprintf ch "%s =\n    Count: %.1f\n    Time: %.0fns\n"
                    parser stats.count (Core_kernel.Float.round_up (stats.time /. 10.) *. 10.)
        end
        list;
    close_out ch

let () =
    Arg.parse speclist anon_fun "";

    let trace, (module Parse) = mk_traced !memo_spec in
    let filename = !input in
    let src = read_file ~filename in

    begin if String.sub filename (String.length filename - 4) 4 = ".res" then
        let _x = Parse.parse_implementation ~src ~filename in
        print_stats trace
    else if String.sub filename (String.length filename - 5) 5 = ".resi" then
        let _x = Parse.parse_interface ~src ~filename in
        print_stats trace
    else
        failwith filename
    end;
