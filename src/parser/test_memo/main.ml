open Run_common

module Peek = Angstrom_pos.Peek.MakePeek(Pc_syntax.Basic.APos)
module Measured =
    Angstrom_pos.Trace.Measured
        (Pc_syntax.Basic.APos)
        (struct let memo_spec = Pc_syntax.Parser.memo_spec end)
module Traced =
    Angstrom_pos.Trace.Traced
        (Pc_syntax.Basic.APos)
        (struct let memo_spec = Pc_syntax.Parser.memo_spec end)

module ParseMeasured =
    Pc_syntax.Parser.Make
        (struct
            module Named = Measured
            module Peek = Peek
        end)
module ParseTraced =
    Pc_syntax.Parser.Make
        (struct
            module Named = Traced
            module Peek = Peek
        end)


let input = ref ""
let output = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, ""
    ; "--output", Arg.Set_string output, ""
    ]

let print_stats _ =
    let stats = Exec_info.to_stats Traced.tt in
    let list = ref [] in

    let ch = open_out !output in

    Hashtbl.iter
        begin fun parser stats ->
            if parser.[0] != '\'' then
            list := (parser, stats) :: !list
        end
        stats;

    let list = List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !list in

    (*Printf.fprintf ch "memo table size: %d\n\n" (Hashtbl.length Measured.table);*)
    Printf.fprintf ch "%40s | %10s |  | \n\n" "name" "call count";

    List.iter
        begin fun (parser, stats) ->
            let i = Hashtbl.find Measured.name2id parser in

            Printf.fprintf ch "%40s | %16d | %16.4f | %16d | %16.2f\n"
                parser
                Measured.counts.(i) stats.Exec_info.call_count
                Measured.times.(i)
                (float_of_int Measured.times.(i) /. float_of_int Measured.counts.(i))
        end
        list;
    close_out ch

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in
    let src = read_file ~filename in

    match Filename.extension filename with
    | ".res" ->
        let _ = ParseTraced.parse_implementation ~src ~filename in
        let _ = ParseMeasured.parse_implementation ~src ~filename in
        print_stats ()
    | ".resi" ->
        let _ = ParseTraced.parse_interface ~src ~filename in
        let _ = ParseMeasured.parse_interface ~src ~filename in
        print_stats ()
    | _ ->
        failwith filename
