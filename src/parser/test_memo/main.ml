open Core_kernel
open Run_common

let input = ref ""
let output = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, ""
    ; "--output", Arg.Set_string output, ""
    ]


let () =
    Arg.parse speclist anon_fun "";

    let (module Traced), (module APosTraced), (module ParseTraced) = mk_traced ~peek:true () in
    let (module Measured), (module APosMeasured), (module ParseMeasured) = mk_measured ~peek:true () in

    let filename = !input in
    let src = read_file ~filename in

    begin match Filename.extension filename with
    | ".res" ->
        let _ = ParseTraced.parse_implementation ~src ~filename in
        let _ = ParseMeasured.parse_implementation ~src ~filename in
        ()
    | ".resi" ->
        let _ = ParseTraced.parse_interface ~src ~filename in
        let _ = ParseMeasured.parse_interface ~src ~filename in
        ()
    | _ ->
        failwith filename
    end;

    let stats = Angstrom_pos.Alt.Exec_info.to_stats Traced.tt in
    let list = ref [] in

    let ch = Out_channel.create !output in

    Hashtbl.iteri
        ~f:begin fun ~key:parser ~data:stats ->
            let name = APosTraced.name_of_id parser in
            if Char.(name.[0] <> '\'') then
            list := (name, stats) :: !list
        end
        stats;

    let list = List.sort ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2) !list in

    (*Printf.fprintf ch "memo table size: %d\n\n" (Hashtbl.length Measured.table);*)
    Printf.fprintf ch "%40s | %16s | %16s | %16s | %16s | %16s | %16s\n\n"
        "name" "call count" "cc per pos" "time" "s time" "s time per call" "ind time";

    List.iter
        ~f:begin fun (name, stats) ->
            let info_time =
                Hashtbl.find APosMeasured.name2id name
                |> Option.value_map ~default:(-1)
                    ~f:(fun id -> (Hashtbl.find_exn Measured.id2info id).time)
            in

            Printf.fprintf ch "%40s | %16d | %16.4f | %16d | %16d | %16.2f | %16d\n"
                name
                stats.Angstrom_pos.Alt.Exec_info.call_count
                (
                    Float.of_int stats.call_count
                    /.
                    Float.of_int stats.Angstrom_pos.Alt.Exec_info.pos_count
                )
                info_time
                stats.Angstrom_pos.Alt.Exec_info.time.sum
                Angstrom_pos.Alt.Exec_info.IntStatistics.(mean stats.time)
                stats.Angstrom_pos.Alt.Exec_info.time_individual.sum
        end
        list;
    Out_channel.close ch
