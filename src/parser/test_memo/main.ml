open Core_kernel
open Run_common

let input = ref ""
let output = ref ""
let tokenize = ref false
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, ""
    ; "--output", Arg.Set_string output, ""
    ; "--tokenize", Arg.Set tokenize, ""
    ]


let () =
    Arg.parse speclist anon_fun "";

    let (module Traced), (module APosTraced), (module ParseTraced) = mk_traced ~peek:true ~memo:true ~tokenize:!tokenize () in

    let filename = !input in
    let src = read_file ~filename in

    begin match Filename.extension filename with
    | ".res" ->
        let _ = ParseTraced.parse_implementation ~src ~filename in
        ()
    | ".resi" ->
        let _ = ParseTraced.parse_interface ~src ~filename in
        ()
    | _ ->
        failwith filename
    end;

    let stats = Exec_info.to_stats @@ List.rev !Traced.entries in
    let list = ref [] in
    let ind_time_sum = ref 0. in

    let ch = Out_channel.create !output in

    Hashtbl.iteri
        ~f:begin fun ~key:parser ~data ->
            ind_time_sum := !ind_time_sum +. data.time_individual.sum;
            let name = APosTraced.name_of_id parser in
            if Char.(name.[0] <> '\'') || true then
            list := (name, data) :: !list
        end
        stats;

    let list = List.sort !list
        ~compare:begin fun (_n1, _s1) (_n2, _s2) ->
            Float.compare
                _s1.Exec_info.time_individual.sum
                _s2.time_individual.sum
        end
        |> List.rev
    in

    (*Printf.fprintf ch "memo table size: %d\n\n" (Hashtbl.length Measured.table);*)
    Printf.fprintf ch "%30s | %10s | %10s | %16s | %16s | %16s | %12s\n\n"
        "name" "call count" "cc per pos" "time" "time per call" "ind time" "ind time (%)";

    List.iter
        ~f:begin fun (name, stats) ->
            Printf.fprintf ch "%30s | %10d | %10.4f | %16.1f | %16.2f | %16.1f | %12.4f\n"
                name
                stats.Exec_info.call_count
                (
                    Float.of_int stats.call_count
                    /.
                    Float.of_int stats.Exec_info.pos_count
                )
                (stats.Exec_info.time.sum *. 1_000_000.)
                Exec_info.FloatStatistics.(mean stats.time *. 1_000_000.)
                (stats.Exec_info.time_individual.sum *. 1_000_000.)
                (stats.Exec_info.time_individual.sum /. !ind_time_sum *. 100.)
        end
        list;
    Out_channel.close ch
