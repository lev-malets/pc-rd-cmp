open Core_kernel
open Run_common

let tokenize = ref false
let anon_fun _ = ()

let speclist =
    [ "--tokenize", Arg.Set tokenize, ""
    ] @ speclist


let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = mk_parse ~tokenize:!tokenize () in

    let filename = !input in
    let src = read_file ~filename in

    let entries =
        let open Parse in
        let open Comb in
        begin match Filename.extension filename with
        | ".res" ->
            let _, x = parse_string_with_trace structure_parser ~filename src in
            x
        | ".resi" ->
            let _, x = parse_string_with_trace signature_parser ~filename src in
            x
        | _ ->
            failwith filename
        end
    in

    let stats = Exec_info.to_stats entries in
    let list = ref [] in
    let ind_time_sum = ref 0. in

    let ch = Out_channel.create !output in

    Hashtbl.iteri
        ~f:begin fun ~key:parser ~data ->
            ind_time_sum := !ind_time_sum +. data.time_individual.sum;
            let name = Parse.Comb.name_of_id parser in
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
