let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
let print = Fmt.string Fmt.stderr

type exit_kind =
    | Exit of { pos: Lexing.position }
    | Error

type exec_info =
    { name: string
    ; enter_pos: Lexing.position
    ; enter_time: float
    ; exit_time: float
    ; exit: exit_kind
    }

type exec_tree =
    { info: exec_info
    ; subs: exec_tree CCVector.vector
    }

module Stub (Angstrom : Sigs.POS) = struct
    module Parser = Angstrom.Parser

    let p _str p = p
end

module Memoized (Angstrom : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Parser = Angstrom.Parser

    let memo_names =
        let tbl = Hashtbl.create @@ List.length Spec.memo_spec in
        List.iter (fun x -> Hashtbl.add tbl x ()) Spec.memo_spec;
        tbl

    let occupied_names = Hashtbl.create 0

    let p name p =
        begin match Hashtbl.find_opt occupied_names name with
        | None -> Hashtbl.replace occupied_names name ()
        | Some _ -> failwith ("name '" ^ name ^ "' occupied")
        end;

        match Hashtbl.find_opt memo_names name with
        | Some _ -> Angstrom.memo p
        | None -> p
end

module Traced (Angstrom : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Memoized = Memoized (Angstrom) (Spec)
    include Memoized

    let tt = CCVector.create ()

    let branch = ref tt

    let traced name p =
        let open Angstrom in
        begin match Hashtbl.find_opt occupied_names name with
        | None -> Hashtbl.replace occupied_names name ()
        | Some _ -> failwith ("name '" ^ name ^ "' occupied")
        end;

        return () >>= fun _ ->
        let parent_branch = !branch in
        let subs = CCVector.create () in

        let finalize enter_pos enter_time exit =
            let exit_time = Sys.time () in
            let info =
                { name; enter_pos; enter_time
                ; exit_time; exit
                }
            in

            branch := parent_branch;
            CCVector.push parent_branch { info; subs }
        in

        branch := subs;

        pos
        >>= fun p1 ->
            let enter_time = Sys.time () in
            p <|>
                (
                    return () >>= fun _ ->
                        finalize p1 enter_time Error;
                        fail name
                )
        >>= fun x ->
            pos
        >>| fun p2 ->
            finalize p1 enter_time @@ Exit { pos = p2 };
            x

    let p name p =
        match Hashtbl.find_opt memo_names name with
        | Some _ -> traced (name ^ " > memo") @@ Angstrom.memo @@ traced name p
        | None -> traced name p

    let rec visualize vec =
        let visualize_entry entry =
            let _subs_height, _subs_width, subs_repr = visualize entry.subs in
            let style, title =
                match entry.info.exit with
                | Error ->
                    "background-color:#FF6666",
                    Printf.sprintf "Enter: %d:%d\nTime: %.2fus"
                        entry.info.enter_pos.pos_lnum
                        (entry.info.enter_pos.pos_cnum - entry.info.enter_pos.pos_bol + 1)
                        ((entry.info.exit_time -. entry.info.enter_time) *. (10. ** 6.))
                | Exit { pos = exit_pos } ->
                    "background-color:#66FF66",
                    Printf.sprintf "Enter: %d:%d\nExit: %d:%d\nTime: %.2fus"
                        entry.info.enter_pos.pos_lnum
                        (entry.info.enter_pos.pos_cnum - entry.info.enter_pos.pos_bol + 1)
                        exit_pos.pos_lnum
                        (exit_pos.pos_cnum - exit_pos.pos_bol + 1)
                        ((entry.info.exit_time -. entry.info.enter_time) *. (10. ** 6.))
            in

            let info_repr =
                Printf.sprintf
                "<div style='display:table-row'>
                    <div style='%s' title=\"%s\">%s</div>
                </div>"
                style title entry.info.name

            in
            1, String.length entry.info.name, Printf.sprintf
                "<div style='display:table-row'>
                    <div style='display:table;margin-left:1em;'>%s%s</div>
                </div>"
                info_repr subs_repr
        in

        let vis_tries i =
            let pos = (CCVector.get vec i).info.enter_pos in
            let rec loop i =
                if i >= CCVector.length vec then i - 1
                else if (CCVector.get vec i).info.enter_pos <> pos then i - 1
                else loop @@ i + 1
            in
            let last_i = loop @@ i + 1 in
            let buf = Buffer.create 0 in
            let rec loop i =
                if i <= last_i then
                    let _, _, s = visualize_entry @@ CCVector.get vec i in
                    Buffer.add_string buf s;
                    loop @@ i + 1
                else
                    Buffer.contents buf
            in

            last_i, Printf.sprintf "<div style='display:table-cell;margin-left:4em'>%s</div>" @@ loop i
        in

        let buf = Buffer.create 0 in
        let rec loop i =
            if i >= CCVector.length vec then ()
            else
                let last_i, s = vis_tries i in
                Buffer.add_string buf s;
                loop @@ last_i + 1
        in
        loop 0;
        0, 0, Printf.sprintf
            "<div style='display:table-row'>
                <div style='display:table-cell'>
                    %s
                </div>
            </div>"
            (Buffer.contents buf)

    let visualize () =
        let _, _, s = visualize tt in
        let s0 = "" in
        let style = ".c1 { display : inline-flex; flex-wrap: wrap; }" in
        Printf.sprintf "<style>%s%s</style>%s" s0 style s

    let collect_stats () =
        let stats = Hashtbl.create @@ Hashtbl.length occupied_names in

        let rec collect_stats vec = CCVector.iter collect_entry_stats vec
        and collect_entry_stats entry =
            let info = entry.info in
            let table =
                match Hashtbl.find_opt stats info.name with
                | Some tbl -> tbl
                | None ->
                    let tbl = Hashtbl.create 0 in
                    Hashtbl.replace stats info.name tbl;
                    tbl
            in

            let count, time =
                match Hashtbl.find_opt table info.enter_pos with
                | Some (c, t) -> c, t
                | None -> 0, 0.
            in
            Hashtbl.replace table info.enter_pos (count + 1, time +. info.exit_time -. info.enter_time);
            collect_stats entry.subs
        in
        collect_stats tt;
        stats

    let print_stats ?(filter=fun ~count:_ ~time:_ -> true) file =
        let stats = collect_stats () in
        let list = ref [] in

        Hashtbl.iter
            begin fun parser stats -> list := (parser, stats) :: !list end
            stats;

        let list = List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !list in

        List.iter
            begin fun (parser, stats) ->
                if Hashtbl.length stats <> 0 then

                let sum = ref 0 in
                let time = ref 0. in
                let count = ref 0 in

                Hashtbl.iter
                    begin fun _pos (count_, time_) ->
                        sum := !sum + count_;
                        time := !time +. time_;
                        count := !count + 1;
                    end
                    stats;

                let time = !time /. (float_of_int !count) *. (10. ** 6.) in
                let count = (float_of_int !sum) /. (float_of_int !count) in

                if filter ~count ~time then
                    Printf.fprintf file "%s =\n    Count: %.1f\n    Time: %.1fns\n"
                        parser count time
            end
            list
end
