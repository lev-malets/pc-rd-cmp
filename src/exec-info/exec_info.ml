include Types

type entry_info =
    { name: string
    ; enter_pos: Lexing.position
    ; enter_time: float
    ; exit_time: float
    ; exit: exit_kind
    }

type entry =
    { info: entry_info
    ; subs: entry CCVector.vector
    }

type t = entry CCVector.vector

let make = CCVector.create

let add_entry t ~name ~enter_pos ~enter_time ~exit ~subs =
    let exit_time = Sys.time () in
    let info =
        { name; enter_pos; enter_time
        ; exit_time; exit
        }
    in

    CCVector.push t {info; subs}

let rec to_html_s t =
    let visualize_entry entry =
        let _subs_height, _subs_width, subs_repr = to_html_s entry.subs in
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
        let pos = (CCVector.get t i).info.enter_pos in
        let rec loop i =
            if i >= CCVector.length t then i - 1
            else if (CCVector.get t i).info.enter_pos <> pos then i - 1
            else loop @@ i + 1
        in
        let last_i = loop @@ i + 1 in
        let buf = Buffer.create 0 in
        let rec loop i =
            if i <= last_i then
                let _, _, s = visualize_entry @@ CCVector.get t i in
                Buffer.add_string buf s;
                loop @@ i + 1
            else
                Buffer.contents buf
        in

        last_i, Printf.sprintf "<div style='display:table-cell;margin-left:4em'>%s</div>" @@ loop i
    in

    let buf = Buffer.create 0 in
    let rec loop i =
        if i >= CCVector.length t then ()
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

let to_html_s t = let _, _, s = to_html_s t in s

let to_html och t = Printf.fprintf och "%s" @@ to_html_s t

let to_pos_stats t =
    let stats = Hashtbl.create 0 in

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
    collect_stats t;
    stats

let to_stats t =
    let pos_stats = to_pos_stats t in
    let table = Hashtbl.create @@ Hashtbl.length pos_stats in
    Hashtbl.iter
        begin fun parser pos_stats ->
            if Hashtbl.length pos_stats <> 0 then

            let sum = ref 0 in
            let time = ref 0. in
            let count = ref 0 in

            Hashtbl.iter
                begin fun _pos (count_, time_) ->
                    sum := !sum + count_;
                    time := !time +. time_ /. (float_of_int count_);
                    count := !count + 1;
                end
                pos_stats;

            let stats =
                { time = !time *. (10. ** 6.) /. (float_of_int !count)
                ; count = (float_of_int !sum) /. (float_of_int !count)
                }
            in
            Hashtbl.replace table parser stats
        end
        pos_stats;
    table

let max (p1: Lexing.position) (p2: Lexing.position) =
    let c1 = compare p1.pos_lnum p2.pos_lnum in
    if c1 > 0 then 1
    else if c1 < 0 then -1
    else
        let c2 = compare p1.pos_cnum p2.pos_cnum in
        if c2 > 0 then 1
        else -1

let rec last_pos t =
    let entry_last_pos entry =
        let path, pos = last_pos entry.subs in
        if max entry.info.enter_pos pos = 1 then
            [entry.info.name], entry.info.enter_pos
        else
            entry.info.name :: path, pos
    in
    CCVector.fold
        begin fun ((_, pos) as acc) entry ->
            let (_, entry_pos) as x = entry_last_pos entry in
            if max pos entry_pos = 1 then
                acc
            else x
        end
        ([], Lexing.dummy_pos) t
