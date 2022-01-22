include Types

type entry_info =
    { name: string
    ; enter_pos: Lexing.position
    ; exit: exit_kind
    }

type entry =
    { info: entry_info
    ; subs: t
    }
and t =
    { mutable entries : entry list
    }

let make _ = { entries = [] }

let add_entry t ~name ~enter_pos ~exit ~subs =
    let info =
        { name; enter_pos; exit }
    in

    t.entries <- {info; subs} :: t.entries

let to_pos_stats t =
    let stats = Hashtbl.create 0 in

    let rec collect_stats vec = List.iter collect_entry_stats vec.entries
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

        let count =
            match Hashtbl.find_opt table info.enter_pos with
            | Some c -> c
            | None -> 0
        in
        Hashtbl.replace table info.enter_pos (count + 1);
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
            let count = ref 0 in

            Hashtbl.iter
                begin fun _pos count_ ->
                    sum := !sum + count_;
                    count := !count + 1;
                end
                pos_stats;

            let stats =
                { call_count = (float_of_int !sum) /. (float_of_int !count)
                ; pos_count = !count
                }
            in
            Hashtbl.replace table parser stats
        end
        pos_stats;
    table

let gt (p1: Lexing.position) (p2: Lexing.position) =
    let c1 = compare p1.pos_lnum p2.pos_lnum in
    if c1 > 0 then true
    else if c1 < 0 then false
    else
        let c2 = compare p1.pos_cnum p2.pos_cnum in
        if c2 > 0 then true
        else false

let rec last_pos t =
    let entry_last_pos entry =
        let path, pos = last_pos entry.subs in
        if gt entry.info.enter_pos pos then
            [entry.info.name], entry.info.enter_pos
        else
            entry.info.name :: path, pos
    in
    List.fold_left
        begin fun ((_, pos) as acc) entry ->
            let (_, entry_pos) as x = entry_last_pos entry in
            if gt pos entry_pos then
                acc
            else x
        end
        ([], Lexing.dummy_pos) t.entries
