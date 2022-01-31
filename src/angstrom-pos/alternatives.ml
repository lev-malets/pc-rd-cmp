open Base

module MakeNotMemoized (Pos : Sigs.POS) = struct
    open Pos

    let memo p =
        let id = Id.get() in
        let _ = Hashtbl.add Pos.memoid2id ~key:id ~data:p.Pos.Parser.id in
        let _ = Hashtbl.add Pos.id2memoid ~data:id ~key:p.Pos.Parser.id in
        Pos.Parser.{p with id}
end

module MakeNotPeek (Pos : Sigs.POS) = struct
    open Pos

    let peek_first expected =
        List.fold_right ~f:(fun c p -> c <|> p) expected ~init:fail
end

type measured_info = {
    mutable time: int;
    mutable count: int;
}
module MakeMeasured (Pos : Sigs.POS) = struct
    let id2info = Hashtbl.create (module Int)

    let measured p =
        let open Pos.Parser in
        let info = { time = 0; count = 0 } in
        Hashtbl.add_exn id2info ~key:p.id ~data:info;

        let start_times = ref [] in
        let open Pos.Angstrom in
        let start =
            exec (fun _ -> start_times := Caml.Sys.time () :: !start_times)
        in
        let stop =
            (exec (fun _ ->
                let time = Int.of_float ((Caml.Sys.time () -. List.hd_exn !start_times) *. 1000000.) in
                info.time <- info.time + time;
                info.count <- info.count + 1;
                start_times := List.tl_exn !start_times))
        in
        { p with p = start >> p.p << stop <|> (stop >> fail "") }

    let named name p = measured @@ Pos.named name p
    let memo p = measured @@ Pos.memo p
end

module Exec_info = struct
    type exit_kind =
        | Exit of { pos: Lexing.position }
        | Error

    type stats =
        { call_count : float
        ; pos_count : int
        }

    type stats_table = (int, stats) Hashtbl.t

    type entry_info =
        { id: int
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

    let add_entry t ~id ~enter_pos ~exit ~subs =
        let info =
            { id; enter_pos; exit }
        in

        t.entries <- {info; subs} :: t.entries

    module PosKey = struct
        type t = Lexing.position

        let compare p1 p2 =
            let open Lexing in

            let c1 = compare p1.pos_cnum p2.pos_cnum in
            if c1 <> 0 then c1 else

            String.compare p1.pos_fname p2.pos_fname


        let sexp_of_t p =
            Sexp.List Lexing.[
                String.sexp_of_t p.pos_fname;
                Int.sexp_of_t p.pos_lnum;
                Int.sexp_of_t p.pos_cnum;
                Int.sexp_of_t p.pos_bol;
            ]

        let hash p = p.Lexing.pos_bol
    end

    let to_pos_stats t =
        let stats = Hashtbl.create (module Int) in

        let rec collect_stats vec = List.iter ~f:collect_entry_stats vec.entries
        and collect_entry_stats entry =
            let info = entry.info in
            let table =
                match Hashtbl.find stats info.id with
                | Some tbl -> tbl
                | None ->
                    let tbl = Hashtbl.create (module PosKey) in
                    let _ = Hashtbl.add stats ~key:info.id ~data:tbl in
                    tbl
            in

            Hashtbl.update table info.enter_pos ~f:(function Some x -> x + 1 | None -> 1);
            collect_stats entry.subs
        in
        collect_stats t;
        stats

    let to_stats t =
        let pos_stats = to_pos_stats t in
        let table = Hashtbl.create (module Int) ~size:(Hashtbl.length pos_stats) in
        Hashtbl.iteri
            ~f:begin fun ~key:parser ~data:pos_stats ->
                if Hashtbl.length pos_stats <> 0 then

                let sum = ref 0 in
                let count = ref 0 in

                Hashtbl.iter
                    ~f:begin fun count_ ->
                        sum := !sum + count_;
                        count := !count + 1;
                    end
                    pos_stats;

                let stats =
                    { call_count = (Float.of_int !sum) /. (Float.of_int !count)
                    ; pos_count = !count
                    }
                in
                let _ = Hashtbl.add table ~key:parser ~data:stats in ()
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
                [entry.info.id], entry.info.enter_pos
            else
                entry.info.id :: path, pos
        in
        List.fold_left
            ~f:begin fun ((_, pos) as acc) entry ->
                let (_, entry_pos) as x = entry_last_pos entry in
                if gt pos entry_pos then
                    acc
                else x
            end
            ~init:([], Lexing.dummy_pos) t.entries

end

module MakeTraced (Pos : Sigs.POS) = struct
    let tt = Exec_info.make ()

    let idx = ref 0

    let branch = ref tt

    let traced p =
        Pos.Parser.{ p with p =
            let open Pos.Angstrom in

            return () >>= fun _ ->
                let parent_branch = !branch in
                let subs = Exec_info.make () in

                let finalize enter_pos exit =
                    Exec_info.add_entry
                        parent_branch
                        ~id:p.id ~enter_pos ~exit ~subs;

                    branch := parent_branch;
                in

                branch := subs;

                Pos.pos.p
                >>= fun p1 ->
                    p.p <|> (exec (fun _ -> finalize p1 Error) >> fail "")
                >>= fun x ->
                    Pos.pos.p
                >>| fun p2 ->
                    finalize p1 @@ Exit { pos = p2 };
                    x
        }

    let named name p = traced @@ Pos.named name p
    let memo p = traced @@ Pos.memo p
end
