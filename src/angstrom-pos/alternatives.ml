open Core_kernel

module MakeNotMemoized (Pos : Sigs.POS) = struct
    open Pos

    let memo p =
        let id = Id.get() in
        let _ = Hashtbl.add Pos.memoid2id ~key:id ~data:p.Parser.id in
        let _ = Hashtbl.add Pos.id2memoid ~data:id ~key:p.Parser.id in
        Parser.{p with id}
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
        let open Parser in
        let info = { time = 0; count = 0 } in
        Hashtbl.add_exn id2info ~key:p.id ~data:info;

        let start_times = ref [] in
        let open Pos.Angstrom in
        let start =
            exec (fun _ -> start_times := Caml.Sys.time () :: !start_times)
        in
        let stop =
            (exec (fun _ ->
                let time = Int.of_float ((Caml.Sys.time () -. List.hd_exn !start_times) *. 1_000_000_000.) in
                info.time <- info.time + time;
                info.count <- info.count + 1;
                start_times := List.tl_exn !start_times))
        in
        { p with p = start >> p.p << stop <|> (stop >> fail "") }

    let named name p = measured @@ Pos.named name p
    let memo p = measured @@ Pos.memo p
end

module Exec_info = struct
    type entry_info =
        { id: int
        ; enter_pos: Lexing.position
        ; exit_pos : Lexing.position
        ; succeded : bool
        ; time     : int
        }

    type entry =
        { info: entry_info
        ; subs: t
        }
    and t =
        { mutable entries : entry list
        }

    let make _ = { entries = [] }

    let add_entry t ~id ~enter_pos ~exit_pos ~succeded ~subs ~time =
        let info =
            { id; enter_pos; exit_pos; succeded; time }
        in

        t.entries <- {info; subs} :: t.entries

    module PosKey = struct
        type t = Lexing.position

        let compare p1 p2 =
            let open Lexing in

            let c1 = compare p1.pos_cnum p2.pos_cnum in
            if c1 <> 0 then c1 else

            String.compare p1.pos_fname p2.pos_fname

        let sexp_of_t _ = failwith ""
        let hash p = p.Lexing.pos_bol
    end

    module IntStatistics = struct
        type t =
            { sum   : int
            ; min   : int
            ; max   : int
            ; count : int
            }

        let empty =
            { sum = 0
            ; min = Int.min_value
            ; max = Int.max_value
            ; count = 0
            }

        let add x t =
            { sum = t.sum + x
            ; min = Int.min x t.min
            ; max = Int.max x t.max
            ; count = t.count + 1
            }

        let mean t = (Float.of_int t.sum /. Float.of_int t.count)
    end

    module Collect = struct
        type stats =
            { positions                   : (PosKey.t, unit) Hashtbl.t
            ; mutable time                : IntStatistics.t
            ; mutable time_individual     : IntStatistics.t
            }

        let rec collect_from_entry table e =
            let stats = Hashtbl.find_or_add table e.info.id
                ~default:begin fun _ ->
                    { positions = Hashtbl.create (module PosKey)
                    ; time = IntStatistics.empty
                    ; time_individual = IntStatistics.empty
                    }
                end
            in

            Hashtbl.change stats.positions e.info.enter_pos ~f:(fun _ -> Some ());
            stats.time <- IntStatistics.add e.info.time stats.time;

            let time_sub = List.fold ~f:(fun acc x -> acc + x.info.time) ~init:0 e.subs.entries in
            stats.time_individual <- IntStatistics.add (e.info.time - time_sub) stats.time_individual;
            collect table e.subs
        and collect table t = List.iter ~f:(collect_from_entry table) t.entries
    end

    type stats =
        { call_count               : int
        ; pos_count                : int
        ; time                     : IntStatistics.t
        ; time_individual          : IntStatistics.t
        }

    let to_stats t =
        let table = Hashtbl.create (module Int) in

        Collect.collect table t;
        Hashtbl.map table ~f:begin fun stats ->
            { call_count = stats.Collect.time.count
            ; pos_count = Hashtbl.length stats.positions
            ; time = stats.time
            ; time_individual = stats.time_individual
            }
        end

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
        Parser.{ p with p =
            { Pos.Angstrom.Expose.Parser.run = fun input pos more fail succ ->
                let parent_branch = !branch in
                let subs = Exec_info.make () in
                branch := subs;

                let enter_pos = Pos.Expose.make_position pos in
                let enter_time = Sys.time () in

                let finalize pos succeded =
                    let exit_time = Sys.time () in
                    let time = Int.of_float_unchecked ((exit_time -. enter_time) *. 1_000_000_000.) in
                    let exit_pos = Pos.Expose.make_position pos in
                    Exec_info.add_entry
                        parent_branch
                        ~id:p.id ~enter_pos ~exit_pos ~subs ~succeded ~time;

                    branch := parent_branch
                in

                let succ input' pos' more' v =
                    finalize pos' true;
                    succ input' pos' more' v
                in

                let fail input' pos' more' marks' msg' =
                    finalize pos' false;
                    fail input' pos' more' marks' msg'
                in

                p.p.run input pos more fail succ
            }
        }

    let named name p = traced @@ Pos.named name p
    let memo p = traced @@ Pos.memo p

    let nop = named "nop" @@ Pos.return ()
    let nop_inner = named "nop-inner" @@ Pos.return ()
    let nop_outer = named "nop-outer" @@ nop_inner

    let hlp =
        Pos.(>>)
        (Array.create ~len:999 nop  |> Array.fold ~init:nop ~f:Pos.(>>))
        (Array.create ~len:999 nop_outer  |> Array.fold ~init:nop_outer ~f:Pos.(>>))
end
