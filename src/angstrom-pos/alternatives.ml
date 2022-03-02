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


module MakeTraced (Pos : Sigs.POS) = struct
    let entries = ref []
    let depth = ref 0

    let traced p =
        Parser.{ p with p =
            { Pos.Angstrom.Expose.Parser.run = fun input pos more fail succ ->
                let enter_time = Sys.time () in
                let enter_pos = Pos.Expose.make_position pos in
                let d = !depth in
                depth := d + 1;

                let finalize pos succeded =
                    depth := d;

                    let exit_pos = Pos.Expose.make_position pos in
                    let exit_time = Sys.time () in

                    let entry =
                        Exec_info.{
                            id = p.Parser.id;
                            enter_time;
                            enter_pos;
                            exit_time;
                            exit_pos;
                            succeded;
                            depth = d;
                        }
                    in

                    entries := entry :: !entries;
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
end
