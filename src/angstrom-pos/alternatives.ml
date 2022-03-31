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

    let peek_first expected = choice expected
end

type measured_info = {
    mutable time: int;
    mutable count: int;
}

module MakeTraced (Pos : Sigs.POS) = struct
    let entries = ref []
    let depth = ref 0

    let traced p =
        Parser.{ p with p =
            { Angstrom.Expose.Parser.run = fun input pos more fail succ ->
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
