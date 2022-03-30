open Core_kernel

module MakeNotMemoized (Pos : Sigs.TPC) = struct
    open Pos
    open Parser

    let memo p =
        let id = Id.get() in
        let _ = Hashtbl.add Pos.memoid2id ~key:id ~data:p.id in
        let _ = Hashtbl.add Pos.id2memoid ~data:id ~key:p.id in
        { p with id }
end

module MakeNotPeek (Pos : Sigs.TPC) = struct
    open Pos

    let peek_first expected =
        List.fold_right ~f:(fun c p -> c <|> p) expected ~init:fail
end

module MakeTraced (Pos : Sigs.TPC) = struct
    let entries = ref []
    let depth = ref 0
    let max_pos = ref Lexing.dummy_pos

    let traced p =
        Parser.{ p with p =
            { Parser.run = fun state fail succ ->
                let enter_time = Sys.time () in
                let enter_pos = Parser.next_token_start state in
                let d = !depth in
                depth := d + 1;

                let finalize state succeded =
                    depth := d;

                    let exit_pos = Parser.next_token_start state in
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

                    begin match Hashtbl.find Pos.id2name p.id with
                    | Some name ->
                        if enter_pos.pos_cnum > (!max_pos).pos_cnum then
                            begin
                                max_pos := enter_pos;
                                let spc = String.make !depth ' ' in
                                print_endline (Printf.sprintf "%s%s %d:%d" spc name enter_pos.pos_lnum (enter_pos.pos_cnum - enter_pos.pos_bol + 1));
                                entries := [entry]
                            end
                    | None -> ()
                    end;

                    (* entries := entry :: !entries *);
                in

                let succ state v =
                    finalize state true;
                    succ state v
                in

                let fail state =
                    finalize state false;
                    fail state
                in

                p.p.run state fail succ
            }
        }

    let named name p = traced @@ Pos.named name p
    let memo p = traced @@ Pos.memo p
end
