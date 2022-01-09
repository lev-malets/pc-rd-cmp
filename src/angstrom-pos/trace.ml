module Stub (Pos : Sigs.POS) = struct
    module Parser = Pos.Parser

    let p _str p = p
end

module Memoized (Pos : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Parser = Pos.Parser

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
        | Some _ -> Pos.memo p
        | None -> p
end

module Traced (Pos : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Memoized = Memoized (Pos) (Spec)
    include Memoized

    let tt = Exec_info.make ()

    let branch = ref tt

    let traced name p =
        begin match Hashtbl.find_opt occupied_names name with
        | None -> Hashtbl.replace occupied_names name ()
        | Some _ -> failwith ("name '" ^ name ^ "' occupied")
        end;

        Pos.Parser.{ p with p =
            let open Pos.Angstrom in

            return () >>= fun _ ->
                let parent_branch = !branch in
                let subs = Exec_info.make () in

                let finalize enter_pos enter_time exit =
                    Exec_info.add_entry
                        parent_branch
                        ~name ~enter_pos ~enter_time ~exit ~subs;

                    branch := parent_branch;
                in

                branch := subs;

                Pos.pos.p
                >>= fun p1 ->
                    (*print_endline name;*)
                    (*Printf.printf "%d:%d" p1.pos_lnum (p1.pos_cnum - p1.pos_bol + 1);*)

                    let enter_time = Sys.time () in
                    p.p <|>
                        (
                            return () >>= fun _ ->
                                finalize p1 enter_time Error;
                                fail name
                        )
                >>= fun x ->
                    Pos.pos.p
                >>| fun p2 ->
                    finalize p1 enter_time @@ Exit { pos = p2 };
                    x
        }

    let p name p =
        match Hashtbl.find_opt memo_names name with
        | Some _ -> traced (name ^ " > memo") @@ Pos.memo @@ traced name p
        | None -> traced name p
end
