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

    let tt = Exec_info.make ()

    let branch = ref tt

    let traced name p =
        let open Angstrom in
        begin match Hashtbl.find_opt occupied_names name with
        | None -> Hashtbl.replace occupied_names name ()
        | Some _ -> failwith ("name '" ^ name ^ "' occupied")
        end;

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
end
