module Stub (Pos : Sigs.POS) = struct
    module Parser = Pos.Parser

    let p _str p = p
end

module Memoized (Pos : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Parser = Pos.Parser

    let memo p =
        let open Parser in

        let table = Hashtbl.create 0 in

        { p with
            p = { run = fun input pos state more fail succ ->
                let input_i: int = Obj.magic (Obj.repr input) in
                let input_hi = input_i lsr 4 in
                let input_li = input_i land 4 in

                let key = (input_hi, input_li, pos) in
                match Hashtbl.find_opt table key with
                | Some x ->
                    (match x with
                    | Ok (input', pos', state', more', v) -> succ input' pos' state' more' v
                    | Error (input', pos', state', more', marks', msg') -> fail input' pos' state' more' marks' msg'
                    )
                | None ->
                    let succ' input' pos' state' more' v =
                        Hashtbl.replace table key @@ Ok (input', pos', state', more', v);
                        succ input' pos' state' more' v
                    in
                    let fail' input' pos' state' more' marks' msg' =
                        Hashtbl.replace table key @@ Error (input', pos', state', more', marks', msg');
                        fail input' pos' state' more' marks' msg'
                    in
                    p.p.run input pos state more fail' succ'
            }
        }

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
        | Some _ -> memo p
        | None -> p
end

module Measured (Pos : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Memoized = Memoized (Pos) (Spec)
    include Memoized

    let name2id = Hashtbl.create 1024
    let id2name = Array.make 1024 ""
    let new_id name =
        let id = Hashtbl.length name2id in
        Hashtbl.replace name2id name id;
        id2name.(id) <- name;
        id
    let get_id name =
        match Hashtbl.find_opt name2id name with
        | Some id -> id
        | None -> new_id name

    let memo_names =
        let tbl = Hashtbl.create @@ List.length Spec.memo_spec in
        List.iter (fun x -> Hashtbl.add tbl x ()) Spec.memo_spec;
        tbl

    let times = Array.make 1024 0
    let counts = Array.make 1024 0

    let measure i p =
        let start_times = ref [] in
        let open Pos.Angstrom in
        let start =
            exec (fun _ -> start_times := Sys.time () :: !start_times)
        in
        let stop =
            (exec (fun _ ->
                let time = int_of_float ((Sys.time () -. List.hd !start_times) *. 1000000.) in
                counts.(i) <- counts.(i) + 1;
                times.(i) <- times.(i) + time;
                start_times := List.tl !start_times))
        in
        { p with Pos.Parser.p = start >> p.Pos.Parser.p << stop <|> (stop >> fail "") }

    let p name p =
        let id =
            match Hashtbl.find_opt name2id name with
            | Some _ -> failwith ("name '" ^ name ^ "' occupied")
            | None -> new_id name
        in

        match Hashtbl.find_opt memo_names name with
        | Some _ ->
            let memo_id = new_id @@ name ^ "*" in
            measure memo_id (memo (measure id p))
        | None -> measure id p
end

module Traced (Pos : Sigs.POS) (Spec: sig val memo_spec: string list end) = struct
    module Memoized = Memoized (Pos) (Spec)
    include Memoized

    let tt = Exec_info.make ()

    let idx = ref 0

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

                let finalize enter_pos exit =
                    Exec_info.add_entry
                        parent_branch
                        ~name ~enter_pos ~exit ~subs;

                    branch := parent_branch;
                in

                branch := subs;

                Pos.pos.p
                >>= fun p1 ->
                    p.p <|> (exec (fun _ -> finalize p1 Error) >> fail name)
                >>= fun x ->
                    Pos.pos.p
                >>| fun p2 ->
                    finalize p1 @@ Exit { pos = p2 };
                    x
        }

    let p name p =
        match Hashtbl.find_opt memo_names name with
        | Some _ -> traced (name ^ "*") @@ memo @@ traced name p
        | None -> traced name p
end
