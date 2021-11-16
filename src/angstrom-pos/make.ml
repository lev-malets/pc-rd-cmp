
module Make(T: sig type s end) = struct

    module Angstrom = Angstrom_mod.Angstrom.Make(struct type s = T.s State.t end)
    include Angstrom

    type s = T.s

    type 'b getter = { get: 'a. ('b -> 'a Parser.t) -> 'a Parser.t }

    let (>>) = ( *> )
    let (<<) = ( <* )

    let parse_string p state ?(filename = "none") =
        parse_string ~consume:All p (State.make filename state)

    let make_position pos s =
        let open State in
        let {line; info; _} = s in
        let (pos_lnum, pos_bol, pos_cnum) =
            (line.no, line.start, pos)
        in

        let open Lexing in
        { info.default_position with
            pos_lnum;
            pos_bol;
            pos_cnum;
        }

    let fix_poly f =
        let rec res = lazy (f getter)
        and getter =
            { get = fun get ->
                { run = fun i -> (get @@ Lazy.force res).run i }
            }
        in
        Lazy.force res

    let (>>$) (p: _ Parser.t) (v: 'a): 'a Parser.t =
        { run = fun input pos state more fail succ ->
            let succ input pos state more _ = succ input pos state more v in
            p.run input pos state more fail succ
        }

    let position = with_state @@ make_position
    let pos = position

    let new_line =
        (string "\n" << state_map (fun pos s -> State.{s with line = { no = s.line.no + 1; start = pos }}))
        <|>
        (string "\r\n" << state_map (fun pos s -> State.{s with line = { no = s.line.no + 1; start = pos }}))

    let whitespace: unit Parser.t =
        { run = fun input pos state more fail succ ->
            let module Input = Angstrom_mod.Input in
            let open State in
            let len = Input.length input in
            let char = Input.unsafe_get_char input in

            let succ lines start pos1 =
                let new_state = match lines <> 0 with
                    | true ->
                        { state with
                            line = { no = state.line.no + lines; start };
                        }
                    | false -> state
                in
                succ input pos1 new_state more ()
            in
            let rec loop lines start pos =
                match pos < len with
                | true ->
                    begin match char pos with
                    | ' ' -> loop lines start @@ pos + 1
                    | '\n' -> loop (lines + 1) (pos + 1) @@ pos + 1
                    | '\t' -> loop lines start @@ pos + 1
                    | '\r' ->
                        begin match pos + 1 < len && char (pos + 1) = '\n' with
                        | true -> loop (lines + 1) (pos + 2) @@ pos + 2
                        | false -> fail input pos state more [] "ws_pos"
                        end
                    | _ -> succ lines start pos
                    end
                | false -> succ lines start pos
            in
            loop 0 0 pos
        }

    let state_get = let open State in state_get >>| fun s -> s.custom
    let state_map f = let open State in state_map (fun _pos s -> {s with custom = f s.custom})
    let state_set custom = state_map (fun _ -> custom)

    let get_input_idx =
        let table = Hashtbl.create 8 in
        fun a ->
            let key = Obj.repr (Obj.magic a) in
            match Hashtbl.find_opt table key with
            | Some idx -> idx
            | None -> 0
            (*
                let idx = Hashtbl.length table in
                Hashtbl.replace table key idx;
                idx
            *)

    let memo : 'a Parser.t -> 'a Parser.t = fun p ->
        let table = Hashtbl.create 16 in

        { run = fun input pos state more fail succ ->
            let key = (get_input_idx input, pos) in
            match Hashtbl.find_opt table key with
            | Some x ->
                (match x with
                | Ok (input', pos', state', more', v) -> succ input' pos' state' more' v
                | Error (input', pos', state', more', marks', msg') -> fail input' pos' state' more' marks' msg'
                )
            | None ->
                let succ' input' pos' state' more' v =
                    Hashtbl.replace table key (Ok (input', pos', state', more', v));
                    succ input' pos' state' more' v
                in
                let fail' input' pos' state' more' marks' msg' =
                    Hashtbl.replace table key (Error (input', pos', state', more', marks', msg'));
                    fail input' pos' state' more' marks' msg'
                in
                p.run input pos state more fail' succ'
        }

(*
    let memo_fk k =
        let inputs = ref [] in
        let krs = Hashtbl.create 8 in
        let input_idx = get_idx inputs in

        fun i p s m marks msg ->
            let key = (input_idx i, p, s, m) in
            match Hashtbl.find_opt krs key with
            | Some x -> x
            | None ->
                let x = k i p s m marks msg in
                Hashtbl.replace krs key x;
                x

    let memo_sk k =
        let inputs = ref [] in
        let krs = Hashtbl.create 8 in
        let input_idx = get_idx inputs in

        fun i p s m v ->
            let key = (input_idx i, p, s, m, v) in
            match Hashtbl.find_opt krs key with
            | Some x -> x
            | None ->
                let x = k i p s m v in
                Hashtbl.replace krs key x;
                x

    let memo_cps : 'a. 'a Parser.t -> 'a Parser.t = fun p ->
        let open Angstrom_mod.State in
        let inputs = ref [] in
        let table = Hashtbl.create 16 in

        let input_idx = get_idx inputs in

        { run = fun input pos state more fail succ ->
            let key = (get_idx inputs input, pos) in
            let x = match Hashtbl.find_opt table key with
            | Some a -> a
            | None ->
                let f = p.run input pos state more in

                let k_args = ref [] in

                let rs = Hashtbl.create 8 in
                let done_ = ref None in

                let succ_conts = Hashtbl.create 8 in
                let x = fun fail succ ->
                    let skk = Hashtbl.length succ_conts in
                    let _ = Hashtbl.replace succ_conts skk succ in

                    if Hashtbl.length succ_conts = 1 then begin
                        let process_kargs = fun args key f klist ->
                            match Hashtbl.find_opt rs key with
                            | Some x -> x
                            | None ->
                                Hashtbl.replace rs key @@ Fail (-1, [], "no result");
                                k_args := args :: !k_args;

                                let rec loop = function
                                    | [] -> Hashtbl.find rs key
                                    | k::ks ->
                                        let r = f k in
                                        let old = Hashtbl.find rs key in
                                        match old with
                                        | Fail _ ->
                                            begin match r with
                                            | Done _ -> done_ := Some r;
                                            | _ -> ()
                                            end;
                                            Hashtbl.replace rs key r;
                                            loop ks
                                        | x -> x
                                in
                                loop klist
                        in

                        let succ' = fun input pos state more v ->
                            process_kargs
                                (input, pos, state, more, v)
                                (input_idx input, pos, state, more, v)
                                (fun k -> k input pos state more v)
                                (Hashtbl.fold (fun _k v l -> v::l) succ_conts [])
                        in
                        f fail succ'
                    end
                    else begin
                        let res = ref @@ Fail (-1, [], "no result") in

                        let join = function
                            | Fail _ -> fun x -> x
                            | x -> fun _ -> x
                        in

                        if List.length !k_args > 0 then
                            List.iter
                                begin fun (i, p, s, m, v) ->
                                    let key, r, kres =
                                        let key = (input_idx i, p, s, m, v) in
                                        key, Hashtbl.find rs key, succ i p s m v
                                    in
                                    res := join !res kres;
                                    let new_res = join r kres in
                                    match new_res with
                                    | Done _ -> done_ := Some new_res
                                    | _ -> ()
                                    ;
                                    Hashtbl.replace rs key new_res;
                                end
                                !k_args
                        else
                            res := fail input pos state more [] "no result"
                        ;
                        !res
                    end
                in
                Hashtbl.replace table key x;
                x
            in
            x fail succ
        }
*)
    let try_fail (p: 'a Parser.t): 'a Parser.t =
        { run = fun input pos state more fail succ ->
            let succ' = fun input' pos' state' more' v ->
                match succ input' pos' state' more' v with
                | Angstrom_mod.State.Fail _ -> fail input' pos state more' [] "try fail"
                | x -> x
            in

            p.run input pos state more fail succ'
        }

    let failed (p: _ Parser.t): unit Parser.t =
        { run = fun input pos state more fail succ ->
            let succ' input' _ _ more' _ =
                fail input' pos state more' [] ""
            in
            let fail' input' _ _ more' _ _ =
                succ input' pos state more' ()
            in

            p.run input pos state more fail' succ'
        }

    let opt p = option None (p >>| fun x -> Some x)
    let opt_ p = option () (p >>$ ())

    let seq min_n ?sep ?(trail = false) p =
        let tail = match sep with
            | None -> many p
            | Some sep ->
                let t = many (sep >> p) in
                if trail then t << opt_ sep else t
        in
        let list =
            map2 p tail
            ~f:begin fun first tail -> first::tail end
            <|>
            return []
        in
        list >>= fun list ->
        if List.length list < min_n then
            fail ""
        else
            return list
    let trail = true

    let upper = function 'A' .. 'Z' -> true | _ -> false
    let lower = function 'a' .. 'z' | '_' -> true | _ -> false

    let make_location loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
    let comb_location loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end
    let loc_comb loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end

    let ( <*>* ) a b = a <*> many b
    let ( <*>+ ) a b = a <*> many1 b
    let ( <*>? ) a b = a <*> opt b

    let mapping = return

    let loc p =
        mapping begin fun p1 x p2 -> Location.mkloc x @@ make_location p1 p2 end
        <*> pos <*> p <*> pos

    let loc_of p =
        mapping make_location
        <*> position <* p <*> position

    let id: 'a. 'a -> 'a = fun x -> x
    let t2: 'a 'b. 'a -> 'b -> 'a * 'b = fun a b -> a, b
    let t3: 'a 'b 'c. 'a -> 'b -> 'c -> 'a * 'b * 'c = fun a b c -> a, b, c
    let t4: 'a 'b 'c 'd. 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd = fun a b c d -> a, b, c, d
    let cons x xs = x :: xs

    let fold_left_0_n ~f nil p =
        let rec loop acc =
            (p >>= fun x -> loop @@ f acc x)
            <|>
            return acc
        in
        nil >>= loop

    let fold_left_0_1 :
        'a 'b 'c. f:('a -> 'b -> 'a) -> fr:('a -> 'c) -> 'a Parser.t -> 'b Parser.t -> 'c Parser.t
        = fun ~f ~fr nil p ->
            map2 nil (opt p)
            ~f:begin fun n t ->
                match t with
                | Some t -> f n t
                | None -> n
            end
            >>| fr

    let fold_right_0_n ~f p nil =
        fix @@ fun loop ->
            map2 p loop
            ~f:begin fun p acc -> f p acc end
            <|>
            nil

    let fold_left_cont_0_n nil cont =
        let tail = fix @@ fun tail ->
            mapping begin fun cont tail ->
                fun prev ->
                    match tail with
                    | None -> cont prev
                    | Some tail -> tail (cont prev)
            end
            <*> cont <*>? tail
        in

        mapping begin fun x tail ->
            match tail with
            | None -> x
            | Some tail -> tail x
        end
        <*> nil <*>? tail

    let fold_left_cont_0_1 nil cont =
        mapping begin fun x cont ->
            match cont with
            | None -> x
            | Some cont -> cont x
        end
        <*> nil <*>? cont
end
