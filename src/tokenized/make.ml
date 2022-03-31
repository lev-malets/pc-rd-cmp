open Base

module Make (Tokenizer : Sigs.TOKENIZER) (Log : sig type t end): Sigs.TPC with type s = Log.t and type tag = Tokenizer.Tag.t = struct
    type s = Log.t
    type log_elem = Log.t
    open Parser
    type tag = Tokenizer.Tag.t

    let tag2int: tag -> int = Caml.Obj.magic

    module Simple = struct
        type 'a t = ('a, tag, log_elem) Parser.simple

        let (>>) p1 p2 =
            { run = fun state fail succ ->
                let succ1 state1 _ = p2.run state1 fail succ in
                p1.run state fail succ1
            }

        let (<<) p1 p2 =
            { run = fun state fail succ ->
                let succ1 state1 v =
                    let succ2 state2 _ = succ state2 v in
                    p2.run state1 fail succ2
                in
                p1.run state fail succ1
            }

        let (>>|) p f =
            { run = fun state fail succ ->
                let succ' state' v = succ state' (f v) in
                p.run state fail succ'
            }


        let exec f =
            { run = fun state _fail succ ->
                succ state (f ())
            }

        let lift f p = p >>| f

        let lift2 f p1 p2 =
            { run = fun state fail succ ->
                let succ1 state1 v1 =
                    let succ2 state2 v2 = succ state2 (f v1 v2) in
                    p2.run state1 fail succ2
                in
                p1.run state fail succ1
            }

        let lift3 f p1 p2 p3 =
            { run = fun state fail succ ->
                let succ1 state1 v1 =
                    let succ2 state2 v2 =
                        let succ3 state3 v3 = succ state3 (f v1 v2 v3) in
                        p3.run state2 fail succ3
                    in
                    p2.run state1 fail succ2
                in
                p1.run state fail succ1
            }

        let lift4 f p1 p2 p3 p4 =
            { run = fun state fail succ ->
                let succ1 state1 v1 =
                    let succ2 state2 v2 =
                        let succ3 state3 v3 =
                            let succ4 state4 v4 = succ state4 (f v1 v2 v3 v4) in
                            p4.run state3 fail succ4
                        in
                        p3.run state2 fail succ3
                    in
                    p2.run state1 fail succ2
                in
                p1.run state fail succ1
            }

        let (<|>) p1 p2 =
            { run = fun state fail succ ->
                let fail1 _ =
                    p2.run state fail succ
                in
                p1.run state fail1 succ
            }

        let (<*>) pf pv =
            { run = fun state fail succ ->
                let succ1 state1 f =
                    let succ2 state2 v = succ state2 (f v) in
                    pv.run state1 fail succ2
                in
                pf.run state fail succ1
            }

        let (>>=) p f =
            { run = fun state fail succ ->
                let succ1 state1 v = (f v).run state1 fail succ in
                p.run state fail succ1
            }

        let (>>$) p v =
            { run = fun state fail succ ->
                let succ1 state1 _ = succ state1 v in
                p.run state fail succ1
            }

        let fix f =
            let rec p = lazy (f r)
            and r =
                { run = fun i -> (Lazy.force p).run i }
            in
            Lazy.force p

        let return x =
            { run = fun state _fail succ ->
                succ state x
            }

        let fail =
            { run = fun state fail _succ ->
                fail state
            }

        let many p = fix @@ fun many_p ->
            (lift2 (fun x xs -> x::xs) p many_p) <|> return []

        let log x =
            { run = fun state _fail succ ->
                let state = { state with log = x :: state.log } in
                succ state ();
            }

        let log_many log =
            { run = fun state _fail succ ->
                let state = { state with log; log_parts = state.log :: state.log_parts } in
                succ state ();
            }
    end

    type 'a t = ('a, tag, log_elem) Parser.t

    type 'b getter = { get: 'a. ('b -> 'a t) -> 'a t }

    module Id = struct
        let next = ref 0
        let get () = let x = !next in next := x + 1; x

        let fail = get ()
        let t2 = get ()
        let t3 = get ()
        let t4 = get ()
        let cons = get ()
        let eof = get ()
    end

    let mk_parser_cont ~p ~typ p1 p2 =
        let info =
            match p1.info with
            | Unknown -> Unknown
            | Empty -> p2.info
            | Consume { empty; first } as p1 ->
                if empty then
                    match p2.info with
                    | Unknown -> Unknown
                    | Empty -> p1
                    | Consume p2 -> Consume { p2 with first = Tset.union first p2.first }
                else
                   p1
        in

        { p; info; typ; id = Id.get() }

    let (>>) (p1 : 'a t) p2 =
        match p1.typ, p2.typ with
        | Return _, _ -> p2
        | _, Return v ->
            mk_parser_cont p1 p2 ~p:Simple.(p1.p >>| fun _ -> v) ~typ:(Value { v; p = p1.p })
        | _, Value { v; _ } ->
            let p = Simple.(p1.p >> p2.p) in
            mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
        | _, Lift { f; a } ->
            let a = Simple.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Simple.(lift f a) ~typ:(Lift { f; a })
        | _, Lift2 { f; a; b } ->
            let a = Simple.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Simple.(lift2 f a b) ~typ:(Lift2 { f; a; b })
        | _, Lift3 { f; a; b; c } ->
            let a = Simple.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Simple.(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
        | _ ->
            mk_parser_cont p1 p2 ~p:Simple.(p1.p >> p2.p) ~typ:Parser

    let (<<) p1 p2 =
        match p1.typ, p2.typ with
        | _, Return _ -> p1
        | Return v, _ ->
            mk_parser_cont p1 p2 ~p:Simple.(p2.p >>| fun _ -> v) ~typ:(Value { v; p = p2.p })
        | Value { v; _ }, _ ->
            let p = Simple.(p1.p << p2.p) in
            mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
        | Lift { f; a }, _ ->
            let a = Simple.(<<) a p2.p in
            mk_parser_cont p1 p2 ~p:Simple.(lift f a) ~typ:(Lift { f; a })
        | Lift2 { f; a; b }, _ ->
            let b = Simple.(<<) b p2.p in
            mk_parser_cont p1 p2 ~p:Simple.(lift2 f a b) ~typ:(Lift2 { f; a; b })
        | Lift3 { f; a; b; c }, _ ->
            let c = Simple.(<<) c p2.p in
            mk_parser_cont p1 p2 ~p:Simple.(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
        | _ ->
            mk_parser_cont p1 p2 ~p:Simple.(p1.p << p2.p) ~typ:Parser

    let (<|>) p1 p2 =
        { p = Simple.(p1.p <|> p2.p)
        ; info =
            begin match p1.info, p2.info with
            | Unknown, _ -> Unknown
            | _, Unknown -> Unknown
            | Empty, Empty -> Empty
            | Empty, Consume p2 -> Consume {p2 with empty = true}
            | Consume p1, Empty -> Consume {p1 with empty = true}
            | Consume p1, Consume p2 ->
                Consume
                { empty = p1.empty || p2.empty
                ; first = Tset.union p1.first p2.first
                }
            end
        ; typ = Parser
        ; id = Id.get()
        }

    let (<*>) pf pv =
        match pf.typ with
        | Return f ->
            let a = pv.p in
            mk_parser_cont pf pv ~p:Simple.(lift f a) ~typ:(Lift { f; a })
        | Value { v = f; p } ->
            let a = Simple.(p >> pv.p) in
            mk_parser_cont pf pv ~p:Simple.(lift f a) ~typ:(Lift { f; a })
        | Lift { f; a } ->
            mk_parser_cont pf pv ~p:Simple.(lift2 f a pv.p) ~typ:(Lift2 { f; a; b = pv.p })
        | Lift2 { f; a; b } ->
            mk_parser_cont pf pv ~p:Simple.(lift3 f a b pv.p) ~typ:(Lift3 { f; a; b; c = pv.p })
        | Lift3 { f; a; b; c } ->
            mk_parser_cont pf pv ~p:Simple.(lift4 f a b c pv.p) ~typ:Parser
        | _ ->
            mk_parser_cont pf pv ~p:Simple.(pf.p <*> pv.p) ~typ:Parser

    let (>>$) pp v =
        let p = Simple.(pp.p >>$ v) in
        { pp with p
        ; typ =
            Value
            { v; p }
        ; id = Id.get()
        }

    let (>>|) p f =
        { p with p = Simple.(p.p >>| f)
        ; typ = Parser
        ; id = Id.get()
        }

    let fix f =
        let rec p = lazy (f r)
        and r =
            { p = { run = fun i -> (Lazy.force p).p.run i }
            ; info = Unknown
            ; typ = Parser
            ; id = Id.get()
            }
        in
        Lazy.force p

    let fix_poly f =
        let rec res = lazy (f getter)
        and getter =
            { get = fun get ->
                { p = { run = fun i -> (get @@ Lazy.force res).p.run i }
                ; info = Unknown
                ; typ = Parser
                ; id = Id.get()
                }
            }
        in
        Lazy.force res

    let return x =
        { p = Simple.(return x)
        ; info = Empty
        ; typ = Return x
        ; id = Id.get()
        }

    let fail =
        { p = Simple.fail
        ; info = Unknown
        ; typ = Parser
        ; id = Id.fail
        }

    let choice ps =
        let rec loop =
            function
            | [] -> failwith "check usage"
            | [x] -> x
            | x :: xs -> x <|> loop xs
        in
        loop ps

    let pos: Lexing.position t =
        { p =
            { run = fun state _fail succ ->
                succ state (next_token_start state)
            }
        ; info = Empty
        ; typ = Parser
        ; id = Id.get()
        }
    let pos_end: Lexing.position t =
        { p =
            { run = fun state _fail succ ->
                succ state (prev_token_end state)
            }
        ; info = Empty
        ; typ = Parser
        ; id = Id.get()
        }

    let exec f =
        { p = Simple.exec f
        ; info = Empty
        ; typ = Parser
        ; id = Id.get()
        }

    let failed p =
        { p =
            { run = fun state fail succ ->
                let succ1 _ _ = fail state in
                let fail1 _ = succ state () in

                p.p.run state fail1 succ1
            }
        ; info = Empty
        ; typ = Parser
        ; id = Id.get()
        }

    let opt p =
        { p = Simple.((p.p >>| fun x -> Some x) <|> return None )
        ; info =
            begin match p.info with
            | Unknown -> Unknown
            | Empty -> Empty
            | Consume p -> Consume {p with empty = true}
            end
        ; typ = Parser
        ; id = Id.get()
        }

    let seq ?(n=0) ?sep ?(trail = false) p =
        let pi, si, tail =
            match sep with
            | None -> p.info, Empty, Simple.many p.p
            | Some sep ->
                let tail =
                    let open Simple in
                    let t = many (sep.p >> p.p) in
                    if trail then t << ((sep.p >>| fun _ -> ()) <|> return ()) else t
                in

                p.info, sep.info, tail
        in

        let info =
            match pi, si with
            | Unknown, _ -> Unknown
            | _, Unknown -> Unknown
            | Empty, Empty -> failwith "Endless sequence"
            | Empty, Consume {empty = true; _} -> failwith "Endless sequence"
            | Empty, Consume p2 ->
                Consume
                { p2 with empty = n < 2 }
            | Consume {empty = true; _}, Empty -> failwith "Endless sequence"
            | Consume p1, Empty ->
                Consume
                { p1 with empty = n = 0 }
            | Consume {empty = true; _}, Consume {empty = true; _} -> failwith "Endless sequence"
            | Consume p1, Consume p2 ->
                if p1.empty then
                    Consume
                    { empty = p1.empty && n = 1
                    ; first = Tset.union p1.first p2.first
                    }
                else
                    Consume
                    { p1 with empty = n = 0 }
        in

        { p =
            begin
                let open Simple in
                let list =
                    lift2 (fun first tail -> first::tail)
                    p.p tail
                    <|>
                    return []
                in
                list >>= fun list ->
                if List.length list < n then
                    fail
                else
                    return list
            end
        ; info
        ; typ = Parser
        ; id = Id.get()
        }

    let trail = true

    let make_location loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
    let loc_comb loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end

    let (+) = (<*>)
    let (-) = (<<)

    let mapping = return

    let loc p =
        mapping begin fun p1 x p2 -> Location.mkloc x @@ make_location p1 p2 end
        <*> pos <*> p <*> pos_end

    let loc_of p =
        mapping make_location
        <*> pos << p <*> pos_end

    let t2 =
        let f = fun a b -> a, b in
        { p = { run = fun state _fail succ -> succ state f }
        ; info = Empty
        ; typ = Return f
        ; id = Id.t2
        }
    let t3 =
        let f = fun a b c -> a, b, c in
        { p = { run = fun state _fail succ -> succ state f }
        ; info = Empty
        ; typ = Return f
        ; id = Id.t3
        }
    let t4 =
        let f = fun a b c d -> a, b, c, d in
        { p = { run = fun state _fail succ -> succ state f }
        ; info = Empty
        ; typ = Return f
        ; id = Id.t4
        }
    let cons =
        let f = fun x xs -> x :: xs in
        { p = { run = fun state _fail succ -> succ state f }
        ; info = Empty
        ; typ = Return f
        ; id = Id.cons
        }

    let eof =
        let p =
            { run = fun state fail succ ->
                if state.pos = Array.length state.tokens then
                    succ state ()
                else
                    fail state
            }
        in
        { p
        ; info = Empty
        ; typ = Value { v = (); p }
        ; id = Id.eof
        }

    let tkn_helper ~f tag =
        let p =
            { run = fun state fail succ ->
                if state.pos = Array.length state.tokens then
                    fail state
                else
                    let t = state.tokens.(state.pos) in
                    if Tokenizer.Tag.equal t.tag tag then
                        match f t with
                        | Some x ->
                            succ { state with pos = Int.succ state.pos } x
                        | None -> fail state
                    else
                        fail state
            }
        in

        { p
        ; info = Consume
            { empty = false
            ; first = Tset.singleton (tag2int tag)
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let tkn tag =
        tkn_helper tag
            ~f:begin fun t ->
                match t.payload with
                    | Some payload -> Some (t.tag, Caml.Obj.obj payload)
                    | _ -> None
            end
    let tkn_ tag = tkn_helper tag ~f:begin fun _ -> Some () end
    let tkn_tag tag = tkn_helper tag ~f:begin fun _ -> Some tag end
    let tkn_payload tag =
        tkn_helper tag
            ~f:begin fun t ->
                match t.payload with
                | Some payload -> Some (Caml.Obj.obj payload)
                | _ -> None
            end

    let peek p =
        { p =
            { run = fun state fail succ ->
                let succ _ v = succ state v in
                p.p.run state fail succ
            }
        ; info = Unknown
        ; typ = Parser
        ; id = Id.get()
        }

    let fold_left_0_n ~f nil p =
        let info = (nil >> seq p).info in

        { p =
            begin
                let open Simple in
                let rec loop acc =
                    (p.p >>= fun x -> loop @@ f acc x)
                    <|>
                    return acc
                in
                nil.p >>= loop
            end
        ; info
        ; typ = Parser
        ; id = Id.get()
        }

    let fold_left_0_1 ~f nil p =
            (mapping f <*> nil <*> p)
        <|> nil

    let fold_left_cont_0_n nil cont =
        let tail = fix @@ fun tail ->
            mapping begin fun cont tail ->
                fun prev ->
                    match tail with
                    | None -> cont prev
                    | Some tail -> tail (cont prev)
            end
            +cont +opt(tail)
        in

        mapping begin fun x tail ->
            match tail with
            | None -> x
            | Some tail -> tail x
        end
        +nil +opt(tail)

    let fold_left_cont_0_1 nil cont =
        mapping begin fun x cont ->
            match cont with
            | None -> x
            | Some cont -> cont x
        end
        +nil +opt(cont)

    let (&&) v f =
        mapping (fun v f -> f v) <*> v <*> f

    let (&) = (@@)

    let with_loc p =
        mapping begin fun p1 f p2 -> f (make_location p1 p2) end
        +pos +p +pos

    let peek_first expected =
        let arr = Array.create ~len:256 Simple.fail in
        let first = ref Tset.empty in

        let rec loop =
            function
            | [] -> ()
            | p::xs ->
                let p_first =
                    match p.info with
                    | Consume {empty=false; first} -> first
                    | _ -> failwith (Printf.sprintf "%d" @@ List.length xs)
                in

                first := Tset.union !first p_first;

                loop xs;

                p_first |> Tset.iter_code
                    begin fun c ->
                        arr.(c) <- Simple.(p.p <|> arr.(c))
                    end
        in
        loop expected;

        { p =
            { run = fun state fail succ ->
                if state.pos = Array.length state.tokens then
                    fail state
                else
                    let p = arr.(tag2int state.tokens.(state.pos).tag) in
                    p.run state fail succ
            }
        ; info =
            Consume
            { empty = false
            ; first = !first
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let memoid2id = Hashtbl.create (module Int)
    let id2memoid: (int, int) Hashtbl.t = Hashtbl.create (module Int)

    let memo p =
        let id = Id.get() in

        let _ = Hashtbl.add memoid2id ~key:id ~data:p.id in
        let _ = Hashtbl.add id2memoid ~data:id ~key:p.id in

        { p with
            id;
            p = { run = fun state fail succ ->
                let table = Hashtbl.find_or_add state.memo_tables id ~default:(fun _ -> Hashtbl.create (module Int)) in
                let key = state.pos in

                match Hashtbl.find table key with
                | Some t ->
                    let (t, state') = Caml.Obj.obj t in
                    let state =
                        { state' with
                            log_parts = state.log :: state.log_parts;
                        }
                    in

                    begin match t with
                    | Some v -> succ state v
                    | None -> fail state
                    end
                | None ->
                    let set_data state x =
                        Hashtbl.add_exn table ~key ~data:(Caml.Obj.repr (x, state))
                    in

                    let succ state v =
                        set_data state @@ Some v;
                        succ state v
                    in
                    let fail state =
                        set_data state None;
                        fail state
                    in

                    let state =
                        { state with log = []; log_parts = state.log :: state.log_parts }
                    in

                    p.p.run state fail succ
            };
        }

    let name2id = Hashtbl.create (module String)
    let id2name = Hashtbl.create (module Int)

    let named (name: string) p =
        let id = Id.get() in
        Hashtbl.add_exn name2id ~key:name ~data:id;
        Hashtbl.add_exn id2name ~key:id ~data:name;
        {p with id}

    let rec name_of_id id =
        match Hashtbl.find memoid2id id with
        | Some id ->
            let name = name_of_id id in
            name ^ "*"
        | None ->
            begin match Hashtbl.find id2name id with
            | Some x -> x
            | None -> "_unnamed_"
            end

    let name_of p = name_of_id p.id

    let fold_log init f =
        { p =
            { run = fun state _ succ ->
                let init = List.fold_left ~init ~f state.log in

                let res =
                    List.fold_left state.log_parts ~init
                        ~f:(fun init -> List.fold_left ~init ~f)
                in

                succ state res
            }
        ; info = Empty
        ; typ = Parser
        ; id = Id.get()
        }

    let scan lexbuf =
        let rec loop tokens =
            Tokenizer.scan_space lexbuf;
            let loc_start = lexbuf.lex_curr_p in
            (* Caml.print_endline (Printf.sprintf "%d:%d" loc_start.pos_lnum Int.(loc_start.pos_cnum - loc_start.pos_bol + 1)); *)
            match Tokenizer.scan_token lexbuf with
            | Eof ->
                (tokens, loc_start)
            | Tag tag ->
                let loc_end = lexbuf.lex_curr_p in
                loop ({ loc_start; tag; payload = None; loc_end } :: tokens)
            | WithPayload { tag; payload } ->
                let loc_end = lexbuf.lex_curr_p in
                loop ({ loc_start; tag; payload = Some payload; loc_end } :: tokens)
        in
        let tokens, pos_eof = loop [] in
        Array.of_list_rev tokens, pos_eof

    let parse_string p ?(filename = "none") text =
        let lexbuf = Lexing.from_string text in
        let zero_pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
        lexbuf.lex_start_p <- lexbuf.lex_curr_p;
        let tokens, pos_eof = scan lexbuf in

        let succ state v =
            if state.pos = Array.length tokens then Some v
            else None
        in
        let fail _state = None in

        p.p.run
            {
                file_start = zero_pos;
                file_end = pos_eof;
                tokens;
                pos = 0;
                log = [];
                log_parts = [];
                memo_tables = Hashtbl.create (module Int)
            }
            fail succ

    let id p = p.id
    let simple p = p.p

    let run p =
        let info =
            match p.info with
            | Consume { empty = false; _ } as x -> x
            | _ -> Unknown
        in

        { p =
            { run = fun state fail succ ->
                let succ' state' v = v.run state' fail succ in
                p.p.run state fail succ'
            }
        ; info
        ; typ = Parser
        ; id = Id.get()
        }

    let print_info p =
        Caml.print_endline @@
        match p.info with
        | Unknown -> "unknown"
        | Empty -> "empty"
        | Consume { empty = true; _ } -> "consume+empty"
        | _ -> "consume"
end
