open Core_kernel

module Make(Conf: Pc.CONF): Sigs.POS with type s = Conf.Log.elem = struct
    type s = Conf.Log.elem

    module State = struct
        module Line = struct
            type t = {
                no    : int;
                start : int;
            }

            let default = { no = 1; start = 0 }
        end

        module Info = struct
            type t = {
                default_position : Lexing.position;
            }
        end

        type t = {
            line            : Line.t;
            info            : Info.t;
            log             : s list;
            log_parts       : s list list;
            memo_tables     : (int, (int, Obj.t) Hashtbl.t) Hashtbl.t
        }

        let state = ref None
        let get () =
            match !state with
            | Some x -> x
            | None -> failwith ""
        let set s = state := Some s
        let map f = set @@ f @@ get ()

        let init file_name =
            Angstrom.Expose.Parser.
            { run = fun input pos more _fail succ ->
                let default_position = Lexing.{ pos_fname = file_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
                state := Some
                    { line = Line.default
                    ; info =
                        { default_position }
                    ; log = []
                    ; log_parts = []
                    ; memo_tables = Hashtbl.create (module Int)
                    };
                succ input pos more ()
            }

        let trace_depth = ref 0
        let trace_entries = ref []
    end

    module Id = struct
        let next = ref 0
        let get () = let x = !next in next := x + 1; x

        let fail = get ()
        let t2 = get ()
        let t3 = get ()
        let t4 = get ()
        let cons = get ()
        let trace_entries = get ()
    end

    module Basic = struct
        type log_elem = s

        module Simple = struct
            include Angstrom

            let fail =
                { Angstrom.Expose.Parser.run = fun input pos more fail _succ ->
                    fail input pos more [] ""
                }

            let (>>) = ( *> )
            let (<<) = ( <* )

            let (>>$) p v =
                Angstrom.Expose.Parser.
                { run = fun input pos more fail succ ->
                    let succ input pos more _ = succ input pos more v in
                    p.run input pos more fail succ
                }

            let exec f =
                { Angstrom.Expose.Parser.run = fun input pos more _fail succ ->
                    succ input pos more (f ())
                }

            let (<|>) p q =
                { Angstrom.Expose.Parser.run = fun input pos more fail succ ->
                    let s = State.get () in
                    let fail' input' _pos' more' _marks _msg =
                        State.set s;
                        q.Angstrom.Expose.Parser.run input' pos more' fail succ
                    in
                    p.Angstrom.Expose.Parser.run input pos more fail' succ
                }

            let many p =
                fix (fun m ->
                (lift2 (fun x xs -> x::xs) p m) <|> return [])

            let log x =
                { Angstrom.Expose.Parser.run = fun input pos more _fail succ ->
                    State.map (fun s -> { s with log = x :: s.log });
                    succ input pos more ();
                }

            let log_many log =
                { Angstrom.Expose.Parser.run = fun input pos more _fail succ ->
                    State.map (fun s -> {s with log; log_parts = s.log :: s.log_parts});
                    succ input pos more ();
                }
        end

        include Parser

        type 'b getter = { get: 'a. ('b -> 'a t) -> 'a t }

        let print_info p =
            Caml.print_endline @@
            match p.info with
            | Unknown -> "unknown"
            | Empty -> "empty"
            | Consume { empty = true; _ } -> "consume+empty"
            | _ -> "consume"

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
                        | Consume p2 -> Consume { p2 with first = Charset.union first p2.first }
                    else
                    p1
            in

            { p; info; typ; id = Id.get() }

        let (>>) p1 p2 =
            let open Simple in
            match p1.typ, p2.typ with
            | Return _, _ -> p2
            | _, Return v ->
                mk_parser_cont p1 p2 ~p:(p1.p >>| fun _ -> v) ~typ:(Value { v; p = p1.p })
            | _, Value { v; _ } ->
                let p = (p1.p >> p2.p) in
                mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
            | _, Lift { f; a } ->
                let a = (p1.p >> a) in
                mk_parser_cont p1 p2 ~p:(lift f a) ~typ:(Lift { f; a })
            | _, Lift2 { f; a; b } ->
                let a = (p1.p >> a) in
                mk_parser_cont p1 p2 ~p:(lift2 f a b) ~typ:(Lift2 { f; a; b })
            | _, Lift3 { f; a; b; c } ->
                let a = (p1.p >> a) in
                mk_parser_cont p1 p2 ~p:(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
            | _ ->
                mk_parser_cont p1 p2 ~p:(p1.p >> p2.p) ~typ:Parser
        let (<<) p1 p2 =
            let open Simple in
            match p1.typ, p2.typ with
            | _, Return _ -> p1
            | Return v, _ ->
                mk_parser_cont p1 p2 ~p:(p2.p >>| fun _ -> v) ~typ:(Value { v; p = p2.p })
            | Value { v; _ }, _ ->
                let p = (p1.p << p2.p) in
                mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
            | Lift { f; a }, _ ->
                let a = (<<) a p2.p in
                mk_parser_cont p1 p2 ~p:(lift f a) ~typ:(Lift { f; a })
            | Lift2 { f; a; b }, _ ->
                let b = (<<) b p2.p in
                mk_parser_cont p1 p2 ~p:(lift2 f a b) ~typ:(Lift2 { f; a; b })
            | Lift3 { f; a; b; c }, _ ->
                let c = (<<) c p2.p in
                mk_parser_cont p1 p2 ~p:(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
            | _ ->
                mk_parser_cont p1 p2 ~p:(p1.p << p2.p) ~typ:Parser
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
                    ; first = Charset.union p1.first p2.first
                    }
                end
            ; typ = Parser
            ; id = Id.get()
            }

        let (<*>) pf pv =
            let open Simple in
            match pf.typ with
            | Return f ->
                let a = pv.p in
                mk_parser_cont pf pv ~p:(lift f a) ~typ:(Lift { f; a })
            | Value { v = f; p } ->
                let a = (p >> pv.p) in
                mk_parser_cont pf pv ~p:(lift f a) ~typ:(Lift { f; a })
            | Lift { f; a } ->
                mk_parser_cont pf pv ~p:(lift2 f a pv.p) ~typ:(Lift2 { f; a; b = pv.p })
            | Lift2 { f; a; b } ->
                mk_parser_cont pf pv ~p:(lift3 f a b pv.p) ~typ:(Lift3 { f; a; b; c = pv.p })
            | Lift3 { f; a; b; c } ->
                mk_parser_cont pf pv ~p:(lift4 f a b c pv.p) ~typ:Parser
            | _ ->
                mk_parser_cont pf pv ~p:(pf.p <*> pv.p) ~typ:Parser
        let (>>$) pp v =
            let p = Simple.(pp.p >>$ v) in
            { pp with p
            ; typ =
                Value
                { v; p }
            ; id = Id.get()
            }

        let run p =
            let info =
                match p.info with
                | Consume { empty = false; _ } as x -> x
                | _ -> Unknown
            in

            { p =
                { run = fun input pos more fail succ ->
                    let succ' input' pos' more' v = v.Angstrom.Expose.Parser.run input' pos' more' fail succ in
                    p.p.run input pos more fail succ'
                }
            ; info
            ; typ = Parser
            ; id = Id.get()
            }

        let (>>|) p f =
            { p with p = Simple.(p.p >>| f)
            ; typ = Parser
            ; id = Id.get()
            }

        let make_position pos =
            let open State in
            let {line; info; _} = State.get () in
            let (pos_lnum, pos_bol, pos_cnum) =
                (line.no, line.start, pos)
            in

            let open Lexing in
            { info.default_position with
                pos_lnum;
                pos_bol;
                pos_cnum;
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

        let pos =
            { p =
                { run = fun i p m _ s ->
                    s i p m (make_position p)
                }
            ; info = Empty
            ; typ = Parser
            ; id = Id.get()
            }
        let pos_end = pos

        let exec f =
            { p = Simple.exec f
            ; info = Empty
            ; typ = Parser
            ; id = Id.get()
            }

        let failed p =
            { p =
                { Angstrom.Expose.Parser.run = fun input pos more fail succ ->
                    let s = State.get () in
                    let succ' input' _ more' _ =
                        State.set s;
                        fail input' pos more' [] ""
                    in
                    let fail' input' _ more' _ _ =
                        State.set s;
                        succ input' pos more' ()
                    in

                    p.p.run input pos more fail' succ'
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
                        ; first = Charset.union p1.first p2.first
                        }
                    else
                        Consume
                        { p1 with empty = n = 0 }
            in

            { p =
                begin
                    let open Simple in
                    let list =
                        map2 p.p tail
                        ~f:begin fun first tail -> first::tail end
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

        let t2 =
            let f = fun a b -> a, b in
            { p = { run = fun input pos more _fail succ -> succ input pos more f }
            ; info = Empty
            ; typ = Return f
            ; id = Id.t2
            }
        let t3 =
            let f = fun a b c -> a, b, c in
            { p = { run = fun input pos more _fail succ -> succ input pos more f }
            ; info = Empty
            ; typ = Return f
            ; id = Id.t3
            }
        let t4 =
            let f = fun a b c d -> a, b, c, d in
            { p = { run = fun input pos more _fail succ -> succ input pos more f }
            ; info = Empty
            ; typ = Return f
            ; id = Id.t4
            }
        let cons =
            let f = fun x xs -> x :: xs in
            { p = { run = fun input pos more _fail succ -> succ input pos more f }
            ; info = Empty
            ; typ = Return f
            ; id = Id.cons
            }

        let trace_entries =
            { p =
                { run = fun input pos more _fail succ ->
                    succ input pos more !(State.trace_entries)
                }
            ; info = Empty
            ; typ = Parser
            ; id = Id.trace_entries
            }

        let touch p = { p with id = Id.get () }
        let modify ~simple p =
            match p.typ with
            | Parser ->
                { p = simple
                ; id = Id.get ()
                ; typ = Parser
                ; info = p.info
                }
            | _ -> failwith "check usage"
        let not_empty p =
            match p.info with
            | Consume { empty = false; _ } -> true
            | _ -> false

        let peek_first expected =
            let arr = Array.create ~len:256 Simple.fail in
            let first = ref Charset.empty in

            let rec loop =
                function
                | [] -> ()
                | p::xs ->
                    let p_first =
                        match p.info with
                        | Consume {empty=false; first} -> first
                        | _ -> failwith ""
                    in

                    first := Charset.union !first p_first;

                    loop xs;

                    p_first |> Charset.iter_code
                        begin fun c ->
                            arr.(c) <- Simple.(p.p <|> arr.(c))
                        end
            in
            loop expected;

            { p = Simple.(peek_char_fail >>= fun c -> arr.(Char.to_int c))
            ; info =
                Consume
                { empty = false
                ; first = !first
                }
            ; typ = Parser
            ; id = Id.get()
            }

        let memo p =
            let id = Id.get() in
            { p with
                id;
                p = { run = fun input pos more fail succ ->
                    let s = State.get () in

                    let table = Hashtbl.find_or_add s.memo_tables id ~default:(fun _ -> Hashtbl.create (module Int)) in

                    match Hashtbl.find table pos with
                    | Some t ->
                        let  (x, log, line) = Obj.obj t in
                        State.set { s with
                            line; log;
                            log_parts = s.log :: s.log_parts;
                        };

                        begin match x with
                        | Ok (input', pos', more', v) -> succ input' pos' more' v
                        | Error (input', pos', more', marks', msg') -> fail input' pos' more' marks' msg'
                        end
                    | None ->
                        let set_data x =
                            let s = State.get () in
                            Hashtbl.add_exn table ~key:pos ~data:(Obj.repr (x, s.log, s.line))
                        in

                        let succ' input' pos' more' v =
                            set_data @@ Ok (input', pos', more', v);
                            succ input' pos' more' v
                        in
                        let fail' input' pos' more' marks' msg' =
                            set_data @@ Error (input', pos', more', marks', msg');
                            fail input' pos' more' marks' msg'
                        in

                        State.set { s with
                            log = [];
                            log_parts = s.log :: s.log_parts;
                        };
                        p.p.run input pos more fail' succ'
                };
            }

        let traced p =
            let id = Id.get () in
            { p with id; p =
                { Angstrom.Expose.Parser.run = fun input pos more fail succ ->
                    let enter_time = Sys.time () in
                    let enter_pos = make_position pos in
                    let depth = State.trace_depth in
                    let entries = State.trace_entries in
                    let d = !depth in
                    depth := d + 1;

                    let finalize pos succeded =
                        depth := d;

                        let exit_pos = make_position pos in
                        let exit_time = Sys.time () in

                        let entry =
                            Exec_info.{
                                id;
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

        let fold_log init f =
            { p =
                { run = fun i p m _ succ ->
                    let s = State.get () in

                    let init = List.fold_left ~init ~f s.log in

                    let res =
                        List.fold_left s.log_parts ~init
                            ~f:(fun init -> List.fold_left ~init ~f)
                    in

                    succ i p m res
                }
            ; info = Empty
            ; typ = Parser
            ; id = Id.get()
            }

        let id p = p.id
        let simple p = p.p

        let eof =
            { p = Angstrom.end_of_input
            ; id = Id.get ()
            ; info = Unknown
            ; typ = Parser
            }


        let parse_string p ?(filename = "none") text =
            let open Simple in
            match parse_string ~consume:All (State.init filename >> p.p) text with
            | Ok x -> Some x
            | _ -> None

        let parse_string_with_trace p ?(filename = "none") text =
            let p = t2 <*> opt (p << eof) <*> trace_entries in
            let open Simple in
            match parse_string ~consume:Prefix (State.init filename >> p.p) text with
            | Ok (x, es) -> x, List.rev es
            | _ -> failwith "unreachable"
    end

    include Pc.Make(Basic)(Conf)
    open Parser

    let char c =
        let p = Angstrom.char c in
        { p
        ; info = Consume
            { empty = false
            ; first = Charset.singleton c
            }
        ; typ = Value { v = c; p }
        ; id = Id.get()
        }

    let string s =
        if String.(s = "") then
            return ""
        else
            let p = Angstrom.string s in
            { p
            ; info = Consume
                { empty = false
                ; first = Charset.singleton s.[0]
                }
            ; typ = Value { v = s; p }
            ; id = Id.get()
            }
    let advance_line =
        let p =
            { Angstrom.Expose.Parser.run = fun i p m _ s ->
                State.map (fun s -> {s with line = {no = Int.(s.line.no + 1); start = p}});
                s i p m ()
            }
        in
        { p
        ; info = Empty
        ; typ = Value { v = (); p }
        ; id = Id.get()
        }

    let new_line =
            (string "\n" << advance_line)
        <|> (string "\r\n" << advance_line)

    let whitespace =
        { Angstrom.Expose.Parser.run = fun input pos more fail succ ->
            let (+) = Int.(+) in
            let (&&) = Base.(&&) in
            let module Input = Angstrom.Expose.Input in
            let len = Input.length input in
            let char = Input.unsafe_get_char input in

            let succ lines start pos1 =
                if lines <> 0 then State.map (fun s -> {s with line = {no = s.line.no + lines; start}});
                succ input pos1 more ()
            in
            let rec loop lines start pos =
                match pos < len with
                | true ->
                    begin match char pos with
                    | ' ' -> loop lines start @@ pos + 1
                    | '\n' -> loop (lines + 1) (pos + 1) @@ pos + 1
                    | '\t' -> loop lines start @@ pos + 1
                    | '\r' ->
                        begin match pos + 1 < len && Char.(char (pos + 1) = '\n') with
                        | true -> loop (lines + 1) (pos + 2) @@ pos + 2
                        | false -> let _ = failwith "here" in fail input pos more [] "ws_pos"
                        end
                    | _ -> succ lines start pos
                    end
                | false -> succ lines start pos
            in
            loop 0 0 pos
        }
    let whitespace =
        { p = whitespace
        ; info = Consume
            { empty = true
            ; first = Charset.of_list [' '; '\n'; '\t'; '\r']
            }
        ; typ = Value { v = (); p = whitespace }
        ; id = Id.get()
        }

    let with_literal p =
        { p with p =
            begin
                let open Simple in
                return () >>= fun _ ->
                let res = ref None in
                Angstrom.consumed (p.p >>| fun v -> res := Some v) >>| fun str ->
                let [@warning "-8"] Some v = !res in
                (v, str)
            end
        ; typ = Parser
        ; id = Id.get()
        }

    let consumed p =
        { p with p = Angstrom.consumed p.p
        ; typ = Parser
        ; id = Id.get()
        }

    let s =
        Fix.Memoize.String.memoize @@ fun x ->
        named ("\'" ^ x ^ "\'") begin
            match String.length x with
            | 0 -> return ()
            | 1 -> char x.[0] >>$ ()
            | _ -> string x >>$ ()
        end
    let advance i =
        if i = 0 then
            { p = Angstrom.(advance 0)
            ; info = Empty
            ; typ = Return ()
            ; id = Id.get()
            }
        else
            let p = Angstrom.(advance i) in
            { p
            ; info = Consume
                { empty = false
                ; first = Charset.full
                }
            ; typ = Value { v = (); p }
            ; id = Id.get()
            }

    let mk_first_test f =
        let rec loop set =
            function
            | 256 -> set
            | i ->
                let c = Char.of_int_exn i in
                let newset =
                    if f c then Charset.add set c
                    else set
                in
                loop newset Int.(i + 1)
        in
        loop Charset.empty 0

    let take_while f =
        { p = Angstrom.take_while f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let take_while1 f =
        { p = Angstrom.take_while1 f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let skip f =
        { p = Angstrom.skip f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let skip_while f =
        { p = Angstrom.skip_while f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let satisfy f =
        { p = Angstrom.satisfy f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let any_char =
        { p = Angstrom.any_char
        ; info = Consume
            { empty = false
            ; first = Charset.full
            }
        ; typ = Parser
        ; id = Id.get()
        }

    let peek_char =
        { p = Angstrom.peek_char
        ; info = Unknown
        ; typ = Parser
        ; id = Id.get()
        }
end
