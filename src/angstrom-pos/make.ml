
module Make(T: sig type s end) = struct
    module Parser = Sigs.MkParser(struct type s = T.s end)
    open Parser

    let (!!) = Lazy.force

    module Angstrom = struct
        module Angstrom_ = Angstrom_mod.Angstrom.Make(struct type s = Parser.Angstrom.s end)
        include Angstrom_

        let (>>$) p v =
            Angstrom_mod.Parser.
            { run = fun input pos state more fail succ ->
                let succ input pos state more _ = succ input pos state more v in
                p.run input pos state more fail succ
            }

        let exec f =
            Angstrom_mod.Parser.
            { run = fun input pos state more _fail succ ->
                succ input pos state more (f ())
            }
    end


    type 'b getter = { get: 'a. ('b -> 'a Parser.t) -> 'a Parser.t }

    let print_info p =
        print_endline @@
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

        { p; info; typ }

    let (>>) p1 p2 =
        match p1.typ, p2.typ with
        | Return _, _ -> p2
        | _, Return v ->
            mk_parser_cont p1 p2 ~p:Angstrom.(p1.p >>| fun _ -> v) ~typ:(Value { v; p = p1.p })
        | _, Value { v; _ } ->
            let p = Angstrom.(p1.p >> p2.p) in
            mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
        | _, Lift { f; a } ->
            let a = Angstrom.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift f a) ~typ:(Lift { f; a })
        | _, Lift2 { f; a; b } ->
            let a = Angstrom.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift2 f a b) ~typ:(Lift2 { f; a; b })
        | _, Lift3 { f; a; b; c } ->
            let a = Angstrom.(p1.p >> a) in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
        | _ ->
            mk_parser_cont p1 p2 ~p:Angstrom.(p1.p >> p2.p) ~typ:Parser
    let (<<) p1 p2 =
        match p1.typ, p2.typ with
        | _, Return _ -> p1
        | Return v, _ ->
            mk_parser_cont p1 p2 ~p:Angstrom.(p2.p >>| fun _ -> v) ~typ:(Value { v; p = p2.p })
        | Value { v; _ }, _ ->
            let p = Angstrom.(p1.p << p2.p) in
            mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
        | Lift { f; a }, _ ->
            let a = Angstrom.(<<) a p2.p in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift f a) ~typ:(Lift { f; a })
        | Lift2 { f; a; b }, _ ->
            let b = Angstrom.(<<) b p2.p in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift2 f a b) ~typ:(Lift2 { f; a; b })
        | Lift3 { f; a; b; c }, _ ->
            let c = Angstrom.(<<) c p2.p in
            mk_parser_cont p1 p2 ~p:Angstrom.(lift3 f a b c) ~typ:(Lift3 { f; a; b; c })
        | _ ->
            mk_parser_cont p1 p2 ~p:Angstrom.(p1.p << p2.p) ~typ:Parser
    let (<|>) p1 p2 =
        { p = Angstrom.(p1.p <|> p2.p)
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
        }
    let (<*>) pf pv =
        match pf.typ with
        | Return f ->
            let a = pv.p in
            mk_parser_cont pf pv ~p:Angstrom.(lift f a) ~typ:(Lift { f; a })
        | Value { v = f; p } ->
            let a = Angstrom.(p >> pv.p) in
            mk_parser_cont pf pv ~p:Angstrom.(lift f a) ~typ:(Lift { f; a })
        | Lift { f; a } ->
            mk_parser_cont pf pv ~p:Angstrom.(lift2 f a pv.p) ~typ:(Lift2 { f; a; b = pv.p })
        | Lift2 { f; a; b } ->
            mk_parser_cont pf pv ~p:Angstrom.(lift3 f a b pv.p) ~typ:(Lift3 { f; a; b; c = pv.p })
        | Lift3 { f; a; b; c } ->
            mk_parser_cont pf pv ~p:Angstrom.(lift4 f a b c pv.p) ~typ:Parser
        | _ ->
            mk_parser_cont pf pv ~p:Angstrom.(pf.p <*> pv.p) ~typ:Parser
    let (>>$) pp v =
        let p = Angstrom.(pp.p >>$ v) in
        { pp with p
        ; typ =
            Value
            { v; p }
        }
    let (>>=) p f =
        let info =
            match p.info with
            | Consume { empty = false; _ } as x -> x
            | _ -> Unknown
        in

        { p = Angstrom.(p.p >>= f)
        ; info
        ; typ = Parser
        }

    let (>>|) p f =
        { p with p = Angstrom.(p.p >>| f)
        ; typ = Parser
        }

    let parse_string p state ?(filename = "none") =
        let open Angstrom in
        parse_string ~consume:All p.p (State.make filename state)

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

    let fix f =
        let rec p = lazy (f r)
        and r =
            { p = { run = fun i -> (Lazy.force p).p.run i }
            ; info = Unknown
            ; typ = Parser
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
                }
            }
        in
        Lazy.force res

    let return x =
        { p = Angstrom.(return x)
        ; info = Empty
        ; typ = Return x
        }
    let advance i =
        if i = 0 then
            { p = Angstrom.(advance 0)
            ; info = Empty
            ; typ = Return ()
            }
        else
            let p = Angstrom.(advance i) in
            { p
            ; info = Consume
                { empty = false
                ; first = Charset.full
                }
            ; typ = Value { v = (); p }
            }

    let fail =
        { p =
            { run = fun input pos state more fail _succ ->
                fail input pos state more [] ""
            }
        ; info = Unknown
        ; typ = Parser
        }

    let any_char =
        { p = Angstrom.any_char
        ; info = Consume
            { empty = false
            ; first = Charset.full
            }
        ; typ = Parser
        }

    let peek_char =
        { p = Angstrom.peek_char
        ; info = Unknown
        ; typ = Parser
        }

    let char c =
        let p = Angstrom.char c in
        { p
        ; info = Consume
            { empty = false
            ; first = Charset.singleton c
            }
        ; typ = Value { v = c; p }
        }

    let string s =
        if s = "" then
            return ""
        else
            let p = Angstrom.string s in
            { p
            ; info = Consume
                { empty = false
                ; first = Charset.singleton s.[0]
                }
            ; typ = Value { v = s; p }
            }

    let mk_first_test f =
        let rec loop set =
            function
            | 256 -> set
            | i ->
                let c = Char.chr i in
                let newset =
                    if f c then Charset.add set c
                    else set
                in
                loop newset @@ i + 1
        in
        loop Charset.empty 0

    let take_while f =
        { p = Angstrom.take_while f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        }

    let take_while1 f =
        { p = Angstrom.take_while1 f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        }

    let skip f =
        { p = Angstrom.skip f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        }

    let skip_while f =
        { p = Angstrom.skip_while f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        }

    let satisfy f =
        { p = Angstrom.satisfy f
        ; info = Consume
            { empty = false
            ; first = mk_first_test f
            }
        ; typ = Parser
        }

    let pos =
        { p = Angstrom.with_state make_position
        ; info = Empty
        ; typ = Parser
        }
    let advance_line =
        let p = Angstrom.state_map (fun pos s -> State.{s with line = { no = s.line.no + 1; start = pos }}) in
        { p
        ; info = Empty
        ; typ = Value { v = (); p }
        }

    let new_line =
            (string "\n" << advance_line)
        <|> (string "\r\n" << advance_line)


    let whitespace: unit Angstrom.Parser.t =
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
    let whitespace =
        { p = whitespace
        ; info = Consume
            { empty = true
            ; first = Charset.of_list [' '; '\n'; '\t'; '\r']
            }
        ; typ = Value { v = (); p = whitespace }
        }

    let with_literal p =
        { p with p =
            begin
                let open Angstrom in
                return () >>= fun _ ->
                let res = ref None in
                consumed (p.p >>| fun v -> res := Some v) >>| fun str ->
                let [@warning "-8"] Some v = !res in
                (v, str)
            end
        ; typ = Parser
        }

    let consumed p =
        { p with p = Angstrom.consumed p.p
        ; typ = Parser
        }

    let state_get =
        { p = Angstrom.(state_get >>| (fun s -> s.custom))
        ; info = Empty
        ; typ = Parser
        }
    let state_map f =
        { p = Angstrom.(state_map (fun _pos s -> {s with custom = f s.custom}))
        ; info = Empty
        ; typ = Parser
        }
    let state_set custom = state_map (fun _ -> custom)

    let failed p =
        { p =
            { run = fun input pos state more fail succ ->
                let succ' input' _ _ more' _ =
                    fail input' pos state more' [] ""
                in
                let fail' input' _ _ more' _ _ =
                    succ input' pos state more' ()
                in

                p.p.run input pos state more fail' succ'
            }
        ; info = Empty
        ; typ = Parser
        }

    let opt p =
        { p = Angstrom.(option None (p.p >>| fun x -> Some x))
        ; info =
            begin match p.info with
            | Unknown -> Unknown
            | Empty -> Empty
            | Consume p -> Consume {p with empty = true}
            end
        ; typ = Parser
        }

    let seq min_n ?sep ?(trail = false) p =
        let pi, si, tail =
            match sep with
            | None -> p.info, Empty, Angstrom.many p.p
            | Some sep ->
                let tail =
                    let open Angstrom in
                    let t = many (sep.p >> p.p) in
                    if trail then t << option () (sep.p >>| fun _ -> ()) else t
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
                { p2 with empty = min_n < 2 }
            | Consume {empty = true; _}, Empty -> failwith "Endless sequence"
            | Consume p1, Empty ->
                Consume
                { p1 with empty = min_n = 0 }
            | Consume {empty = true; _}, Consume {empty = true; _} -> failwith "Endless sequence"
            | Consume p1, Consume p2 ->
                if p1.empty then
                    Consume
                    { empty = p1.empty && min_n = 1
                    ; first = Charset.union p1.first p2.first
                    }
                else
                    Consume
                    { p1 with empty = min_n = 0 }
        in

        { p =
            begin
                let open Angstrom in
                let list =
                    map2 p.p tail
                    ~f:begin fun first tail -> first::tail end
                    <|>
                    return []
                in
                list >>= fun list ->
                if List.length list < min_n then
                    fail ""
                else
                    return list
            end
        ; info
        ; typ = Parser
        }

    let trail = true

    let make_location loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
    let comb_location loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end
    let loc_comb loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end

    let ( <*>* ) a b = a <*> seq 0 b
    let ( <*>+ ) a b = a <*> seq 1 b
    let ( <*>? ) a b = a <*> opt b

    let mapping = return

    let loc p =
        mapping begin fun p1 x p2 -> Location.mkloc x @@ make_location p1 p2 end
        <*> pos <*> p <*> pos

    let loc_of p =
        mapping make_location
        <*> pos << p <*> pos

    let t2: 'a 'b. 'a -> 'b -> 'a * 'b = fun a b -> a, b
    let t3: 'a 'b 'c. 'a -> 'b -> 'c -> 'a * 'b * 'c = fun a b c -> a, b, c
    let t4: 'a 'b 'c 'd. 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd = fun a b c d -> a, b, c, d
    let cons x xs = x :: xs

    let exec f =
        { p = Angstrom.exec f
        ; info = Empty
        ; typ = Parser
        }

    let fold_left_0_n ~f nil p =
        let info = (nil >> seq 0 p).info in

        { p =
            begin
                let open Angstrom in
                let rec loop acc =
                    (p.p >>= fun x -> loop @@ f acc x)
                    <|>
                    return acc
                in
                nil.p >>= loop
            end
        ; info
        ; typ = Parser
        }



    let fold_left_0_1 ~f ~fr nil p =
            (mapping f <*> nil <*> p)
        <|> nil
        >>| fr

(*
    let fold_right_0_n ~f p nil =
        fix @@ fun loop ->
            map2 p loop
            ~f:begin fun p acc -> f p acc end
            <|>
            nil
*)
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

    let wrapped start final p =
        { p with p = (start >> p << final <|> (final >> fail)).p }

    let rapply v f =
        mapping (fun v f -> f v) <*> v <*> f
end
