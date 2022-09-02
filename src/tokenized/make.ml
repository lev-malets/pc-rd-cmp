open Base

module Make (Tokenizer : Sigs.TOKENIZER) (Conf : Pc.CONF) :
  Sigs.TPC with type s = Conf.Log.elem and type tag = Tokenizer.Tag.t = struct
  module Id = struct
    let next = ref 0

    let get () =
      let x = !next in
      next := x + 1;
      x

    let fail = get ()
    let t2 = get ()
    let t3 = get ()
    let t4 = get ()
    let cons = get ()
    let eof = get ()
  end

  type s = Conf.Log.elem
  type tag = Tokenizer.Tag.t

  let tag2int : tag -> int = Caml.Obj.magic

  module Basic = struct
    module Conf = Conf
    open Parser

    module Simple = struct
      type 'a t = ('a, tag, Conf.Log.elem) Parser.simple

      let ( >> ) p1 p2 =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 _ = p2.run state1 fail succ in
              p1.run state fail succ1);
        }

      let ( << ) p1 p2 =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 v =
                let succ2 state2 _ = succ state2 v in
                p2.run state1 fail succ2
              in
              p1.run state fail succ1);
        }

      let ( >>| ) p f =
        {
          run =
            (fun state fail succ ->
              let succ' state' v = succ state' (f v) in
              p.run state fail succ');
        }

      let exec f = { run = (fun state _fail succ -> succ state (f ())) }
      let lift f p = p >>| f

      let lift2 f p1 p2 =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 v1 =
                let succ2 state2 v2 = succ state2 (f v1 v2) in
                p2.run state1 fail succ2
              in
              p1.run state fail succ1);
        }

      let lift3 f p1 p2 p3 =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 v1 =
                let succ2 state2 v2 =
                  let succ3 state3 v3 = succ state3 (f v1 v2 v3) in
                  p3.run state2 fail succ3
                in
                p2.run state1 fail succ2
              in
              p1.run state fail succ1);
        }

      let lift4 f p1 p2 p3 p4 =
        {
          run =
            (fun state fail succ ->
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
              p1.run state fail succ1);
        }

      let ( <|> ) p1 p2 =
        {
          run =
            (fun state fail succ ->
              let fail1 _ = p2.run state fail succ in
              p1.run state fail1 succ);
        }

      let ( <*> ) pf pv =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 f =
                let succ2 state2 v = succ state2 (f v) in
                pv.run state1 fail succ2
              in
              pf.run state fail succ1);
        }

      let ( >>= ) p f =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 v = (f v).run state1 fail succ in
              p.run state fail succ1);
        }

      let ( >>$ ) p v =
        {
          run =
            (fun state fail succ ->
              let succ1 state1 _ = succ state1 v in
              p.run state fail succ1);
        }

      let fix f =
        let rec p = lazy (f r)
        and r = { run = (fun i -> (Lazy.force p).run i) } in
        Lazy.force p

      let return x = { run = (fun state _fail succ -> succ state x) }
      let fail = { run = (fun state fail _succ -> fail state) }

      let many p =
        fix @@ fun many_p -> lift2 (fun x xs -> x :: xs) p many_p <|> return []

      let log x =
        {
          run =
            (fun state _fail succ ->
              let state = { state with log = x :: state.log } in
              succ state ());
        }

      let log_many log =
        {
          run =
            (fun state _fail succ ->
              let state =
                { state with log; log_parts = state.log :: state.log_parts }
              in
              succ state ());
        }
    end

    type 'a t = ('a, tag, Conf.Log.elem) Parser.t
    type 'b getter = { get : 'a 'c. ?info:'c t -> ('b -> 'a t) -> 'a t }

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
              | Consume p2 ->
                  Consume { p2 with first = Tset.union first p2.first }
            else p1
      in

      { p; info; typ; id = Id.get () }

    let ( >> ) (p1 : 'a t) p2 =
      match (p1.typ, p2.typ) with
      | Return _, _ -> p2
      | _, Return v ->
          mk_parser_cont p1 p2
            ~p:Simple.(p1.p >>| fun _ -> v)
            ~typ:(Value { v; p = p1.p })
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
          mk_parser_cont p1 p2
            ~p:Simple.(lift3 f a b c)
            ~typ:(Lift3 { f; a; b; c })
      | _ -> mk_parser_cont p1 p2 ~p:Simple.(p1.p >> p2.p) ~typ:Parser

    let ( << ) p1 p2 =
      match (p1.typ, p2.typ) with
      | _, Return _ -> p1
      | Return v, _ ->
          mk_parser_cont p1 p2
            ~p:Simple.(p2.p >>| fun _ -> v)
            ~typ:(Value { v; p = p2.p })
      | Value { v; _ }, _ ->
          let p = Simple.(p1.p << p2.p) in
          mk_parser_cont p1 p2 ~p ~typ:(Value { v; p })
      | Lift { f; a }, _ ->
          let a = Simple.( << ) a p2.p in
          mk_parser_cont p1 p2 ~p:Simple.(lift f a) ~typ:(Lift { f; a })
      | Lift2 { f; a; b }, _ ->
          let b = Simple.( << ) b p2.p in
          mk_parser_cont p1 p2 ~p:Simple.(lift2 f a b) ~typ:(Lift2 { f; a; b })
      | Lift3 { f; a; b; c }, _ ->
          let c = Simple.( << ) c p2.p in
          mk_parser_cont p1 p2
            ~p:Simple.(lift3 f a b c)
            ~typ:(Lift3 { f; a; b; c })
      | _ -> mk_parser_cont p1 p2 ~p:Simple.(p1.p << p2.p) ~typ:Parser

    let ( <|> ) p1 p2 =
      {
        p = Simple.(p1.p <|> p2.p);
        info =
          (match (p1.info, p2.info) with
          | Unknown, _ -> Unknown
          | _, Unknown -> Unknown
          | Empty, Empty -> Empty
          | Empty, Consume p2 -> Consume { p2 with empty = true }
          | Consume p1, Empty -> Consume { p1 with empty = true }
          | Consume p1, Consume p2 ->
              Consume
                {
                  empty = p1.empty || p2.empty;
                  first = Tset.union p1.first p2.first;
                });
        typ = Parser;
        id = Id.get ();
      }

    let ( <*> ) pf pv =
      match pf.typ with
      | Return f ->
          let a = pv.p in
          mk_parser_cont pf pv ~p:Simple.(lift f a) ~typ:(Lift { f; a })
      | Value { v = f; p } ->
          let a = Simple.(p >> pv.p) in
          mk_parser_cont pf pv ~p:Simple.(lift f a) ~typ:(Lift { f; a })
      | Lift { f; a } ->
          mk_parser_cont pf pv
            ~p:Simple.(lift2 f a pv.p)
            ~typ:(Lift2 { f; a; b = pv.p })
      | Lift2 { f; a; b } ->
          mk_parser_cont pf pv
            ~p:Simple.(lift3 f a b pv.p)
            ~typ:(Lift3 { f; a; b; c = pv.p })
      | Lift3 { f; a; b; c } ->
          mk_parser_cont pf pv ~p:Simple.(lift4 f a b c pv.p) ~typ:Parser
      | _ -> mk_parser_cont pf pv ~p:Simple.(pf.p <*> pv.p) ~typ:Parser

    let ( >>$ ) pp v =
      let p = Simple.(pp.p >>$ v) in
      { pp with p; typ = Value { v; p }; id = Id.get () }

    let ( >>| ) p f =
      { p with p = Simple.(p.p >>| f); typ = Parser; id = Id.get () }

    let fix_gen f =
      let rec res = lazy (f getter)
      and getter =
        {
          get =
            (fun ?info get ->
              match info with
              | None ->
                  {
                    p = { run = (fun i -> (get @@ Lazy.force res).p.run i) };
                    info = Unknown;
                    typ = Parser;
                    id = Id.get ();
                  }
              | Some info ->
                  {
                    p =
                      (if Conf.config.debug then
                       {
                         run =
                           (fun i ->
                             let p = get @@ Lazy.force res in
                             assert (equal_info p.info info.info);
                             p.p.run i);
                       }
                      else { run = (fun i -> (get @@ Lazy.force res).p.run i) });
                    info = info.info;
                    typ = Parser;
                    id = Id.get ();
                  });
        }
      in
      Lazy.force res

    let return x =
      { p = Simple.(return x); info = Empty; typ = Return x; id = Id.get () }

    let fail = { p = Simple.fail; info = Unknown; typ = Parser; id = Id.fail }
    let touch p = { p with id = Id.get () }

    let modify ~simple p =
      match p.typ with
      | Parser -> { p = simple; id = Id.get (); typ = Parser; info = p.info }
      | _ -> failwith "check usage"

    let pos : Compilerlibs406.Lexing.position t =
      {
        p =
          {
            run = (fun state _fail succ -> succ state (next_token_start state));
          };
        info = Empty;
        typ = Parser;
        id = Id.get ();
      }

    let pos_end : Compilerlibs406.Lexing.position t =
      {
        p =
          { run = (fun state _fail succ -> succ state (prev_token_end state)) };
        info = Empty;
        typ = Parser;
        id = Id.get ();
      }

    let exec f =
      { p = Simple.exec f; info = Empty; typ = Parser; id = Id.get () }

    let failed p =
      {
        p =
          {
            run =
              (fun state fail succ ->
                let succ1 _ _ = fail state in
                let fail1 _ = succ state () in

                p.p.run state fail1 succ1);
          };
        info = Empty;
        typ = Parser;
        id = Id.get ();
      }

    let opt p =
      {
        p = Simple.(p.p >>| (fun x -> Some x) <|> return None);
        info =
          (match p.info with
          | Unknown -> Unknown
          | Empty -> Empty
          | Consume p -> Consume { p with empty = true });
        typ = Parser;
        id = Id.get ();
      }

    let seq ?(n = 0) ?sep ?(trail = false) p =
      let pi, si, tail =
        match sep with
        | None -> (p.info, Empty, Simple.many p.p)
        | Some sep ->
            let tail =
              let open Simple in
              let t = many (sep.p >> p.p) in
              if trail then t << (sep.p >>| (fun _ -> ()) <|> return ()) else t
            in

            (p.info, sep.info, tail)
      in

      let info =
        match (pi, si) with
        | Unknown, _ -> Unknown
        | _, Unknown -> Unknown
        | Empty, Empty -> failwith "Endless sequence"
        | Empty, Consume { empty = true; _ } -> failwith "Endless sequence"
        | Empty, Consume p2 -> Consume { p2 with empty = n < 2 }
        | Consume { empty = true; _ }, Empty -> failwith "Endless sequence"
        | Consume p1, Empty -> Consume { p1 with empty = n = 0 }
        | Consume { empty = true; _ }, Consume { empty = true; _ } ->
            failwith "Endless sequence"
        | Consume p1, Consume p2 ->
            if p1.empty then
              Consume
                {
                  empty = p1.empty && n = 1;
                  first = Tset.union p1.first p2.first;
                }
            else Consume { p1 with empty = n = 0 }
      in

      {
        p =
          (let open Simple in
          let list =
            lift2 (fun first tail -> first :: tail) p.p tail <|> return []
          in
          list >>= fun list ->
          if List.length list < n then fail else return list);
        info;
        typ = Parser;
        id = Id.get ();
      }

    let t2 =
      let f a b = (a, b) in
      {
        p = { run = (fun state _fail succ -> succ state f) };
        info = Empty;
        typ = Return f;
        id = Id.t2;
      }

    let t3 =
      let f a b c = (a, b, c) in
      {
        p = { run = (fun state _fail succ -> succ state f) };
        info = Empty;
        typ = Return f;
        id = Id.t3;
      }

    let t4 =
      let f a b c d = (a, b, c, d) in
      {
        p = { run = (fun state _fail succ -> succ state f) };
        info = Empty;
        typ = Return f;
        id = Id.t4;
      }

    let cons =
      let f x xs = x :: xs in
      {
        p = { run = (fun state _fail succ -> succ state f) };
        info = Empty;
        typ = Return f;
        id = Id.cons;
      }

    let not_empty p =
      match p.info with Consume { empty = false; _ } -> true | _ -> false

    let first_size_max = Tset.size Tset.full

    let first_size p =
      match p.info with
      | Unknown -> None
      | Empty -> None
      | Consume { first; _ } -> Some (Tset.size first)

    let first_iter ~f p =
      match p.info with
      | Consume { first; _ } -> Tset.iter_code f first
      | _ -> failwith "check usage"

    let peek_first expected =
      let arr = Array.create ~len:256 Simple.fail in
      let first = ref Tset.empty in

      let rec loop = function
        | [] -> ()
        | p :: xs ->
            let p_first =
              match p.info with
              | Consume { empty = false; first } -> first
              | _ -> failwith (Printf.sprintf "%d" @@ List.length xs)
            in

            first := Tset.union !first p_first;

            loop xs;

            p_first
            |> Tset.iter_code (fun c -> arr.(c) <- Simple.(p.p <|> arr.(c)))
      in
      loop expected;

      {
        p =
          {
            run =
              (fun state fail succ ->
                if state.pos = Array.length state.im.tokens then fail state
                else
                  let p = arr.(tag2int state.im.tokens.(state.pos).tag) in
                  p.run state fail succ);
          };
        info = Consume { empty = false; first = !first };
        typ = Parser;
        id = Id.get ();
      }

    let memo p =
      let id = Id.get () in

      {
        p with
        id;
        p =
          {
            run =
              (fun state fail succ ->
                let table =
                  Hashtbl.find_or_add state.im.memo_tables id ~default:(fun _ ->
                      Hashtbl.create (module Int))
                in
                let key = state.pos in

                match Hashtbl.find table key with
                | Some t -> (
                    let t, state' = Caml.Obj.obj t in
                    let state =
                      { state' with log_parts = state.log :: state.log_parts }
                    in

                    match t with Some v -> succ state v | None -> fail state)
                | None ->
                    let set_data state x =
                      Hashtbl.add_exn table ~key
                        ~data:(Caml.Obj.repr (x, state))
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
                      {
                        state with
                        log = [];
                        log_parts = state.log :: state.log_parts;
                      }
                    in

                    p.p.run state fail succ);
          };
      }

    let traced p =
      let id = Id.get () in
      Parser.
        {
          p with
          id;
          p =
            {
              Parser.run =
                (fun state fail succ ->
                  let enter_time = Caml.Sys.time () in
                  let enter_pos = Parser.next_token_start state in
                  let depth = state.im.trace_depth in
                  let entries = state.im.trace_entries in
                  let d = !depth in
                  depth := d + 1;

                  let finalize state succeded =
                    depth := d;

                    let exit_pos = Parser.next_token_start state in
                    let exit_time = Caml.Sys.time () in

                    let entry =
                      Exec_info.
                        {
                          id;
                          enter_time;
                          enter_pos = Caml.Obj.magic enter_pos;
                          exit_time;
                          exit_pos = Caml.Obj.magic exit_pos;
                          succeded;
                          depth = d;
                        }
                    in

                    (* begin match Hashtbl.find Pos.id2name p.id with
                       | Some name ->
                           if enter_pos.pos_cnum > (!max_pos).pos_cnum then
                               begin
                                   max_pos := enter_pos;
                                   let spc = String.make !depth ' ' in
                                   print_endline (Printf.sprintf "%s%s %d:%d" spc name enter_pos.pos_lnum (enter_pos.pos_cnum - enter_pos.pos_bol + 1));
                                   entries := [entry]
                               end
                       | None -> ()
                       end; *)
                    entries := entry :: !entries
                  in

                  let succ state v =
                    finalize state true;
                    succ state v
                  in

                  let fail state =
                    finalize state false;
                    fail state
                  in

                  p.p.run state fail succ);
            };
        }

    let fold_log init f =
      {
        p =
          {
            run =
              (fun state _ succ ->
                let init = List.fold_left ~init ~f state.log in

                let res =
                  List.fold_left state.log_parts ~init ~f:(fun init ->
                      List.fold_left ~init ~f)
                in

                succ state res);
          };
        info = Empty;
        typ = Parser;
        id = Id.get ();
      }

    let scan lexbuf =
      let rec loop tokens =
        Tokenizer.scan_space lexbuf;
        let loc_start = Caml.Obj.magic lexbuf.lex_curr_p in
        (* Caml.print_endline (Printf.sprintf "%d:%d" loc_start.pos_lnum Int.(loc_start.pos_cnum - loc_start.pos_bol + 1)); *)
        match Tokenizer.scan_token lexbuf with
        | Eof -> (tokens, loc_start)
        | Tag tag ->
            let loc_end = Caml.Obj.magic lexbuf.lex_curr_p in
            loop ({ loc_start; tag; payload = None; loc_end } :: tokens)
        | WithPayload { tag; payload } ->
            let loc_end = Caml.Obj.magic lexbuf.lex_curr_p in
            loop ({ loc_start; tag; payload = Some payload; loc_end } :: tokens)
      in
      let tokens, pos_eof = loop [] in
      (Array.of_list_rev tokens, pos_eof)

    let parse_helper fail succ p ?(filename = "none") text =
      let lexbuf = Lexing.from_string text in
      let zero_pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      lexbuf.lex_start_p <- lexbuf.lex_curr_p;
      let tokens, pos_eof = scan lexbuf in

      p.p.run
        {
          im =
            {
              file_start = Caml.Obj.magic zero_pos;
              file_end = pos_eof;
              tokens;
              memo_tables = Hashtbl.create (module Int);
              trace_depth = ref 0;
              trace_entries = ref [];
            };
          pos = 0;
          log = [];
          log_parts = [];
        }
        fail succ

    let parse_string p ?filename text =
      let succ state v =
        if state.pos = Array.length state.im.tokens then Some v else None
      in
      let fail _state = None in
      parse_helper fail succ p ?filename text

    let parse_string_with_trace p ?filename text =
      let succ state v =
        let x =
          if state.pos = Array.length state.im.tokens then Some v else None
        in
        Some (x, List.rev !(state.im.trace_entries))
      in
      let fail state = Some (None, List.rev !(state.im.trace_entries)) in

      let[@warning "-8"] (Some x) = parse_helper fail succ p ?filename text in
      x

    let id p = p.id
    let simple p = p.p

    let run p =
      let info =
        match p.info with
        | Consume { empty = false; _ } as x -> x
        | _ -> Unknown
      in

      {
        p =
          {
            run =
              (fun state fail succ ->
                let succ' state' v = v.run state' fail succ in
                p.p.run state fail succ');
          };
        info;
        typ = Parser;
        id = Id.get ();
      }

    let print_info p =
      Caml.print_endline
      @@
      match p.info with
      | Unknown -> "unknown"
      | Empty -> "empty"
      | Consume { empty = true; _ } -> "consume+empty"
      | _ -> "consume"

    let eof =
      let p =
        {
          run =
            (fun state fail succ ->
              if state.pos = Array.length state.im.tokens then succ state ()
              else fail state);
        }
      in
      { p; info = Empty; typ = Value { v = (); p }; id = Id.eof }
  end

  include Pc.Make (Basic)
  open Parser

  let tkn_helper ~f tag =
    let p =
      {
        run =
          (fun state fail succ ->
            if state.pos = Array.length state.im.tokens then fail state
            else
              let t = state.im.tokens.(state.pos) in
              if Tokenizer.Tag.equal t.tag tag then
                match f t with
                | Some x -> succ { state with pos = Int.succ state.pos } x
                | None -> fail state
              else fail state);
      }
    in

    {
      p;
      info = Consume { empty = false; first = Tset.singleton (tag2int tag) };
      typ = Parser;
      id = Id.get ();
    }

  let tkn tag =
    tkn_helper tag ~f:(fun t ->
        match t.payload with
        | Some payload -> Some (t.tag, Caml.Obj.obj payload)
        | _ -> None)

  let tkn_ tag = tkn_helper tag ~f:(fun _ -> Some ())
  let tkn_tag tag = tkn_helper tag ~f:(fun _ -> Some tag)

  let tkn_payload tag =
    tkn_helper tag ~f:(fun t ->
        match t.payload with
        | Some payload -> Some (Caml.Obj.obj payload)
        | _ -> None)

  let peek p =
    {
      p =
        {
          run =
            (fun state fail succ ->
              let succ _ v = succ state v in
              p.p.run state fail succ);
        };
      info = Unknown;
      typ = Parser;
      id = Id.get ();
    }
end
