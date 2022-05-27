open Base

type 'a scan_result =
  | Tag of 'a
  | WithPayload of { tag : 'a; payload : Caml.Obj.t }
  | Eof

let with_payload tag p = WithPayload { tag; payload = Caml.Obj.repr p }

type 'a token = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  tag : 'a;
  payload : Caml.Obj.t option;
}

type ('t, 'l) state_immutable = {
  file_start : Lexing.position;
  file_end : Lexing.position;
  tokens : 't token array;
  memo_tables : (int, (int, Caml.Obj.t) Hashtbl.t) Hashtbl.t;
  trace_depth : int ref;
  trace_entries : Exec_info.entry list ref;
}

type ('t, 'l) state = {
  im : ('t, 'l) state_immutable;
  pos : int;
  log : 'l list;
  log_parts : 'l list list;
}

let prev_token_end state =
  if state.pos = 0 then state.im.file_start
  else state.im.tokens.(state.pos - 1).loc_end

let next_token_start state =
  if state.pos = Array.length state.im.tokens then state.im.file_end
  else state.im.tokens.(state.pos).loc_start

type ('a, 't, 'l) failure = ('t, 'l) state -> 'a option

type ('a, 'r, 't, 'l) success = ('t, 'l) state -> 'a -> 'r option

type ('a, 't, 'l) simple = {
  run :
    'r.
    ('t, 'l) state ->
    ('r, 't, 'l) failure ->
    ('a, 'r, 't, 'l) success ->
    'r option;
}

type (_, 't, 'l) typ =
  | Parser : _ typ
  | Return : 'a -> ('a, 't, 'l) typ
  | Value : { v : 'a; p : (_, 't, 'l) simple } -> ('a, 't, 'l) typ
  | Lift : { f : 'a -> 'b; a : ('a, 't, 'l) simple } -> ('b, 't, 'l) typ
  | Lift2 : {
      f : 'a -> 'b -> 'c;
      a : ('a, 't, 'l) simple;
      b : ('b, 't, 'l) simple;
    }
      -> ('c, 't, 'l) typ
  | Lift3 : {
      f : 'a -> 'b -> 'c -> 'd;
      a : ('a, 't, 'l) simple;
      b : ('b, 't, 'l) simple;
      c : ('c, 't, 'l) simple;
    }
      -> ('d, 't, 'l) typ

type info = Unknown | Empty | Consume of { empty : bool; first : Tset.t }

type ('a, 't, 'l) t = {
  p : ('a, 't, 'l) simple;
  info : info;
  typ : ('a, 't, 'l) typ;
  id : int;
}
