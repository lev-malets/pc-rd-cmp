open Base
open Compilerlibs406

type entry =
  { id : int
  ; depth : int
  ; enter_pos : Lexing.position
  ; exit_pos : Lexing.position
  ; enter_time : float
  ; exit_time : float
  ; succeded : bool }

module PosKey = struct
  type t = Lexing.position

  let compare p1 p2 =
    let open Lexing in
    let c1 = compare p1.pos_cnum p2.pos_cnum in
    if c1 <> 0 then c1 else String.compare p1.pos_fname p2.pos_fname

  let sexp_of_t _ = failwith ""
  let hash p = p.Lexing.pos_bol
end

module FloatStatistics = struct
  type t = {sum : float; min : float; max : float; count : int}
  [@@deriving sexp, ord]

  let equal x y =
    Float.equal x.sum y.sum && Float.equal x.min y.min
    && Float.equal x.max y.max && Int.equal x.count y.count

  let empty = {sum = 0.; min = Float.max_value; max = Float.min_value; count = 0}

  let add x t =
    { sum = t.sum +. x
    ; min = Float.min x t.min
    ; max = Float.max x t.max
    ; count = t.count + 1 }

  let append x t =
    { sum = t.sum +. x.sum
    ; min = Float.min x.min t.min
    ; max = Float.max x.max t.max
    ; count = t.count + x.count }

  let mean t = t.sum /. Float.of_int t.count
end

module Collect = struct
  type stats =
    { positions : (PosKey.t, unit) Hashtbl.t
    ; mutable time : FloatStatistics.t
    ; mutable time_individual : FloatStatistics.t }

  let collect =
    let module Hlp = struct
      type t = {entries : entry list; time : float}
    end in
    fun table entries ->
      let rec helper depth entries =
        let rec loop entries time =
          match entries with
          | [] -> Hlp.{entries; time}
          | x :: xs as entries ->
              if x.depth < depth then {entries; time}
              else
                let x, xs, time_sub =
                  if x.depth = depth then (x, xs, 0.)
                  else
                    let Hlp.{entries; time = time_sub} =
                      helper (depth + 1) entries
                    in
                    match entries with
                    | [] -> failwith ""
                    | x :: xs ->
                        assert (x.depth = depth);
                        (x, xs, time_sub)
                in
                assert (x.depth = depth);
                let x_time = x.exit_time -. x.enter_time in
                let x_time_individual = x_time -. time_sub in
                let stats =
                  Hashtbl.find_or_add table x.id ~default:(fun _ ->
                      { positions = Hashtbl.create (module PosKey)
                      ; time = FloatStatistics.empty
                      ; time_individual = FloatStatistics.empty })
                in
                Hashtbl.change stats.positions x.enter_pos ~f:(fun _ -> Some ());
                stats.time <- FloatStatistics.add x_time stats.time;
                stats.time_individual <-
                  FloatStatistics.add x_time_individual stats.time_individual;
                loop xs (time +. x_time)
        in
        loop entries 0.
      in
      let hlp = helper 0 entries in
      assert (List.is_empty hlp.entries)
end

type stats =
  { call_count : int
  ; pos_count : int
  ; time : FloatStatistics.t
  ; time_individual : FloatStatistics.t }
[@@deriving sexp, ord]

let to_stats t =
  let table = Hashtbl.create (module Int) in
  Collect.collect table t;
  Hashtbl.map table ~f:(fun stats ->
      { call_count = stats.Collect.time.count
      ; pos_count = Hashtbl.length stats.positions
      ; time = stats.time
      ; time_individual = stats.time_individual })

let gt (p1 : Lexing.position) (p2 : Lexing.position) =
  let c1 = compare p1.pos_lnum p2.pos_lnum in
  if c1 > 0 then true
  else if c1 < 0 then false
  else
    let c2 = compare p1.pos_cnum p2.pos_cnum in
    if c2 > 0 then true else false

let last_pos =
  List.fold_left
    ~f:(fun acc entry ->
      if gt acc entry.enter_pos then acc else entry.enter_pos)
    ~init:Lexing.dummy_pos
