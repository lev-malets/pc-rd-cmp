open Base
open Exec_info
open Compilerlibs406

module Raws = struct
  let _x =
    { id = 0
    ; depth = 0
    ; enter_pos = Lexing.dummy_pos
    ; exit_pos = Lexing.dummy_pos
    ; enter_time = 0.
    ; exit_time = 1.
    ; succeded = true }

  let empty = []
  let single = [_x]

  let complex =
    (*
            0 - 0 - 1
            |
            1 - 0 - 0
            |
            0 - 1 - 0
        *)
    [ {_x with id = 1; depth = 2; enter_time = 2.; exit_time = 3.}
    ; {_x with id = 0; depth = 1; enter_time = 1.; exit_time = 3.}
    ; {_x with id = 0; depth = 0; enter_time = 0.; exit_time = 3.}
    ; {_x with id = 0; depth = 2; enter_time = 5.; exit_time = 6.}
    ; {_x with id = 0; depth = 1; enter_time = 4.; exit_time = 6.}
    ; {_x with id = 1; depth = 0; enter_time = 3.; exit_time = 6.}
    ; {_x with id = 0; depth = 2; enter_time = 8.; exit_time = 9.}
    ; {_x with id = 1; depth = 1; enter_time = 7.; exit_time = 9.}
    ; {_x with id = 0; depth = 0; enter_time = 6.; exit_time = 9.} ]
end

module Stats = struct
  let empty = to_stats Raws.empty
  let single = to_stats Raws.single
  let complex = to_stats Raws.complex
end

let%test "empty: stats are empty" = Hashtbl.length Stats.empty = 0
let%test "single: stats table has one entry" = Hashtbl.length Stats.single = 1

let%test "single stats" =
  let stats = Hashtbl.find_exn Stats.single 0 in
  stats.pos_count = 1
  && Float.(stats.time.sum = 1.)
  && stats.time.count = 1
  && Float.(stats.time_individual.sum = 1.)

let%test "complex: stats table has two entries" =
  Hashtbl.length Stats.complex = 2

let%test_unit "complex stats (id = 0)" =
  let stats = Hashtbl.find_exn Stats.complex 0 in
  [%test_eq: Exec_info.stats] stats
    { call_count = 6
    ; pos_count = 1
    ; time = FloatStatistics.{sum = 12.; min = 1.; max = 3.; count = 6}
    ; time_individual = {sum = 6.; min = 1.; max = 1.; count = 6} }

let%test_unit "complex stats (id = 1)" =
  let stats = Hashtbl.find_exn Stats.complex 1 in
  [%test_eq: Exec_info.stats] stats
    { call_count = 3
    ; pos_count = 1
    ; time = FloatStatistics.{sum = 6.; min = 1.; max = 3.; count = 3}
    ; time_individual = {sum = 3.; min = 1.; max = 1.; count = 3} }
