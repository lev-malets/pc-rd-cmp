module Angstrom = Angstrom_pos.Make(State)

open Core_kernel
open Angstrom
open Let_syntax

let opt p = option None (p >>| Option.some)

let sequence ?(sep = return ()) p =
    let p1 = sep >> p in
    match%bind opt p with
    | None -> return []
    | Some first -> let%map tail = many p1 in first::tail
let sequence1 ?(sep = return ()) p =
    let p1 = sep >> p in
    let%map first = p
    and others = many p1 in
    first :: others

let upper = function 'A' .. 'Z' -> true | _ -> false
let lower = function 'a' .. 'z' -> true | _ -> false

let position = memo_by_pos position

let make_location loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
let comb_location loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end

let with_location p =
    let%map loc_start = position
    and res = p
    and loc_end = position
    in
    (res, make_location loc_start loc_end)

let check p = option false (p >>$ true)

let tuple3 p1 p2 p3 = lift3 (fun x y z -> (x, y, z)) p1 p2 p3

let tuple4 p1 p2 p3 p4 = lift4 (fun x y z w -> (x, y, z, w)) p1 p2 p3 p4
