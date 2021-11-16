module Angstrom = Angstrom_pos.Make(State)

open Angstrom
open Core_kernel

type 'a parser = 'a Angstrom.Parser.t

let with_literal p =
    return () >>= fun _ ->
    let res = ref None in
    consumed (p >>| fun v -> res := Some v) >>| fun str ->
    let [@warning "-8"] Some v = !res in
    (v, str)

open Parsetree

module Opt = struct
    let set x =
        function
        | Some _ -> Some x
        | None -> None
end

type 'a helper_fn = p1:Lexing.position -> p2:Lexing.position -> attrs:attributes -> 'a
type 'a helper = 'a helper_fn * attributes
type 'a ahelper_fn = attrs:attributes -> 'a
type 'a ahelper = 'a ahelper_fn * attributes

let apply : 'a. 'a helper -> Lexing.position -> Lexing.position -> 'a
    = fun (hlp, attrs) p1 p2 -> hlp ~p1 ~p2 ~attrs

let apply_ah : 'a. 'a ahelper -> 'a
    = fun (hlp, attrs) -> hlp ~attrs

let helper: 'a. 'a helper_fn -> 'a helper
    = fun f -> f, []

let helper_map: 'a. ('a helper_fn -> 'b helper_fn) -> 'a helper -> 'b helper
    = fun f (hlp, attrs) -> (f hlp, attrs)


let ahelper_map: 'a. f:('a ahelper_fn -> 'b helper_fn) -> 'a ahelper -> 'b helper
    = fun ~f (hlp, attrs) -> (f hlp, attrs)

let helper_add_attr_loc : 'a. string -> Location.t -> 'a * attributes -> 'a * attributes
    = fun s loc (hlp, attrs) -> hlp, (Location.mkloc s loc, PStr []) :: attrs

let helper_add_attr : 'a. string -> 'a * attributes -> 'a * attributes
    = fun s (hlp, attrs) -> hlp, (Location.mknoloc s, PStr []) :: attrs

let helper_add_attrs : 'a. attributes -> 'a * attributes -> 'a * attributes
    = fun s (hlp, attrs) -> hlp, s @ attrs

let mk_helper_fn : 'a 'b. (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) -> 'a -> 'b helper_fn
    = fun f a -> fun ~p1 ~p2 ~attrs -> f ~loc:(make_location p1 p2) ~attrs a

let mk_helper : 'a 'b.
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) ->
    'a -> 'b helper
    =
    fun f a -> mk_helper_fn f a, []

let mk_na_helper : 'a 'b.
    (?loc:Warnings.loc -> 'a -> 'b) ->
    'a -> 'b helper
    =
    fun f a -> (fun ~p1 ~p2 ~attrs:_ -> f ~loc:(make_location p1 p2) a), []

let mk_na_helper2 : 'a 'b 'c.
    (?loc:Warnings.loc -> 'a -> 'b -> 'c) ->
    'a -> 'b -> 'c helper
    =
    fun f a b -> (fun ~p1 ~p2 ~attrs:_ -> f ~loc:(make_location p1 p2) a b), []

let mk_helper2 : 'a 'b 'c.
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) ->
    'a -> 'b -> 'c helper
    =
    fun f a b -> (fun ~p1 ~p2 ~attrs -> f ~loc:(make_location p1 p2) ~attrs a b), []

let mk_helper3 : 'a 'b 'c 'd.
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) ->
    'a -> 'b -> 'c -> 'd helper
    =
    fun f a b c -> (fun ~p1 ~p2 ~attrs -> f ~loc:(make_location p1 p2) ~attrs a b c), []

let mk_helper4 : 'a 'b 'c 'd 'e.
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) ->
    'a -> 'b -> 'c -> 'd -> 'e helper
    =
    fun f a b c d -> (fun ~p1 ~p2 ~attrs -> f ~loc:(make_location p1 p2) ~attrs a b c d), []

let mk_helper5 : 'a 'b 'c 'd 'e 'f.
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    'a -> 'b -> 'c -> 'd -> 'e -> 'f helper
    =
    fun f a b c d e -> (fun ~p1 ~p2 ~attrs -> f ~loc:(make_location p1 p2) ~attrs a b c d e), []

let set_loc : 'a. 'a helper parser -> 'a ahelper parser
    = fun p ->
        mapping (fun p1 (hlp, attrs) p2 -> hlp ~p1 ~p2, attrs)
        <*> pos <*> p <*> pos

let fold_hlp_left_0_1 ~f nil tail =
    mapping begin fun p1 n p2 t ->
        match t with
        | Some t -> f (apply n p1 p2) t
        | None -> n
    end
    <*> pos <*> nil <*> pos <*>? tail

let fold_hlp_left_0_n ~f nil p =
    let tail = fix @@ fun tail ->
        mapping begin fun x p2 tail ->
            fun acc p1 ->
                match tail with
                | None -> f acc x
                | Some tail -> tail (apply (f acc x) p1 p2) p1
        end
        <*> p <*> pos <*>? tail
    in

    mapping begin fun p1 x p2 tail ->
        match tail with
        | None -> x
        | Some tail -> tail (apply x p1 p2) p1
    end
    <*> pos <*> nil <*> pos <*>? tail

let some x = Some x

let make_list_helper ~constr ~tuple ~get_loc seq ext =
    mk_helper begin fun ?(loc=Location.none) ?attrs () ->
        let nil_loc = {loc with Location.loc_ghost = true} in
        let nil = Location.mkloc (Longident.Lident "[]") nil_loc in

        let rec loop f =
            function
            | [] ->
                begin match ext with
                | Some ext -> ext
                | None -> f nil_loc nil None
                end
            | x::xs ->
                let tail_exp = loop (fun loc -> constr ?loc:(Some loc) ?attrs:None) xs in
                let loc = {loc with loc_start = (get_loc x).Location.loc_start} in
                let arg = tuple ?loc:(Some loc) ?attrs:None [x; tail_exp] in
                f loc (Location.mkloc (Longident.Lident "::") loc) (Some arg)
        in

        loop (fun loc -> constr ?loc:(Some loc) ?attrs) seq
    end ()
