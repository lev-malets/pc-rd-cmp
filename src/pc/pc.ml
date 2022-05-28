open Base
include Sigs

let loc_mk loc_start loc_end =
  Location.{ loc_start; loc_end; loc_ghost = false }

let loc_comb loc1 loc2 = loc_mk loc1.Location.loc_start loc2.Location.loc_end

let ( & ) = ( @@ )

module Utils = struct
  let mk_regexp xs =
    let rec loop = function
      | [] -> ""
      | [ x ] -> "\\(" ^ x ^ "\\)"
      | x :: xs -> "\\(" ^ x ^ "\\)\\|" ^ loop xs
    in
    Str.regexp @@ loop xs

  let empty_regexp = mk_regexp []

  let empty_regexp_pair = { accept = empty_regexp; decline = empty_regexp }

  module type MK_CONF = functor (Log : CONF_LOG) -> CONF with module Log = Log

  type _ parse_triplet =
    | Triplet :
        (string * (Yojson.Safe.t -> ('a, string) Result.t) * 'a Ref.t)
        -> unit parse_triplet

  let mk_conf ?filename ~src : ((module MK_CONF), string) Result.t =
    let open Yojson.Safe in
    let open Util in
    let open Result in
    let json = from_string ?fname:filename src in

    let error_prefix s = Result.map_error ~f:(fun x -> s ^ " > " ^ x) in

    let to_bool x =
      match x with
      | `Null -> Ok false
      | `Bool x -> Ok x
      | _ -> Error "expected boolean"
    in

    let list_to_regexp x =
      x
      |> List.map ~f:(function
           | `String s -> Ok s
           | _ -> Error "expected array of strings")
      |> Result.all >>| mk_regexp
    in

    let to_regexp x =
      match x with
      | `Null -> Ok empty_regexp
      | `List x -> list_to_regexp x
      | _ -> Error "expected array of strings"
    in

    let regexp_pair ?(accept = empty_regexp) ?(decline = empty_regexp) () =
      { accept; decline }
    in

    let parse_fields fields =
      let helper =
        List.fold
          ~init:(fun s _ -> Error (Printf.sprintf "unexpected key '%s'" s))
          ~f:(fun acc (Triplet (name, mapper, ref)) s x ->
            if String.equal s name then
              error_prefix name (mapper x) >>| ( := ) ref
            else acc s x)
          fields
      in
      fun pairs -> List.map ~f:(fun (s, x) -> helper s x) pairs |> Result.all
    in

    let to_regexp_pair x =
      match x with
      | `Null -> Ok (regexp_pair ())
      | `Assoc pairs ->
          let accept = ref empty_regexp in
          let decline = ref empty_regexp in
          parse_fields
            [
              Triplet ("include", to_regexp, accept);
              Triplet ("exclude", to_regexp, decline);
            ]
            pairs
          >>| fun _ -> { accept = !accept; decline = !decline }
      | _ -> Error "expected filter"
    in

    let to_peek_auto_conf x =
      match x with
      | `Null -> Ok Conf.Peek.Auto.Disable
      | `Assoc pairs ->
          let min_variants = ref Conf.Peek.Auto.default_min_variants in
          let to_int x =
            match x with `Int i -> Ok i | _ -> Error "expected integer"
          in

          parse_fields [ Triplet ("min-variants", to_int, min_variants) ] pairs
          >>| fun _ -> Conf.Peek.Auto.Enable { min_variants = !min_variants }
      | _ -> Error "expected auto peek conf"
    in

    let to_peek_conf x =
      match x with
      | `Null ->
          Ok
            Conf.Peek.
              { filter = empty_regexp_pair; auto = Conf.Peek.Auto.default }
      | `Assoc pairs ->
          let filter = ref empty_regexp_pair in
          let auto = ref Conf.Peek.Auto.default in
          parse_fields
            [
              Triplet ("filter", to_regexp_pair, filter);
              Triplet ("auto", to_peek_auto_conf, auto);
            ]
            pairs
          >>| fun _ -> Conf.Peek.{ filter = !filter; auto = !auto }
      | _ -> Error "expected peek conf"
    in

    let config =
      match json with
      | `Assoc pairs ->
          let debug = ref false in
          let memoize = ref empty_regexp_pair in
          let trace = ref empty_regexp_pair in
          let peek =
            ref
              Conf.Peek.
                { filter = empty_regexp_pair; auto = Conf.Peek.Auto.default }
          in

          parse_fields
            [
              Triplet ("memoize", to_regexp_pair, memoize);
              Triplet ("trace", to_regexp_pair, trace);
              Triplet ("peek", to_peek_conf, peek);
              Triplet ("debug", to_bool, debug);
            ]
            pairs
          >>| fun _ ->
          Conf.
            { memoize = !memoize; trace = !trace; peek = !peek; debug = !debug }
      | _ -> Error "expected object"
    in

    error_prefix (Option.value ~default:"__config__" filename) config
    >>| fun config : (module MK_CONF) ->
    (module functor
              (Log : CONF_LOG)
              ->
              struct
                module Log = Log

                let config = config
              end)

  let check_string regexp str =
    Str.string_match regexp str 0 && Str.match_end () = String.length str

  let check_string__pair pair str =
    check_string pair.accept str && not (check_string pair.decline str)
end

module Make
    (Basic : COMB_BASE)
    (Conf : CONF with type Log.elem = Basic.log_elem) :
  COMB
    with type 'a t = 'a Basic.t
     and type 'a Simple.t = 'a Basic.Simple.t
     and type log_elem = Basic.log_elem = struct
  include Basic

  let config = Conf.config

  let ( + ) = ( <*> )

  let ( - ) = ( << )

  let fix ?info f = fix_gen (fun get -> f @@ get.get ?info (fun x -> x))

  let mapping = return

  let fold_left_0_n ~f nil p =
    let hlp = nil >> seq p in
    modify hlp
      ~simple:
        (let open Simple in
        let rec loop acc =
          simple p >>= (fun x -> loop @@ f acc x) <|> return acc
        in
        simple nil >>= loop)

  let fold_left_0_1 ~f nil p = mapping f <*> nil <*> p <|> nil

  let fold_left_cont_0_n nil cont =
    let tail =
      fix @@ fun tail ->
      mapping (fun cont tail prev ->
          match tail with None -> cont prev | Some tail -> tail (cont prev))
      + cont + opt tail
    in

    mapping (fun x tail -> match tail with None -> x | Some tail -> tail x)
    + nil + opt tail

  let fold_left_cont_0_1 nil cont =
    mapping (fun x cont -> match cont with None -> x | Some cont -> cont x)
    + nil + opt cont

  let ( && ) v f = mapping (fun v f -> f v) <*> v <*> f

  let loc p =
    mapping (fun p1 x p2 -> Location.mkloc x @@ loc_mk p1 p2)
    + pos + p + pos_end

  let loc_of p = mapping loc_mk + pos - p + pos_end

  let with_loc p = mapping (fun p1 f p2 -> f (loc_mk p1 p2)) + pos + p + pos

  let memoid2id = Hashtbl.create (module Int)

  let id2memoid : (int, int) Hashtbl.t = Hashtbl.create (module Int)

  let name2id = Hashtbl.create (module String)

  let id2name = Hashtbl.create (module Int)

  let named (name : string) (p : 'a t) =
    let p =
      match
        ( Utils.check_string__pair config.memoize name,
          Utils.check_string__pair config.trace name )
      with
      | false, false -> touch p
      | true, false ->
          let p' = memo p in
          Hashtbl.add_exn memoid2id ~key:(id p') ~data:(id p);
          Hashtbl.add_exn id2memoid ~key:(id p) ~data:(id p');
          p'
      | false, true -> traced p
      | true, true ->
          let p = traced p in
          let p' = traced @@ memo p in
          Hashtbl.add_exn memoid2id ~key:(id p') ~data:(id p);
          Hashtbl.add_exn id2memoid ~key:(id p) ~data:(id p');
          p'
    in

    Hashtbl.add_exn name2id ~key:name ~data:(id p);
    Hashtbl.add_exn id2name ~key:(id p) ~data:name;
    p

  let rec name_of_id id =
    match Hashtbl.find id2memoid id with
    | Some id ->
        let name = name_of_id id in
        name ^ "*"
    | None -> (
        match Hashtbl.find id2name id with Some x -> x | None -> "_unnamed_")

  let name_of p = name_of_id @@ id p

  let choice ?name ps =
    match name with
    | None ->
        let rec loop = function
          | [] -> failwith "check usage"
          | [ x ] -> x
          | x :: xs -> x <|> loop xs
        in
        loop ps
    | Some name ->
        let p =
          if Utils.check_string__pair config.peek.filter name then peek_first ps
          else
            let check x =
              if not_empty x then x else failwith "check usage: empty"
            in
            let rec loop = function
              | [] -> failwith "check usage"
              | [ x ] -> check x
              | x :: xs -> check x <|> loop xs
            in
            loop ps
        in
        named name p
end
