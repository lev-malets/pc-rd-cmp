open Compilerlibs406
open Base
include Sigs

let loc_mk loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
let loc_comb loc1 loc2 = loc_mk loc1.Location.loc_start loc2.Location.loc_end
let ( & ) = ( @@ )

module Utils = struct
  let mk_regexp xs =
    let rec loop = function
      | [] -> ""
      | [x] -> "\\(" ^ x ^ "\\)"
      | x :: xs -> "\\(" ^ x ^ "\\)\\|" ^ loop xs
    in
    let xs =
      List.sort xs ~compare:(fun a b ->
          Int.compare (String.length b) (String.length a))
    in
    let s = loop xs in
    Str.regexp s

  let empty_regexp = mk_regexp []
  let empty_regexp_pair = {accept = empty_regexp; decline = empty_regexp}

  module type MK_CONF = functor (Log : CONF_LOG) -> CONF with module Log = Log

  type _ parse_triplet =
    | Triplet :
        (string * (Yojson.Safe.t -> ('a, string) Result.t) * 'a Ref.t)
        -> unit parse_triplet

  let mk_conf ?filename src : ((module MK_CONF), string) Result.t =
    let open Yojson.Safe in
    let open Result in
    let json = from_string ?fname:filename src in
    (* Stdio.print_endline @@ Yojson.Safe.pretty_to_string json; *)
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
      {accept; decline}
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
            [ Triplet ("include", to_regexp, accept)
            ; Triplet ("exclude", to_regexp, decline) ]
            pairs
          >>| fun _ -> {accept = !accept; decline = !decline}
      | _ -> Error "expected filter"
    in
    let to_peek_auto_conf x =
      let open Config.Peek.Auto in
      match x with
      | `Null -> Ok Disable
      | `Assoc pairs ->
          let min_variants = ref default_min_variants in
          let to_int x =
            match x with
            | `Int i -> Ok i
            | _ -> Error "expected integer"
          in
          parse_fields [Triplet ("min-variants", to_int, min_variants)] pairs
          >>| fun _ -> Enable {min_variants = !min_variants}
      | _ -> Error "expected auto peek conf"
    in
    let to_peek_conf x =
      let open Config.Peek in
      match x with
      | `Null -> Ok {filter = empty_regexp_pair; auto = Auto.default}
      | `Assoc pairs ->
          let filter = ref empty_regexp_pair in
          let auto = ref Auto.default in
          parse_fields
            [ Triplet ("filter", to_regexp_pair, filter)
            ; Triplet ("auto", to_peek_auto_conf, auto) ]
            pairs
          >>| fun _ -> {filter = !filter; auto = !auto}
      | _ -> Error "expected peek conf"
    in
    let config =
      let open Config in
      match json with
      | `Assoc pairs ->
          let debug = ref false in
          let memoize = ref empty_regexp_pair in
          let trace = ref empty_regexp_pair in
          let peek =
            ref Peek.{filter = empty_regexp_pair; auto = Peek.Auto.default}
          in
          parse_fields
            [ Triplet ("memoize", to_regexp_pair, memoize)
            ; Triplet ("trace", to_regexp_pair, trace)
            ; Triplet ("peek", to_peek_conf, peek)
            ; Triplet ("debug", to_bool, debug) ]
            pairs
          >>| fun _ ->
          {memoize = !memoize; trace = !trace; peek = !peek; debug = !debug}
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

  let to_hex_string c =
    let to_hex_char c =
      if c < 10 then Char.of_int_exn @@ (Char.to_int '0' + c)
      else Char.of_int_exn @@ (Char.to_int 'a' + c - 10)
    in
    String.of_char_list ['x'; to_hex_char (c lsr 4); to_hex_char (c land 0xF)]
end

module Make (Basic : COMB_BASE) :
  COMB
    with type 'a t = 'a Basic.t
     and type 'a Simple.t = 'a Basic.Simple.t
     and module Conf = Basic.Conf = struct
  include Basic

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
      fix
      @@ fun tail ->
      mapping (fun cont tail prev ->
          match tail with
          | None -> cont prev
          | Some tail -> tail (cont prev))
      <*> cont <*> opt tail
    in
    mapping (fun x tail ->
        match tail with
        | None -> x
        | Some tail -> tail x)
    <*> nil <*> opt tail

  let fold_left_cont_0_1 nil cont =
    mapping (fun x cont ->
        match cont with
        | None -> x
        | Some cont -> cont x)
    <*> nil <*> opt cont

  let ( && ) v f = mapping (fun v f -> f v) <*> v <*> f

  let loc p =
    mapping (fun p1 x p2 -> Location.mkloc x @@ loc_mk p1 p2)
    <*> pos <*> p <*> pos_end

  let loc_of p = mapping loc_mk <*> pos << p <*> pos_end

  let with_loc p =
    mapping (fun p1 f p2 -> f (loc_mk p1 p2)) <*> pos <*> p <*> pos

  let memoid2id = Hashtbl.create (module Int)
  let id2memoid : (int, int) Hashtbl.t = Hashtbl.create (module Int)
  let name2id = Hashtbl.create (module String)
  let id2name = Hashtbl.create (module Int)

  let named (name : string) (p : 'a t) =
    let p =
      match
        ( Utils.check_string__pair Conf.config.memoize name
        , Utils.check_string__pair Conf.config.trace name )
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
      match Hashtbl.find id2name id with
      | Some x -> x
      | None -> "_unnamed_")

  let name_of p = name_of_id @@ id p

  let choice ?name (ps : 'a t list) =
    let open Base in
    let auto_peek_border =
      let open Config.Peek.Auto in
      match Conf.config.peek.auto with
      | Disable -> None
      | Enable {min_variants} -> Some min_variants
    in
    let rec loop = function
      | [] -> failwith "check usage"
      | [x] ->
          ( x
          , if not_empty x then Some (0, Array.create ~len:first_size_max 0)
            else None )
      | x :: xs -> (
          let tail, variants = loop xs in
          let p = x <|> tail in
          match (variants, not_empty x) with
          | Some (last_set, sets), true ->
              let set_map = Hashtbl.create (module Int) in
              let new_last = ref last_set in
              first_iter x ~f:(fun i ->
                  let old_set = sets.(i) in
                  let new_set =
                    Hashtbl.find_or_add set_map old_set ~default:(fun () ->
                        let x = !new_last + 1 in
                        new_last := x;
                        x)
                  in
                  sets.(i) <- new_set);
              (p, Some (!new_last, sets))
          | _ -> (p, None))
    in
    let alteration, variants = loop ps in
    let variants =
      Option.map variants ~f:(fun (_, arr) ->
          let set = Hash_set.create (module Int) in
          Array.iter arr ~f:(Hash_set.add set);
          Hash_set.length set)
    in
    if Option.is_some name && Option.is_none variants then
      failwith "choice: named empty";
    let variant_cond =
      let cond = Option.map2 ~f:( >= ) variants auto_peek_border in
      Option.value ~default:false cond
    in
    let accept_cond =
      name
      |> Option.map ~f:(fun name ->
             Utils.check_string Conf.config.peek.filter.accept name)
      |> Option.value ~default:false
    in
    let decline_cond =
      name
      |> Option.map ~f:(fun name ->
             Utils.check_string Conf.config.peek.filter.decline name)
      |> Option.value ~default:false
    in
    if
      Base.(
        Conf.config.debug && accept_cond && (not decline_cond)
        && not variant_cond)
    then
      Caml.Printf.eprintf "choice: forced peek for %s\n"
        (Option.value ~default:"__unnamed__" name);
    if Base.(Conf.config.debug && decline_cond && variant_cond) then
      Caml.Printf.eprintf "choice: forced alteration for %s\n"
        (Option.value ~default:"__unnamed__" name);
    let p =
      if (accept_cond || variant_cond) && not decline_cond then peek_first ps
      else alteration
    in
    let p =
      match name with
      | Some name -> named name p
      | None -> p
    in
    p
end
