open Base

include Sigs

let loc_mk loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
let loc_comb loc1 loc2 = loc_mk loc1.Location.loc_start loc2.Location.loc_end
let (&) = (@@)

module Utils = struct
    let mk_regexp xs =
        let rec loop =
            function
            | [] -> ""
            | [x] -> "\\(" ^ x ^ "\\)"
            | x::xs -> "\\(" ^ x ^ "\\)\\|" ^ loop xs
        in
        Str.regexp @@ loop xs
    let empty_regexp = mk_regexp []

    module MakeConf (Log : CONF_LOG) (Params : sig val filename : string option;; val src : string end)
            : CONF with module Log = Log
            = struct

        open Yojson.Safe
        open Util

        module Log = Log

        let json = from_string ?fname:Params.filename Params.src
        let memoize =
            match member "memoize" json with
            | `Null -> empty_regexp
            | x -> to_list x |> List.map ~f:to_string |> mk_regexp
        let trace =
            match member "trace" json with
            | `Null -> empty_regexp
            | x -> to_list x |> List.map ~f:to_string |> mk_regexp
        let peek =
            match member "peek" json with
            | `Null -> empty_regexp
            | x -> to_list x |> List.map ~f:to_string |> mk_regexp
    end

    let check_string regexp str =
        Str.string_match regexp str 0
        &&
        Str.match_end () = String.length str
end


module Make
        (Basic : COMB_BASE) (Conf : CONF with type Log.elem = Basic.log_elem)
        : COMB
            with type 'a t = 'a Basic.t
            and type 'a Simple.t = 'a Basic.Simple.t
            and type log_elem = Basic.log_elem
        = struct
    include Basic

    let (+) = (<*>)
    let (-) = (<<)
    let mapping = return

    let fold_left_0_n ~f nil p =
        let hlp = nil >> seq p in
        modify hlp
            ~simple:begin
                let open Simple in
                let rec loop acc =
                    (simple p >>= fun x -> loop @@ f acc x)
                    <|>
                    return acc
                in
                simple nil >>= loop
            end

    let fold_left_0_1 ~f nil p =
            (mapping f <*> nil <*> p)
        <|> nil

    let fold_left_cont_0_n nil cont =
        let tail = fix @@ fun tail ->
            mapping begin fun cont tail ->
                fun prev ->
                    match tail with
                    | None -> cont prev
                    | Some tail -> tail (cont prev)
            end
            +cont +opt(tail)
        in

        mapping begin fun x tail ->
            match tail with
            | None -> x
            | Some tail -> tail x
        end
        +nil +opt(tail)

    let fold_left_cont_0_1 nil cont =
        mapping begin fun x cont ->
            match cont with
            | None -> x
            | Some cont -> cont x
        end
        +nil +opt(cont)

        let (&&) v f =
        mapping (fun v f -> f v) <*> v <*> f

    let loc p =
        mapping begin fun p1 x p2 -> Location.mkloc x @@ loc_mk p1 p2 end
        +pos +p +pos_end
    let loc_of p =
        mapping loc_mk
        +pos -p +pos_end
    let with_loc p =
        mapping begin fun p1 f p2 -> f (loc_mk p1 p2) end
        +pos +p +pos

    let memoid2id = Hashtbl.create (module Int)
    let id2memoid: (int, int) Hashtbl.t = Hashtbl.create (module Int)
    let name2id = Hashtbl.create (module String)
    let id2name = Hashtbl.create (module Int)

    let named (name: string) (p: 'a t) =
        let p =
            match Utils.check_string Conf.memoize name, Utils.check_string Conf.trace name with
            | false, false ->
                touch p
            | true, false ->
                let p' = memo p in
                Hashtbl.add_exn memoid2id ~key:(id p') ~data:(id p);
                Hashtbl.add_exn id2memoid ~key:(id p) ~data:(id p');
                p'
            | false, true ->
                traced p
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
        | None ->
            begin match Hashtbl.find id2name id with
            | Some x -> x
            | None -> "_unnamed_"
            end

    let name_of p = name_of_id @@ id p

    let choice ?name ps =
        match name with
        | None ->
            let rec loop =
                function
                | [] -> failwith "check usage"
                | [x] -> x
                | x :: xs -> x <|> loop xs
            in
            loop ps
        | Some name ->
            let p =
                if Utils.check_string Conf.peek name then
                    peek_first ps
                else
                    let check x =
                        if not_empty x then x else failwith "check usage"
                    in
                    let rec loop =
                        function
                        | [] -> failwith "check usage"
                        | [x] -> check x
                        | x :: xs -> check x <|> loop xs
                    in
                    loop ps
            in
            named name p
end
