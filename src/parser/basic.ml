open Base
open Parsetree

module LogElement = struct
    type t =
    | Comment of Res_comment.t
    | Diagnostics of Res_diagnostics.t
end

module Charset = Angstrom_pos.Charset

type 'a parser = 'a Angstrom_pos.Parser.t

type 'a parse_result = ('a, Res_diagnostics.t list) Res_driver.parseResult

module Opt = struct
    let set x =
        function
        | Some _ -> Some x
        | None -> None
end

let mkloc ?loc a =
    match loc with
    | None -> Location.mknoloc a
    | Some loc -> Location.mkloc a loc

module Hc = struct
    let attr ?loc s = mkloc ?loc s, PStr []

    let lid =
        let rec loop acc =
            function
            | [] -> acc
            | x::xs -> loop (Longident.Ldot (acc, x)) xs
        in

        function
        | [] -> failwith ""
        | x::xs -> loop (Longident.Lident x) xs

    let expr_id ?loc ?attrs xs =
        let lid = lid xs in
        Ast_helper.Exp.ident ?loc ?attrs (mkloc ?loc lid)

    let unit_expr loc = Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
end

let make_list_helper ~constr ~tuple ~get_loc seq ext loc =
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

    loop (fun loc -> constr ?loc:(Some loc) ?attrs:None) seq

let pat_loc p loc = {p with ppat_loc = loc}
let pat_add_attr ?loc n x = {x with ppat_attributes = Hc.attr ?loc n :: x.ppat_attributes}

let exp_loc p loc = {p with pexp_loc = loc}
let exp_add_attr ?loc n x = {x with pexp_attributes = Hc.attr ?loc n :: x.pexp_attributes}

let mod_loc p loc = {p with pmod_loc = loc}
let mod_add_attr ?loc n x = {x with pmod_attributes = Hc.attr ?loc n :: x.pmod_attributes}

let mty_loc p loc = {p with pmty_loc = loc}
let mty_add_attr ?loc n x = {x with pmty_attributes = Hc.attr ?loc n :: x.pmty_attributes}

let typ_loc p loc = {p with ptyp_loc = loc}
let typ_add_attr ?loc n x = {x with ptyp_attributes = Hc.attr ?loc n :: x.ptyp_attributes}

let tdecl_add_attr ?loc n x = {x with ptype_attributes = Hc.attr ?loc n :: x.ptype_attributes}
