open Parsetree

let rec fix_module_type_desc = function
    | Pmty_signature s -> Pmty_signature (fix_signature s)
    | x -> x

and fix_module_type_declaration x = match x.pmtd_type with
    | None -> x
    | Some typ -> {x with pmtd_type = Some {typ with pmty_desc = fix_module_type_desc typ.pmty_desc}}

and fix_signature_item ({psig_desc; psig_loc} as item) =
    match psig_desc with
    | Psig_modtype x ->
        let x = fix_module_type_declaration x in
        {item with psig_desc = Psig_modtype {x with pmtd_loc = psig_loc}}
    | Psig_exception x ->
        {item with psig_desc = Psig_exception {x with pext_loc = psig_loc}}
    | Psig_value x ->
        let x = fix_value x in
        {item with psig_desc = Psig_value x}
    | _ -> item

and fix_signature list = List.map fix_signature_item list

and fix_value x =
    {x with pval_type = fix_core_type x.pval_type}

and fix_core_type x =
    match x.ptyp_desc with
    | Ptyp_arrow (label, t1, t2) ->
        {x with ptyp_desc = Ptyp_arrow (label, fix_core_type t1, fix_core_type t2)}
    | Ptyp_constr (ident, _list) ->
        (match ident.txt with
        | Longident.Lident "unit" -> {x with ptyp_loc = ident.loc}
        | _ -> x
        )
    | _ -> x
