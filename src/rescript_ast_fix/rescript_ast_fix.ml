open Parsetree

let rec fix_module_type_desc = function
    | Pmty_signature s -> Pmty_signature (fix_signature s)
    | x -> x

and fix_module_type_declaration x = match x.pmtd_type with
    | None -> x
    | Some typ -> {x with pmtd_type = Some {typ with pmty_desc = fix_module_type_desc typ.pmty_desc}}

and fix_signature_item ({psig_desc; psig_loc} as item) =
    let psig_desc = match psig_desc with
        | Psig_modtype x ->
            let x = fix_module_type_declaration x in
            Psig_modtype {x with pmtd_loc = psig_loc}
        | x -> x
    in
    {item with psig_desc}

and fix_signature list = List.map fix_signature_item list
