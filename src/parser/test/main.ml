
let dump_loc_mapping =
    let open Pc_syntax.Parsetree_mapping in
    let open Parsetree in
    let open Location in

    let loc: 'a. 'a loc -> 'a loc = fun x -> {x with loc = none} in

    { default with
        signature_item  = (fun x -> {x with psig_loc = none});
        structure_item  = (fun x -> {x with pstr_loc = none});
        pattern         = (fun x -> {x with ppat_loc = none});
        pattern_desc    =
            { default.pattern_desc with
                var         = (fun x -> Ppat_var (loc x));
                alias       = (fun x n -> Ppat_alias (x, loc n));
                construct   = (fun x a -> Ppat_construct (loc x, a));
                record      = (fun x f -> Ppat_record (List.map (fun (n, x) -> loc n, x) x, f));
                type_       = (fun x -> Ppat_type (loc x));
                unpack      = (fun x -> Ppat_unpack (loc x));
                extension   = (fun (n, x) -> Ppat_extension (loc n, x));
                open_       = (fun x p -> Ppat_open (loc x, p));
            };
        expression       = (fun x -> {x with pexp_loc = none});
        expression_desc  =
            { default.expression_desc with
                construct    = (fun x a -> Pexp_construct (loc x, a));
                field        = (fun x f -> Pexp_field (x, loc f));
                ident        = (fun x -> Pexp_ident (loc x));
                letmodule    = (fun l x e -> Pexp_letmodule (loc l, x, e));
                new_         = (fun x -> Pexp_new (loc x));
                newtype      = (fun l x -> Pexp_newtype (loc l, x));
                open_        = (fun f n x -> Pexp_open (f, loc n, x));
                override     = (fun x -> Pexp_override (List.map (fun (m, e) -> loc m, e) x));
                record       = (fun x e -> Pexp_record (List.map (fun (m, e) -> loc m, e) x, e));
                send         = (fun x n -> Pexp_send (x, loc n));
                setfield     = (fun x n y -> Pexp_setfield (x, loc n, y));
                setinstvar   = (fun n x -> Pexp_setinstvar (loc n, x));
            };
        core_type        = (fun x -> {x with ptyp_loc = none});
        core_type_desc   =
            { default.core_type_desc with
                class_    = (fun n x -> Ptyp_class (loc n, x));
                constr    = (fun n x -> Ptyp_constr (loc n, x));
                poly      = (fun n x -> Ptyp_poly (List.map (fun n -> loc n) n, x));
            };
        module_expr      = (fun x -> {x with pmod_loc = none});
        module_expr_desc =
            { default.module_expr_desc with
                functor_ = (fun n t x -> Pmod_functor (loc n, t, x));
                ident    = (fun x -> Pmod_ident (loc x));
            };
        module_type      = (fun x -> {x with pmty_loc = none});
        module_type_desc =
            { default.module_type_desc with
                alias     = (fun x -> Pmty_alias (loc x));
                functor_  = (fun n x t -> Pmty_functor (loc n, x, t));
                ident     = (fun x -> Pmty_ident (loc x));
            };

        class_expr            = (fun x -> {x with pcl_loc = none});
        class_expr_desc       =
            { default.class_expr_desc with
                constr      = (fun n x -> Pcl_constr (loc n, x));
                open_       = (fun f n x -> Pcl_open (f, loc n, x));
            };
        class_field           = (fun x -> {x with pcf_loc = none});
        class_field_desc      =
            { default.class_field_desc with
                inherit_     = (fun f x n -> Pcf_inherit (f, x, Option.map loc n));
                method_      = (fun n f x -> Pcf_method (loc n, f, x));
                val_         = (fun n f x -> Pcf_val (loc n, f, x));
            };
        class_type            = (fun x -> {x with pcty_loc = none});
        class_type_desc       =
            { default.class_type_desc with
                constr    = (fun n x -> Pcty_constr (loc n, x));
                open_     = (fun f n x -> Pcty_open (f, loc n, x));
            };
        class_type_field      = (fun x -> {x with pctf_loc = none});
        class_type_field_desc =
            { default.class_type_field_desc with
                method_     = (fun n f1 f2 x -> Pctf_method (loc n, f1, f2, x));
                val_        = (fun n f1 f2 x -> Pctf_val (loc n, f1, f2, x));
            };

        with_constraint            =
            {
                type_     = (fun x y -> Pwith_type (loc x, y));
                module_   = (fun x y -> Pwith_module (loc x, loc y));
                typesubst = (fun x y -> Pwith_typesubst (loc x, y));
                modsubst  = (fun x y -> Pwith_modsubst (loc x, loc y));
            };
        object_field               =
            { default.object_field with
                tag      = (fun n a t -> Otag (loc n, a, t));
            };
        row_field                  =
            { default.row_field with
                tag      = (fun n a f l -> Rtag (loc n, a, f, l));
            };
        extension_constructor_kind =
            { default.extension_constructor_kind with
                rebind = (fun x -> Pext_rebind (loc x));
            };

        attribute               = (fun (n, x) -> loc n, x);
        extension               = (fun (n, x) -> loc n, x);
        class_description       = (fun x -> {x with pci_name = loc x.pci_name; pci_loc = none});
        class_type_declaration  = (fun x -> {x with pci_name = loc x.pci_name; pci_loc = none});
        extension_constructor   = (fun x -> {x with pext_name = loc x.pext_name; pext_loc = none});
        module_binding          = (fun x -> {x with pmb_name = loc x.pmb_name; pmb_loc = none});
        module_type_declaration = (fun x -> {x with pmtd_name = loc x.pmtd_name; pmtd_loc = none});
        module_declaration      = (fun x -> {x with pmd_name = loc x.pmd_name; pmd_loc = none});
        open_description        = (fun x -> {x with popen_lid = loc x.popen_lid; popen_loc = none});
        type_extension          = (fun x -> {x with ptyext_path = loc x.ptyext_path});
        type_declaration        =
            (fun x ->
                {x with
                    ptype_name = loc x.ptype_name;
                    ptype_cstrs = List.map (fun (x, y, _) -> x, y, none) x.ptype_cstrs;
                    ptype_loc = none;
                }
            );
        value_description       = (fun x -> {x with pval_name = loc x.pval_name; pval_loc = none});
        constructor_declaration = (fun x -> {x with pcd_name = loc x.pcd_name; pcd_loc = none});
        value_binding           = (fun x -> {x with pvb_loc = none});
        label_declaration       = (fun x -> {x with pld_loc = none});
        include_description     = (fun x -> {x with pincl_loc = none});
        include_declaration     = (fun x -> {x with pincl_loc = none});
    }

module Res = struct
    let signature file: (Parsetree.signature, string) result =
        Ok (
            Pc_syntax.Parsetree_mapping.signature dump_loc_mapping @@
            Res_driver.parse_interface file
        )

    let structure file: (Parsetree.structure, string) result =
        Ok (
            Pc_syntax.Parsetree_mapping.structure dump_loc_mapping @@
            Res_driver.parse_implementation file
        )
end

module Trace = Angstrom_pos.Trace.Traced
    (Pc_syntax.Basic.Angstrom)
    (struct let memo_spec = ["expression"] end)
module Pc = struct
    module Parser = Pc_syntax.Parser.Make(Trace)

    let signature file =
        Base.Result.map
            ~f:(Pc_syntax.Parsetree_mapping.signature dump_loc_mapping)
            (Parser.parse_interface file)
    let structure file =
        Base.Result.map
            ~f:(Pc_syntax.Parsetree_mapping.structure dump_loc_mapping)
            (Parser.parse_implementation file)
end

module type A = module type of Res

let file = ref ""
let parser = ref ((module Pc): (module A))
let simplified = ref false
let stats = ref ""
let anon_fun filename = file := filename

let speclist = [
    (
        "--parser",
        Arg.Symbol (["res"; "pc"],
            function
            | "res" -> parser := (module Res)
            | "pc" -> parser := (module Pc)
            | _ -> failwith "unreachable"
        ),
        ""
    );
    ("--simplified", Arg.Set simplified, "");
    ("--stats", Arg.Set_string stats, "");
]

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse) = !parser in

    begin if String.sub !file (String.length !file - 4) 4 = ".res" then
        let x = Parse.structure !file in
        begin match !stats with
        | "" -> ()
        | file ->
            Printf.fprintf (open_out @@ file ^ ".vis.html") "%s" (Trace.visualize ());
            Trace.print_stats ~filter:(fun ~count ~time -> count > 1. && time > 50.) (open_out @@ file ^ ".stats");
        end;

        match x with
        | Error x -> failwith x
        | Ok s ->
            if !simplified then
                Pprintast.structure (Format.std_formatter) s
            else
                Ast_show.Parsetree.pp_structure (Format.std_formatter) s
    else if String.sub !file (String.length !file - 5) 5 = ".resi" then
        let x = Parse.signature !file in
        begin match !stats with
        | "" -> ()
        | file ->
            Printf.fprintf (open_out @@ file ^ ".vis.html") "%s" (Trace.visualize ());
            Trace.print_stats ~filter:(fun ~count ~time -> count > 1. && time > 50.) (open_out @@ file ^ ".stats");
        end;

        match x with
        | Error x -> failwith x
        | Ok s ->
            if !simplified then
                Pprintast.signature (Format.std_formatter) s
            else
                Ast_show.Parsetree.pp_signature (Format.std_formatter) s
    else
        failwith !file
    end;
