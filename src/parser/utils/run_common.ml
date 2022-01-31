module ParseRes: Pc_syntax.Sigs.PARSE = struct
    let parse_interface ~src ~filename =
        let x = Res_parse_string.parse_interface ~src ~filename in
        Some x

    let parse_implementation ~src ~filename =
        let x = Res_parse_string.parse_implementation ~src ~filename in
        Some x
end

let mk_apos ?(peek=false) ?(memo=false) (): (module Pc_syntax.Sigs.APOS) =
    let module APos = Angstrom_pos.Make(Pc_syntax.State) in
    let (module APos: Pc_syntax.Sigs.APOS) =
        if peek then
            (module APos)
        else
            let module NotPeek = Angstrom_pos.Alt.MakeNotPeek(APos) in
            (module struct
                include APos
                include NotPeek
            end)
    in

    let (module APos: Pc_syntax.Sigs.APOS) =
        if memo then
            (module APos)
        else
            let module NotMemoized = Angstrom_pos.Alt.MakeNotMemoized(APos) in
            (module struct
                include APos
                include NotMemoized
            end)
    in

    (module APos: Pc_syntax.Sigs.APOS)

let mk_parse ?(peek=false) ?(memo=false) (): (module Pc_syntax.Sigs.PARSE) =
    let (module APos) = mk_apos ~peek ~memo () in
    (module Pc_syntax.Parser.Make(APos): Pc_syntax.Sigs.PARSE)


module type TRACED = module type of Angstrom_pos.Alt.MakeTraced(Angstrom_pos.Make(Pc_syntax.State))
module type MEAZURED = module type of Angstrom_pos.Alt.MakeMeasured(Angstrom_pos.Make(Pc_syntax.State))

let mk_traced ?(peek=false) () =
    let (module APos: Pc_syntax.Sigs.APOS) = mk_apos ~peek ~memo:true () in

    let module Traced = Angstrom_pos.Alt.MakeTraced(APos) in
    (module Traced: TRACED)
    ,
    (module APos: Pc_syntax.Sigs.APOS)
    ,
    (module Pc_syntax.Parser.Make
        (
            struct
                include APos
                include Traced
            end
        ): Pc_syntax.Sigs.PARSE
    )

let mk_measured ?(peek=false) () =
    let (module APos: Pc_syntax.Sigs.APOS) = mk_apos ~peek ~memo:true () in

    let module Measured = Angstrom_pos.Alt.MakeMeasured(APos) in
    (module Measured: MEAZURED)
    ,
    (module APos: Pc_syntax.Sigs.APOS)
    ,
    (module Pc_syntax.Parser.Make
        (
            struct
                include APos
                include Measured
            end
        ): Pc_syntax.Sigs.PARSE
    )


let input = ref ""
let output = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, ""
    ; "--output", Arg.Set_string output, ""
    ]

let read_file ~filename =
    Core_kernel.In_channel.read_all filename

let dump_loc_mapping =
    let open Pc_syntax.Parsetree_mapping in
    let open Parsetree in
    let open Location in

    let loc: 'a. 'a loc -> 'a loc = fun x -> {x with loc = none} in

    { default with
        signature_item  = (fun x -> {x with psig_loc = none});
        structure_item  = (fun x -> {x with pstr_loc = none});
        pattern         =
            begin fun x ->
                {x with
                    ppat_loc = none;
                    ppat_desc =
                        begin match x.ppat_desc with
                        | Ppat_var x            -> Ppat_var (loc x)
                        | Ppat_alias (x, n)     -> Ppat_alias (x, loc n)
                        | Ppat_construct (x, a) -> Ppat_construct (loc x, a)
                        | Ppat_record (x, f)    -> Ppat_record (List.map (fun (n, x) -> loc n, x) x, f)
                        | Ppat_type x           -> Ppat_type (loc x)
                        | Ppat_unpack x         -> Ppat_unpack (loc x)
                        | Ppat_extension (n, x) -> Ppat_extension (loc n, x)
                        | Ppat_open (x, p)      -> Ppat_open (loc x, p)
                        | x                     -> x
                        end;
                }
            end;
        expression       =
            begin fun x ->
                {x with
                    pexp_loc = none;
                    pexp_desc =
                        match x.pexp_desc with
                        | Pexp_construct (x, a)    -> Pexp_construct (loc x, a)
                        | Pexp_field (x, f)        -> Pexp_field (x, loc f)
                        | Pexp_ident x             -> Pexp_ident (loc x)
                        | Pexp_letmodule (l, x, e) -> Pexp_letmodule (loc l, x, e)
                        | Pexp_new x               -> Pexp_new (loc x)
                        | Pexp_newtype (l, x)      -> Pexp_newtype (loc l, x)
                        | Pexp_open (f, n, x)      -> Pexp_open (f, loc n, x)
                        | Pexp_override x          -> Pexp_override (List.map (fun (m, e) -> loc m, e) x)
                        | Pexp_record (x, e)       -> Pexp_record (List.map (fun (m, e) -> loc m, e) x, e)
                        | Pexp_send (x, n)         -> Pexp_send (x, loc n)
                        | Pexp_setfield (x, n, y)  -> Pexp_setfield (x, loc n, y)
                        | Pexp_setinstvar (n, x)   -> Pexp_setinstvar (loc n, x)
                        | x                        -> x
                }
            end;

        core_type        =
            begin fun x ->
                {x with
                    ptyp_loc = none;
                    ptyp_desc =
                    begin match x.ptyp_desc with
                    | Ptyp_class (n, x)  -> Ptyp_class (loc n, x)
                    | Ptyp_constr (n, x) -> Ptyp_constr (loc n, x)
                    | Ptyp_poly (n, x)   -> Ptyp_poly (List.map (fun n -> loc n) n, x)
                    | x                  -> x
                    end;
                }
            end;
        module_expr      =
            begin fun x ->
                {x with
                    pmod_loc = none;
                    pmod_desc =
                        begin match x.pmod_desc with
                        | Pmod_functor (n, t, x) -> Pmod_functor (loc n, t, x)
                        | Pmod_ident x           -> Pmod_ident (loc x)
                        | x                      -> x
                    end;
                }
            end;
        module_type      =
            begin fun x ->
                {x with
                    pmty_loc = none;
                    pmty_desc =
                        begin match x.pmty_desc with
                        | Pmty_alias x           -> Pmty_alias (loc x)
                        | Pmty_functor (n, x, t) -> Pmty_functor (loc n, x, t);
                        | Pmty_ident x           -> Pmty_ident (loc x)
                        | x                      -> x
                        end;
                }
            end;
        class_expr            =
            begin fun x ->
                {x with
                    pcl_loc = none;
                    pcl_desc =
                        match x.pcl_desc with
                        | Pcl_constr (n, x)  -> Pcl_constr (loc n, x)
                        | Pcl_open (f, n, x) -> Pcl_open (f, loc n, x)
                        | x                  -> x
                }
            end;
        class_field           =
            begin fun x ->
                {x with
                    pcf_loc = none;
                    pcf_desc =
                        match x.pcf_desc with
                        | Pcf_inherit (f, x, n) -> Pcf_inherit (f, x, match n with Some x -> Some (loc x) | None -> None)
                        | Pcf_method (n, f, x)  -> Pcf_method (loc n, f, x)
                        | Pcf_val (n, f, x)     -> Pcf_val (loc n, f, x)
                        | x                     -> x
                }
            end;
        class_type            =
            begin fun x ->
                {x with
                    pcty_loc = none;
                    pcty_desc =
                        match x.pcty_desc with
                        | Pcty_constr (n, x)  -> Pcty_constr (loc n, x)
                        | Pcty_open (f, n, x) -> Pcty_open (f, loc n, x)
                        | x                   -> x
                }
            end;
        class_type_field      =
            begin fun x ->
                {x with
                    pctf_loc = none;
                    pctf_desc =
                        match x.pctf_desc with
                        | Pctf_method (n, f1, f2, x) -> Pctf_method (loc n, f1, f2, x)
                        | Pctf_val (n, f1, f2, x)    -> Pctf_val (loc n, f1, f2, x)
                        | x                          -> x
                }
            end;
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
