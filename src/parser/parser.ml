open Base
open Basic
open Parsetree
open Sigs
open Ast_helper
open Asttypes

module Make (APos: APOS) = struct
    module Utils = Parser_utils.Make(APos)

    open Utils
    open APos

    type parsers =
        {
            signature : signature parser;
            structure : structure parser;
            payload : payload parser;

            modexpr : (module MODEXPR);
        }

    let parsers = fix_poly @@ fun getter ->
        let module Constant = Parser_constant.Make(APos) in
        let open Constant in
        let open Utils in

        let payload = getter.get @@ fun x -> x.payload in
        let attrubute_id = take_while1 (fun x -> Caml.(||) (identifier's_character x) (Char.equal x '.')) in

        let id_payload_pair start =
            mapping t2
            +loc(s start >> attrubute_id) +payload
        in

        let module Core: CORE =
            struct
                let signature = getter.get @@ fun x -> x.signature
                let structure = getter.get @@ fun x -> x.structure

                let attribute = named "attribute" @@ id_payload_pair "@"
                let extension = named "extension" @@ id_payload_pair "%"

                let attrs_ =
                    memo & named "attrs" &
                    seq ~sep:ng ~trail attribute
                let attrs1_ =
                    named "attrs1" @@
                    seq ~n:1 ~sep:ng ~trail attribute

                let pat_attrs p =
                    mapping begin fun loc_start attrs x ->
                        match attrs with
                        | [] -> x
                        | _ ->
                            {x with
                                ppat_attributes = x.ppat_attributes @ attrs;
                                ppat_loc = {x.ppat_loc with loc_start}
                            }
                    end
                    +pos +attrs_ +p

                let exp_attrs p =
                    mapping begin fun loc_start attrs x ->
                        match attrs with
                        | [] -> x
                        | _ ->
                            {x with
                                pexp_attributes = x.pexp_attributes @ attrs;
                                pexp_loc = {x.pexp_loc with loc_start}
                            }
                    end
                    +pos +attrs_ +p

                let mod_attrs p =
                    mapping begin fun loc_start attrs x ->
                        match attrs with
                        | [] -> x
                        | _ ->
                            {x with
                                pmod_attributes = x.pmod_attributes @ attrs;
                                pmod_loc = {x.pmod_loc with loc_start}
                            }
                    end
                    +pos +attrs_ +p

                let mty_attrs p =
                    mapping begin fun loc_start attrs x ->
                        match attrs with
                        | [] -> x
                        | _ ->
                            {x with
                                pmty_attributes = x.pmty_attributes @ attrs;
                                pmty_loc = {x.pmty_loc with loc_start}
                            }
                    end
                    +pos +attrs_ +p

                let typ_attrs p =
                    mapping begin fun loc_start attrs x ->
                        match attrs with
                        | [] -> x
                        | _ ->
                            {x with
                                ptyp_attributes = x.ptyp_attributes @ attrs;
                                ptyp_loc = {x.ptyp_loc with loc_start}
                            }
                    end
                    +pos +attrs_ +p

                let variant_tag =
                    s"#"
                    >>
                    (ident <|> string_raw <|> take_while1 (function '0'..'9' -> true | _ -> false))
            end
        in

        let module Type = Parser_type.Make (APos) (Utils) (Constant) (Core) in
        let module Pattern = Parser_pattern.Make (APos) (Utils) (Constant) (Core) (Type) in

        let open Core in
        let open Type in
        let open Pattern in

        let payload =
            named "payload" begin
                    parens & mapping (fun x -> PPat (x, None))
                    -s"?" -ng +pattern

                ||  parens & mapping (fun x -> PSig x)
                    -s":" -ng -k"sig" +signature

                ||  parens & mapping (fun x -> PTyp x)
                    -s":" -ng +core_type

                ||  parens & mapping (fun x -> PStr x)
                    +structure

                ||  return @@ PStr []
            end
        in

        let module Modexpr = struct
            let modexpr = getter.get @@ fun x -> let (module M) = x.modexpr in M.modexpr
            let modexpr_constrainted = getter.get @@ fun x -> let (module M) = x.modexpr in M.modexpr_constrainted
        end in

        let module Expression =
            Parser_expression.Make
                (APos) (Utils) (Constant)
                (Core) (Type) (Pattern) (Modexpr)
        in

        let module Modtype = Parser_modtype.Make
            (APos) (Utils) (Core) (Type) (Expression) (Modexpr)
        in
        let module Modexpr = Parser_modexpr.Make
            (APos) (Utils) (Core) (Type) (Expression) (Modtype)
        in

        let open Modtype in
        let open Modexpr in
        let open Expression in

        let module_extension =
            named "module extension" @@
            id_payload_pair "%%"
        in

        let module_attribute =
            named "module attribute" @@
            id_payload_pair "@@"
        in

        let open_ =
            named "open" &
            with_loc & mapping begin fun attrs override name loc ->
                Opn.mk ~loc ~attrs ?docs:None ?override name
            end
            +attrs_ -k"open" +opt(s"!" >>$ Override) -ng +loc u_longident
        in

        let exception_ =
            named "top:exception" &
            mapping (fun attrs constr -> {constr with pext_attributes = constr.pext_attributes @ attrs} )
            +attrs_ -k"exception" -ng +type_extension_constructor
        in

        let include_ x =
            with_loc & hlp_a (Incl.mk ?docs:None)
            +attrs_ -k"include" -ng +x
        in

        let modtype_base =
            named "top:modtype" & with_loc & mapping
                (fun attrs name typ loc -> Mtd.mk ~attrs ~loc ~typ name)
            +attrs_ -k"module" -ng -k"type" -ng +loc ident -ng -s"=" -ng +modtype_with
        in

        let type_ mk =
            let first =
                mapping begin fun attrs eflag rec_flag decl ->
                    let decl =
                        match eflag with
                        | None -> decl
                        | Some loc -> tdecl_add_attr "genType" ~loc decl
                    in
                    rec_flag, {decl with ptype_attributes = attrs @ decl.ptype_attributes}
                end
                +attrs_ +opt(loc_of @@ k"export" << ng) -k"type" -ng
                +(((k"rec" >> ng >>$ Recursive) <|> (k"nonrec" >> ng >>$ Nonrecursive) <|> return Nonrecursive))
                +type_declaration
            in

            let other =
                mapping begin fun attrs eflag decl ->
                    let decl =
                        match eflag with
                        | None -> decl
                        | Some loc -> tdecl_add_attr "genType" ~loc decl
                    in
                    {decl with ptype_attributes = attrs @ decl.ptype_attributes}
                end
                +attrs_ -k"and" -ng +opt(loc_of @@ k"export" << ng) +type_declaration
            in

            with_del & mapping begin fun (rec_flag, hd) tail loc ->
                mk ?loc:(Some loc) rec_flag (hd :: tail)
            end
            +first +seq (ng >> other)
        in

        let external_ =
            named "external" begin
                with_loc & mapping begin fun attrs name typ prim loc ->
                    Val.mk ~loc ~attrs ?docs:None ~prim name typ
                end
                +attrs_ -k"external" -ng +loc l_ident -ng -s":" -ng +core_type_poly
                -ng -s"=" +seq ~n:1 (ng >> string_raw)
            end
        in

        let module_decl =
            named "module decl" begin
                let module_alias =
                    with_loc & hlp Mty.alias
                    +loc u_longident
                in

                mapping (fun n t attrs loc -> Md.mk ~loc ~attrs n t)
                +loc u_ident -ng
                +(
                        s":" >> ng >> modtype_with
                    ||  s"=" >> ng >> module_alias
                )
            end
        in

        let module_ =
            named "module" & with_loc &
            attrs_ && k"module" >> ng >> module_decl
        in

        let module_rec =
            named "module rec" begin
                let first =
                    with_loc & attrs_ &&
                    k"module" >> ng >> k"rec" >> ng >> module_decl
                in
                let other =
                    with_loc & attrs_ &&
                    k"and" >> ng >> module_decl
                in

                with_loc & mapping (fun hd tail loc -> Sig.rec_module ~loc (hd :: tail))
                +first +seq (ng >> other)
            end
        in

        let import =
            named "import" begin
                let item =
                    with_loc & mapping begin fun name alias typ loc attrs ->
                        let alias = Option.value alias ~default:name in
                        Val.mk ~loc ~attrs ~prim:[name.txt] alias typ
                    end
                    +loc l_ident -ng +opt(k"as" >> ng >> loc l_ident << ng) -s":" -ng +core_type_poly
                in

                let list = braces & seq ~n:1 item ~sep in

                let scope =
                    seq ~n:1 ~sep:(ng-s"."-ng)
                    (
                        with_loc & mapping (fun x loc -> Exp.constant ~loc (Const.string x))
                        +ident
                    )
                in

                    with_loc & mapping begin fun attrs list name loc ->
                        let attr = Location.mkloc "genType.import" name.loc, PStr [Str.eval @@ Exp.constant @@ Const.string name.txt] in
                        let list = List.map ~f:(fun x -> x [attr]) list in
                        let str = List.map ~f:Str.primitive list in
                        let mod_ = Mod.structure str in

                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +list -ng -k"from" -ng +loc string_raw

                ||  with_loc & mapping begin fun attrs item name loc ->
                        let attr =
                            Location.mkloc "genType.import" name.loc,
                            PStr [Str.eval @@ Exp.tuple [Exp.constant @@ Const.string name.txt; Exp.constant @@ Const.string "default"]]
                        in
                        let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +item -ng -k"from" -ng +loc string_raw

                ||  with_loc & mapping begin fun attrs list names loc ->
                        let attr =
                            [ Location.mknoloc "val", PStr []
                            ; Location.mkloc "scope" names.loc,
                                PStr [Str.eval @@ match names.txt with [x] -> x | xs -> Exp.tuple xs]
                            ]
                        in
                        let list = List.map list ~f:(fun x -> x attr) in
                        let str = List.map ~f:Str.primitive list in
                        let mod_ = Mod.structure str in
                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +list -ng -k"from" -ng +loc scope

                ||  with_loc & mapping begin fun attrs item names loc ->
                        let attr =
                            [ Location.mknoloc "val", PStr []
                            ; Location.mkloc "scope" names.loc,
                                PStr [Str.eval @@ match names.txt with [x] -> x | xs -> Exp.tuple xs]
                            ]
                        in
                        let mod_ = Mod.structure [Str.primitive @@ item attr] in
                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +item -ng -k"from" -ng +loc scope

                ||  with_loc & mapping begin fun attrs list loc ->
                        let attr = Location.mknoloc "val", PStr [] in
                        let list = List.map ~f:(fun x -> x [attr]) list in
                        let str = List.map ~f:Str.primitive list in
                        let mod_ = Mod.structure str in
                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +list

                ||  with_loc & mapping begin fun attrs item loc ->
                        let attr = Location.mknoloc "val", PStr [] in
                        let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                        Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_
                    end
                    +attrs_ -k"import" -ng +item
            end
        in

        let signature =
            named "sig" begin
                seq ~sep:ng
                (
                    peek_first
                    [
                        with_del & na_hlp Sig.attribute
                        +module_attribute
                    ; type_ Sig.type_
                    ; module_rec
                    ;
                        with_del & hlp_a Sig.extension
                        +attrs_ +module_extension
                    ;
                        named "sig:exception" & with_del & na_hlp Sig.exception_
                        +exception_
                    ;
                        with_del & na_hlp Sig.value
                        +external_
                    ;
                        named "sig:include" & with_del & na_hlp Sig.include_
                        +include_ modtype_with
                    ;
                        named "sig:value" & with_del & na_hlp Sig.value
                        +(
                            with_loc & hlp2_a (Val.mk ?docs:None ?prim:None)
                            +attrs_ -k"let" -ng +loc l_ident -ng -s":" -ng +core_type_poly
                        )
                    ;
                        named "sig:modtype" & with_del & na_hlp Sig.modtype
                        +(
                                modtype_base

                            ||  with_loc & hlp_a (Mtd.mk ?docs:None ?text:None ?typ:None)
                                +attrs_ -k"module" -ng -k"type" -ng +loc u_ident
                        )
                    ;
                        with_del & na_hlp Sig.module_
                        +module_
                    ;
                        with_del & na_hlp Sig.open_
                        +open_
                    ;
                        with_del & na_hlp Sig.type_extension
                        +type_extension
                    ]
                )
            end
        in

        let value_str =
            named "str:value" begin
                let lat = k"type" >> seq ~n:1 (ng >> loc l_ident) << ng << s"." in

                let with_lat =
                    named "str:value:vb:lat" begin
                        mapping begin fun p1 attrs pat types typ expr vb_attrs vb_loc ->
                            let mapping =
                                { Parsetree_mapping.default with
                                    core_type_desc =
                                        { Parsetree_mapping.default.core_type_desc with
                                            constr = fun t a ->
                                                match a with
                                                | [] ->
                                                    let rec loop =
                                                        function
                                                        | [] -> Ptyp_constr (t, a)
                                                        | x::xs ->
                                                            match t.txt with
                                                            | Longident.Lident t when String.equal t x.txt ->
                                                                Ptyp_var x.txt
                                                            | _ -> loop xs
                                                    in
                                                    loop types
                                                | _ -> Ptyp_constr (t, a)
                                        }
                                }
                            in

                            let typ_pat = Parsetree_mapping.core_type mapping typ in
                            let typ_pat = Typ.poly types typ_pat in

                            let pat = Pat.constraint_ ~loc:{typ.ptyp_loc with loc_start = p1} ~attrs pat typ_pat in

                            let expr =
                                List.fold_right types
                                ~f:Exp.newtype
                                ~init:(Exp.constraint_ expr typ)
                            in
                            Vb.mk ~loc:vb_loc ~attrs:vb_attrs pat expr
                        end
                        +pos +attrs_ +pattern -ng -s":" -ng +lat -ng +core_type_atom
                        -ng -s"=" -ng +expression_arrow
                    end
                in

                let helper =
                        with_lat

                    ||  mapping begin fun pattern expr attrs loc ->
                            Vb.mk ~loc ~attrs pattern expr
                        end
                        +pattern_poly_constrainted -ng -s"=" -ng +expression_arrow
                in

                let first =
                        with_loc & mapping begin fun attrs eflag rec_flag decl loc ->
                            let decl =
                                match eflag with
                                | None -> decl attrs
                                | Some loc -> decl (Hc.attr "genType" ~loc :: attrs)
                            in
                            match rec_flag with
                            | None -> Nonrecursive, decl loc
                            | _ -> Recursive, decl loc
                        end
                        +attrs_ +opt(loc_of(k"export")-ng) -k"let" -ng +opt(k"rec"-ng) +helper

                    ||  with_loc & mapping begin fun attrs loc decl decl_loc ->
                            Nonrecursive, decl (Hc.attr "genType" ~loc :: attrs) decl_loc
                        end
                        +attrs_ +loc_of(k"export") -ng +helper
                in
                let other =
                    mapping begin fun attrs p1 eflag x p2 ->
                        let decl =
                            match eflag with
                            | None -> x attrs
                            | Some loc -> x (Hc.attr "genType" ~loc :: attrs)
                        in
                        decl @@ make_location p1 p2
                    end
                    +attrs_ -k"and" -ng +pos +opt(loc_of(k"export")-ng) +helper +pos
                in

                with_del & mapping begin fun (rec_flag, first) others loc ->
                    Str.value ~loc rec_flag (first :: others)
                end
                +first +seq (ng >> other)
            end
        in

        let module_binding_helper =
            mapping begin fun a t b attrs loc ->
                let b = Option.value ~default:b (Option.map ~f:(fun t -> Mod.constraint_ b t) t) in
                Mb.mk ~loc ~attrs a b
            end
            +loc u_ident -ng +opt(s":" >> ng >> modtype << ng) -o"=" -ng +modexpr
        in

        let module_binding =
            named "str:mb" begin
                with_loc &
                attrs_ && k"module" >> ng >> module_binding_helper
            end
        in

        let rec_module_binding =
            let first =
                with_loc &
                attrs_ && k"module" >> ng >> k"rec" >> ng >> module_binding_helper
            in
            let other =
                with_loc &
                attrs_ && k"and" >> ng >> module_binding_helper
            in

            named "str:mb:rec" begin
                mapping cons
                +first +seq (ng >> other)
            end
        in

        let structure =
            memo & named "str" &
            seq ~sep:ng
            (
                peek_first
                [
                    with_del & na_hlp Str.attribute
                    +module_attribute
                ;
                    with_del & na_hlp Str.rec_module
                    +rec_module_binding
                ; value_str
                ;
                    with_del & hlp_a Str.extension
                    +attrs_ +module_extension
                ;
                    named "str:exception" & with_del & na_hlp Str.exception_
                    +exception_
                ;
                    with_del & na_hlp Str.primitive
                    +external_
                ;
                    with_del & na_hlp Str.include_
                    +import
                ;
                    named "str:include" & with_del & na_hlp Str.include_
                    +include_ modexpr
                ;
                    with_del & na_hlp Str.module_
                    +module_binding
                ;
                    named "str:modtype" & with_del & na_hlp Str.modtype
                    +modtype_base
                ;
                    with_del & na_hlp Str.open_
                    +open_
                ;
                    with_del & na_hlp Str.type_extension
                    +type_extension
                ; type_ Str.type_
                ;
                    with_del & hlp_a Str.eval
                    +attrs_ +expression_arrow
                ]
            )
        in

        {
            signature;
            structure;
            payload;

            modexpr = (module Modexpr);
        }

    let with_print p = p
        (*Res_diagnostics.printReport state.diagnostics "TODO";*)

    let parse p state ~src ~filename =
        parse_string p state ~filename src

    let parse_interface = parse (ng >> parsers.signature << ng) State.default
    let parse_implementation = parse (ng >> parsers.structure << ng) State.default
end
