open Basic
open APos
open Parsetree
open Sigs

module Make
        (Ext: Ext)
        = struct
    module Utils = Parser_utils.Make(Ext.Named)
    open Utils

    type parsers =
        {
            signature : signature parser;
            structure : structure parser;
            payload : payload parser;

            modexpr : (module MODEXPR);
        }

    let
        {
            signature;
            structure;
            _
        }
    = fix_poly @@ fun getter ->
        let open Ast_helper in
        let open Asttypes in
        let open Ext in

        let module Constant = Parser_constant.Make(Ext)(Utils) in
        let open Utils in

        let payload = getter.get @@ fun x -> x.payload in
        let attrubute_id = take_while1 Pervasives.(fun x -> identifier's_character x || x = '.') in

        let id_payload_pair start =
            mapping t2
            <*> loc (s start >> attrubute_id) <*> payload
        in

        let module Core: CORE =
            struct
                let signature = getter.get @@ fun x -> x.signature
                let structure = getter.get @@ fun x -> x.structure

                let attribute = id_payload_pair "@"
                let extension = id_payload_pair "%"

                let attrs_ =
                    Named.p "attrs" @@
                    seq 0 ~sep:ng ~trail attribute

                let use mk =
                    mapping begin fun p1 hlp p2 -> apply hlp p1 p2 end
                    <*> pos <*> mk <*> pos

                let use_na mk =
                    mapping begin fun p1 hlp p2 -> hlp ~p1 ~p2 end
                    <*> pos <*> mk <*> pos

                let use_del mk =
                    mapping begin fun p1 hlp p2 -> apply hlp p1 p2 end
                    <*> pos <*> mk <*> del_pos

                let use_na_del mk =
                    mapping begin fun p1 hlp p2 -> hlp ~p1 ~p2 end
                    <*> pos <*> mk <*> del_pos

                let add_attrs =
                    fun mk ->
                        mapping begin fun attrs_tail (hlp, attrs) ->
                            let attrs_tail = Base.Option.value attrs_tail ~default:[] in
                            (hlp, attrs @ attrs_tail)
                        end
                        <*>? attrs_ <*> mk

                let set_p1 p =
                    mapping begin fun p1 (hlp, attrs) ->
                        (fun ~p1:_ -> hlp ~p1), attrs
                    end
                    <*> pos <*> p

                let variant_tag =
                    s"#"
                    >>
                    (ident <|> Constant.String.string <|> take_while1 (function '0'..'9' -> true | _ -> false))
            end
        in

        let module Type = Parser_type.Make (Ext) (Utils) (Constant) (Core) in
        let module Pattern = Parser_pattern.Make (Ext) (Utils) (Constant) (Core) (Type) in

        let open Core in
        let open Type in
        let open Pattern in

        let payload =
            Named.p "payload" begin
                    (s"(" >> -s"?" >> -use pattern << -s")" >>| fun x -> PPat (x, None))
                <|> (s"(" >> -s":" >> -k"sig" >> signature << -s")" >>| fun x -> PSig x)
                <|> (s"(" >> -s":" >> -use core_type  << -s")" >>| fun x -> PTyp x)
                <|> (s"(" >> structure << -s")" >>| fun x -> PStr x)
                <|> return @@ PStr []
            end
        in

        let module Modexpr = struct
            let modexpr = getter.get @@ fun x -> let (module M) = x.modexpr in M.modexpr
            let modexpr_constrainted = getter.get @@ fun x -> let (module M) = x.modexpr in M.modexpr_constrainted
            let modtype = getter.get @@ fun x -> let (module M) = x.modexpr in M.modtype
            let modtype_functor = getter.get @@ fun x -> let (module M) = x.modexpr in M.modtype_functor
            let modtype_with = getter.get @@ fun x -> let (module M) = x.modexpr in M.modtype_with
        end in

        let module Expression =
            Parser_expression.Make
                (Ext) (Utils) (Constant)
                (Core) (Type) (Pattern) (Modexpr)
        in

        let module Modexpr = Parser_modexpr.Make
            (Ext) (Utils) (Core) (Type) (Expression)
        in

        let open Modexpr in
        let open Expression in

        let module_extension =
            Named.p "module extension" @@
            id_payload_pair "%%"
        in

        let module_attribute =
            Named.p "module attribute" @@
            id_payload_pair "@@"
        in

        let open_ =
            Named.p "open" begin
                let override = s"!" >>$ Override in

                add_attrs (
                    mapping begin fun override name ->
                        mk_helper ~f:(Opn.mk ?docs:None ?override) name
                    end
                    << k"open" <*>? override <*> -loc u_longident
                )
            end
        in

        let exception_ =
            Named.p "exception" @@
            add_attrs (k"exception" >> ng >> type_extension_constructor)
        in

        let include_sig =
            add_attrs (k"include" >> -use modtype_with >>| mk_helper ~f:(Incl.mk ?docs:None))
        in

        let value_sig =
            Named.p "sig:value" @@
            add_attrs (
                mapping (mk_helper2 (Val.mk ?docs:None ?prim:None))
                << k"let" <*> -loc l_ident << -s":" <*> -use core_type_poly
            )
        in

        let modtype_base =
            Named.p "top:modtype" @@
            add_attrs (
                mapping begin fun name typ ->
                    mk_helper ~f:(Mtd.mk ?docs:None ?text:None ~typ) name
                end
                << k"module" << ng << k"type" << ng <*> loc ident << -s"=" <*> -use modtype_with
            )
        in

        let modtype_sig =
            Named.p "sig:modtype" begin
                    modtype_base
                <|> add_attrs (
                        mapping @@ mk_helper ~f:(Mtd.mk ?docs:None ?text:None ?typ:None)
                        << k"module" << -k"type" <*> -loc u_ident
                    )
            end
        in

        let modtype_str =
            Named.p "str:modtype" @@
            modtype_base
        in

        let export p =
                (
                    mapping begin fun loc ->
                        helper_add_attr_loc "genType" loc
                    end
                    <*> loc_of @@ k"export" << ng <*> p
                )
            <|> p
        in

        let type_ mk =
            let first =
                add_attrs begin
                    mapping begin fun eflag rec_flag decl ->
                        helper @@ fun ~p1 ~p2 ~attrs ->
                            let decl =
                                match eflag with
                                | None -> decl
                                | Some loc -> helper_add_attr_loc "genType" loc decl
                            in

                            rec_flag,
                            apply (helper_add_attrs attrs decl) p1 p2
                    end
                    <*>? (loc_of @@ k"export" << ng)
                    << k"type" <*> (((-k"rec" >>$ Recursive) <|> (-k"nonrec" >>$ Nonrecursive) <|> return Nonrecursive))
                    <*> -type_declaration
                end
            in

            let other =
                add_attrs (k"and" >> -export type_declaration)
            in

            mapping begin fun (rec_flag, hd) tail ->
                mk_na_helper2 mk rec_flag (hd :: tail)
            end
            <*> use first <*>* -use other
        in

        let external_ =
            Named.p "external" @@
            add_attrs begin
                mapping begin fun name typ prim ->
                    mk_helper2 (Val.mk ?docs:None ~prim) name typ
                end
                << k"external" <*> -loc l_ident << -s":" <*> -use core_type_poly
                << -s"=" << ng <*>+ Constant.String.string
            end
        in

        let module_decl =
            Named.p "module decl" begin
                let module_alias =
                    loc u_longident >>| mk_helper ~f:Mty.alias
                in

                mapping (mk_helper2 (Md.mk ?docs:None ?text:None))
                <*> loc u_ident <*>
                    (
                            (-s":" >> -use modtype_with)
                        <|> (-s"=" >> -use module_alias)
                    )
            end
        in

        let module_ =
            Named.p "module" @@
            add_attrs (k"module" >> -set_p1 module_decl)
        in

        let module_rec =
            Named.p "module rec" begin
                let first = add_attrs (k"module" >> ng >> k"rec" >> ng >> module_decl) in
                let other = -use (add_attrs (k"and" >> ng >> module_decl)) in

                mapping begin fun hd tail -> mk_na_helper Sig.rec_module (hd :: tail) end
                <*> use first <*> seq 0 other
            end
        in

        let import =
            Named.p "import" begin

                let item =
                    mapping begin fun name alias typ ->
                        let alias = Base.Option.value alias ~default:name in
                        helper_map
                            (fun hlp ->
                                fun ~p1 ~p2 ~attrs ->
                                    fun attr -> hlp ~p1 ~p2 ~attrs:(attr @ attrs)
                            )
                            (mk_helper2 (Val.mk ?docs:None ?prim:(Some [name.txt])) alias typ)
                    end
                    <*> loc l_ident <*>? (ng >> k"as" >> ng >> loc l_ident) << -s":" <*> -use core_type_poly
                in

                let list =
                    s"{" >> seq 1 ~sep:(-s",") (-use item) << -s"}"
                in

                (
                    mapping begin fun list name ->
                        let attr = Location.mkloc "genType.import" name.loc, PStr [Str.eval @@ Exp.constant @@ Const.string name.txt] in
                        let list = List.map (fun x -> x [attr]) list in
                        let str = List.map Str.primitive list in
                        let mod_ = Mod.structure str in
                        helper_add_attr "ns.jsFfi" @@ mk_helper ~f:(Incl.mk ?docs:None) mod_
                    end
                    << k"import" << ng <*> list << ng << k"from" << ng <*> loc Constant.String.string
                )
                <|>
                (
                    mapping begin fun item name ->
                        let attr =
                            Location.mkloc "genType.import" name.loc,
                            PStr [Str.eval @@ Exp.tuple [Exp.constant @@ Const.string name.txt; Exp.constant @@ Const.string "default"]]
                        in
                        let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                        mk_helper ~f:(Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                    end
                    << k"import" <*> -use item << ng << k"from" << ng <*> loc Constant.String.string
                )
                <|>
                (
                    mapping begin fun list names ->
                        let attr =
                            [ Location.mknoloc "val", PStr []
                            ; Location.mkloc "scope" names.loc,
                                PStr [Str.eval @@ if List.length names.txt = 1 then List.hd names.txt else Exp.tuple names.txt]
                            ]
                        in
                        let list = list |> List.map @@ fun x -> x attr in
                        let str = List.map Str.primitive list in
                        let mod_ = Mod.structure str in
                        mk_helper ~f:(Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                    end
                    << k"import" << ng <*> list << ng << k"from" << ng
                    <*> loc @@ seq 1 ~sep:(-s".") (-use (ident >>| fun x -> mk_helper ~f:Exp.constant (Const.string x)))
                )
                <|>
                (
                    mapping begin fun item names ->
                        let attr =
                            [ Location.mknoloc "val", PStr []
                            ; Location.mkloc "scope" names.loc,
                                PStr [Str.eval @@ if List.length names.txt = 1 then List.hd names.txt else Exp.tuple names.txt]
                            ]
                        in
                        let mod_ = Mod.structure [Str.primitive @@ item attr] in
                        mk_helper ~f:(Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                    end
                    << k"import" <*> -use item << ng << k"from" << ng
                    <*> loc @@ seq 1 ~sep:(-s".") (-use (ident >>| fun x -> mk_helper ~f:Exp.constant (Const.string x)))
                )
                <|>
                (
                    mapping begin fun list ->
                        let attr = Location.mknoloc "val", PStr [] in
                        let list = List.map (fun x -> x [attr]) list in
                        let str = List.map Str.primitive list in
                        let mod_ = Mod.structure str in
                        mk_helper ~f:(Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                    end
                    << k"import" << ng <*> list
                )
                <|>
                (
                    mapping begin fun item ->
                        let attr = Location.mknoloc "val", PStr [] in
                        let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                        mk_helper ~f:(Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                    end
                    << k"import" <*> -use item
                )
            end
        in

        let item_helper
            : 'a 'b. (?loc:Warnings.loc -> 'a -> 'b)
            -> 'a helper parser -> 'b helper parser
            = fun h p ->
                set_loc p >>| fun ph ->
                    helper @@ fun ~p1 ~p2 ~attrs ->
                        let x = apply_ah (helper_add_attrs attrs ph) in
                        h ~loc:(make_location p1 p2) x
        in

        let signature =
            Named.p "sig" begin
                let mb_attributed =
                    [ module_extension >>| mk_helper ~f:Sig.extension
                    ; item_helper Sig.exception_ exception_
                    ; item_helper Sig.value external_
                    ; item_helper Sig.include_ include_sig
                    ; item_helper Sig.value value_sig
                    ; item_helper Sig.modtype modtype_sig
                    ; item_helper Sig.module_ module_
                    ; item_helper Sig.open_ open_
                    ; item_helper Sig.type_extension type_extension
                    ]
                in

                seq 0
                (
                    -use_na_del
                    (
                        Peek.first
                        (
                            [ module_attribute >>| mk_na_helper Sig.attribute
                            ; type_ Sig.type_
                            ; module_rec
                            ; na @@ add_attrs @@ Peek.first mb_attributed
                            ] @ List.map na mb_attributed
                        )
                    )

                )
            end
        in

        let value_str =
            Named.p "str:value" begin
                let lat =
                    k"type" >> seq 1 (-loc l_ident) << -s"."
                in

                let with_lat =
                    Named.p "str:value:vb:lat" begin
                        mapping begin fun p1 attrs pat p2 types typ p3 expr ->
                            let mapping =
                                { Parsetree_mapping.default with
                                    core_type_desc =
                                        { Parsetree_mapping.default.core_type_desc with
                                            constr = fun t a ->
                                                if a <> [] then Ptyp_constr (t, a) else
                                                let rec loop =
                                                    function
                                                    | [] -> Ptyp_constr (t, a)
                                                    | x::xs ->
                                                        if Longident.Lident x.txt = t.txt then
                                                            Ptyp_var x.txt
                                                        else
                                                            loop xs
                                                in
                                                loop types
                                        }
                                }
                            in

                            let typ_pat = Parsetree_mapping.core_type mapping typ in
                            let typ_pat = Typ.poly types typ_pat in

                            let pat = apply pat p1 p2 in
                            let pat = Pat.constraint_ ~loc:(make_location p1 p3) ~attrs pat typ_pat in

                            let expr = List.fold_right
                                begin fun x acc ->
                                    Exp.newtype x acc
                                end
                                types
                                (Exp.constraint_ expr typ)
                            in
                            mk_helper2 (Vb.mk ?docs:None ?text:None) pat expr
                        end
                        <*> pos <*> attrs_ <*> pattern <*> pos << -s":" <*> -lat <*> -use core_type_atom <*> pos
                        << -s"=" <*> -use expression_arrow
                    end
                in

                let helper =
                        with_lat
                    <|> (
                            mapping begin fun pattern expr ->
                                mk_helper2 (Vb.mk ?docs:None ?text:None) pattern expr
                            end
                            <*> use pattern_poly_constrainted << -s"=" <*> -use expression_arrow
                        )
                in

                let first =
                        export (k"let" >> ng >> helper)
                    <|> (
                            mapping begin helper_add_attr_loc "genType" end
                            <*> loc_of @@ k"export" << ng <*> helper
                        )
                in
                let first_rec = export (k"let" >> -k"rec" >> ng >> helper) in
                let other = k"and" >> -set_p1 (export helper) in

                    (
                        mapping begin fun first others ->
                            mk_na_helper2 Str.value Recursive (first :: others)
                        end
                        <*> use @@ add_attrs first_rec <*>* -use (add_attrs other)
                    )
                <|> (
                        mapping begin fun first others ->
                            mk_na_helper2 Str.value Nonrecursive (first :: others)
                        end
                        <*> use @@ add_attrs first <*>* -use (add_attrs other)
                    )
            end
        in

        let _module_binding =
            mapping begin fun a t b ->
                let b = Base.Option.value ~default:b (Base.Option.map ~f:(fun t -> Mod.constraint_ b t) t) in
                mk_helper2 (Mb.mk ?docs:None ?text:None) a b
            end
            <*> loc u_ident <*>? (-s":" >> -use modtype) << ng << o"=" <*> -use modexpr
        in

        let module_binding =
            Named.p "str:mb" begin
                k"module" >> ng >> _module_binding
            end
        in

        let rec_module_binding =
            Named.p "str:mb:rec" begin
                mapping cons
                <*> use @@ add_attrs (k"module" >> ng >> k"rec" >> ng >> _module_binding)
                <*>* -use (add_attrs (k"and" >> ng >> _module_binding))
            end
        in

        let include_ =
            Named.p "include" @@
            add_attrs (
                mapping @@ mk_helper ~f:(Incl.mk ?docs:None)
                << k"include" <*> -use modexpr
            )
        in

        let structure =
            Named.p "str" begin
                let eval = mapping @@ mk_helper ~f:Str.eval <*> use expression_arrow in

                let mb_attributed =
                    [ module_extension >>| mk_helper ~f:Str.extension
                    ; item_helper Str.exception_ exception_
                    ; item_helper Str.primitive external_
                    ; item_helper Str.include_ import
                    ; item_helper Str.include_ include_
                    ; item_helper Str.module_ module_binding
                    ; item_helper Str.modtype modtype_str
                    ; item_helper Str.open_ open_
                    ; item_helper Str.type_extension type_extension
                    ]
                in

                seq 0
                (
                    -use_na_del
                    (
                        Peek.first
                        (
                            [ module_attribute >>| mk_na_helper Str.attribute
                            ; rec_module_binding >>| mk_na_helper Str.rec_module
                            ; value_str
                            ; na @@ add_attrs @@ Peek.first mb_attributed
                            ] @ List.map na mb_attributed
                            @
                            [ type_ Str.type_
                            ; na @@ add_attrs eval
                            ]
                        )
                    )
                    <|>
                    -use_del eval
                )
            end
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

    let parse_interface = parse (signature << ng) State.default
    let parse_implementation = parse (structure << ng) State.default
end

let memo_spec =
    [ "expression"
    ; "expression:arrow"
    ; "expression:p0"
    ; "expression:p8"
	; "pattern"
	; "typexpr"
    ; "typexpr:atom"
    ]
