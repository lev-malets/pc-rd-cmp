open Basic
open Angstrom
open Angstrom.Let_syntax
open Angstrom.Parser
open Parsetree
open Sigs

(* TODO errors position *)
(* points:
    number of calls
    number of state changes
*)

module Make (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser) = struct
    module Utils = Parser_utils.Make(Named)
    open Utils

    type parsers =
        {
            signature : signature t;
            structure : structure t;
            attribute : attribute t;
            extension : attribute t;

            modexpr : (module MODEXPR);
        }

    let
        {
            signature;
            structure;
            extension = _;
            attribute = _;
            modexpr = _;
        }
    = fix_poly @@ fun getter ->
        let open Ast_helper in
        let open Asttypes in

        let module Constant = Parser_constant.Make(Named)(Utils) in
        let open Utils in

        let module Core: CORE =
            struct
                let signature = getter.get @@ fun x -> x.signature
                let structure = getter.get @@ fun x -> x.structure
                let attribute = getter.get @@ fun x -> x.attribute
                let extension = getter.get @@ fun x -> x.extension

                let attrs_ =
                    Named.p "attrs" @@
                    memo @@
                    seq 0 ~sep:ng ~trail attribute

                let use mk =
                    mapping begin fun p1 hlp p2 -> apply hlp p1 p2 end
                    <*> pos <*> mk <*> pos

                let _use mk = ng >> use mk

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

        let module Type = Parser_type.Make (Named) (Utils) (Constant) (Core) in
        let module Pattern = Parser_pattern.Make (Named) (Utils) (Constant) (Core) (Type) in

        let open Core in
        let open Type in
        let open Pattern in

        let attrubute_id = take_while1 Pervasives.(fun x -> identifier's_character x || x = '.') in
        let payload =
            Named.p "payload" begin
                    (s"(" >> _s"?" >> _use pattern << _s")" >>| fun x -> PPat (x, None))
                <|> (s"(" >> _s":" >> _k"sig" >> signature << _s")" >>| fun x -> PSig x)
                <|> (s"(" >> _s":" >> _k"sig" >> _use core_type  << _s")" >>| fun x -> PTyp x)
                <|> (s"(" >> structure << _s")" >>| fun x -> PStr x)
                <|> return @@ PStr []
            end
        in

        let id_payload_pair start =
            both
                (loc (s start >> attrubute_id))
                payload
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
                (Named) (Utils) (Constant)
                (Core) (Type) (Pattern) (Modexpr)
        in

        let module Modexpr = Parser_modexpr.Make
            (Named) (Utils) (Core) (Type) (Expression)
        in


        let open Modexpr in
        let open Expression in

        let attribute =
            id_payload_pair "@"
        in

        let extension =
            id_payload_pair "%"
        in

        let module_extension =
            Named.p "module extension" @@
            id_payload_pair "%%"
        in

        let module_attribute =
            id_payload_pair "@@"
        in

        let open_ =
            Named.p "open" begin
                let override = option Fresh (s"!" >>$ Override) in

                add_attrs @@ map2
                    (k"open" >> override)
                    (ng >> loc u_longident)
                    ~f:(fun override name ->
                        mk_helper (Opn.mk ?docs:None ?override:(Some override)) name
                    )
            end
        in

        let exception_sig =
            Named.p "exception" @@
            add_attrs (k"exception" >> ng >> type_extension_constructor)
        in

        let include_sig =
            add_attrs (k"include" >> _use modtype_with >>| mk_helper (Incl.mk ?docs:None))
        in

        let value_sig =
            Named.p "sig:value" @@
            add_attrs (
                mapping (mk_helper2 (Val.mk ?docs:None ?prim:None))
                <* k"let" <*> _loc l_ident <* _s":" <*> _use core_type_poly
            )
        in

        let value_sig = set_p1 value_sig in

        let modtype_base =
            Named.p "top:modtype" @@
            add_attrs (
                mapping begin fun name typ ->
                    mk_helper (Mtd.mk ?docs:None ?text:None ?typ:(Some typ)) name
                end
                << k"module" << ng << k"type" << ng <*> loc ident <* _s"=" <*> _use modtype_with
            )
        in

        let modtype_sig =
            Named.p "modtype sig" begin
                modtype_base
                <|>
                add_attrs (
                    mapping @@ mk_helper @@ Mtd.mk ?docs:None ?text:None ?typ:None
                    <* k"module" <* _k"type" <*> _loc u_ident
                )
            end
        in

        let modtype_str =
            Named.p "modtype str" @@
            modtype_base
        in

        let export p =
                (
                    mapping begin fun loc ->
                        helper_add_attr_loc "genType" loc
                    end
                    <*> loc_of @@ k"export" <* ng <*> p
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
                    <* k"type" << ng <*> option Nonrecursive (((k"rec" >>$ Recursive) <|> (k"nonrec" >>$ Nonrecursive)) << ng)
                    <*> type_declaration
                end
            in

            let other =
                add_attrs (k"and" >> ng >> export type_declaration)
            in

            mapping begin fun (rec_flag, hd) tail ->
                mk_na_helper2 mk rec_flag (hd :: tail)
            end
            <*> use first <*>* _use other
        in

        let external_ =
            Named.p "external" @@
            add_attrs begin
                mapping begin fun name typ prim ->
                    mk_helper2 (Val.mk ?docs:None ~prim) name typ
                end
                <* k"external" <*> _loc l_ident <* _s":" <*> _use core_type_poly
                <* _s_"=" <*>+ Constant.String.string
            end
        in

        let module_decl =
            Named.p "module decl" begin
                let module_alias =
                    map
                    (loc u_longident)
                    ~f:(mk_helper Mty.alias)
                in

                map2
                (loc u_ident)
                (
                    (_s":" >> _use modtype_with)
                    <|>
                    (_s"=" >> _use module_alias)
                )
                ~f:(mk_helper2 (Md.mk ?docs:None ?text:None))
            end
        in

        let module_ =
            Named.p "module" @@
            add_attrs (k_"module" >> set_p1 module_decl)
        in

        let module_rec =
            Named.p "module rec" begin
                let first = add_attrs (k"module" >> ng >> k"rec" >> ng >> module_decl) in
                let other = _use @@ add_attrs (k"and" >> ng >> module_decl) in

                map2 (use first) (many other)
                ~f:(fun hd tail -> mk_na_helper Sig.rec_module (hd :: tail))
            end
        in

        let item_helper : 'a 'b.
            (?loc:Warnings.loc -> 'a -> 'b) ->
            'a helper parser ->
            'b helper parser
            = fun h p ->
                mapping (mk_na_helper h)
                <*> use p
        in

        let item_nohelper : 'a.
            'a helper t -> 'a helper t
            = fun p ->
                item_helper (fun ?loc:_ x -> x) p
        in

        let use_item : 'a.
            'a helper t -> 'a t
            =
            fun p ->
                mapping begin fun p1 hlp p2 ->
                    apply hlp p1 p2
                end
                <*> pos <*> p <*> del_pos
        in

        let _use_item p = ng >> use_item p in

        let import =
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
                <*> loc l_ident <*>? (ng >> k"as" >> ng >> loc l_ident) <* _s":" <*> _use core_type_poly
            in

            let list =
                s_"{" >> seq 1 ~sep:(_s",") (_use item) << _s"}"
            in

            (
                mapping begin fun list name ->
                    let attr = Location.mkloc "genType.import" name.loc, PStr [Str.eval @@ Exp.constant @@ Const.string name.txt] in
                    let list = List.map (fun x -> x [attr]) list in
                    let str = List.map Str.primitive list in
                    let mod_ = Mod.structure str in
                    helper_add_attr "ns.jsFfi" @@ mk_helper (Incl.mk ?docs:None) mod_
                end
                <* k"import" <* ng <*> list <* ng <* k"from" <* ng <*> loc Constant.String.string
            )
            <|>
            (
                mapping begin fun item name ->
                    let attr =
                        Location.mkloc "genType.import" name.loc,
                        PStr [Str.eval @@ Exp.tuple [Exp.constant @@ Const.string name.txt; Exp.constant @@ Const.string "default"]]
                    in
                    let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                    mk_helper (Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                end
                <* k"import" <*> _use item <* ng <* k"from" <* ng <*> loc Constant.String.string
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
                    mk_helper (Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                end
                <* k"import" <* ng <*> list <* ng <* k"from" <* ng
                <*> loc @@ seq 1 ~sep:(_s".") (_use (ident >>| fun x -> mk_helper Exp.constant (Const.string x)))
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
                    mk_helper (Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                end
                <* k"import" <*> _use item <* ng <* k"from" <* ng
                <*> loc @@ seq 1 ~sep:(_s".") (_use (ident >>| fun x -> mk_helper Exp.constant (Const.string x)))
            )
            <|>
            (
                mapping begin fun list ->
                    let attr = Location.mknoloc "val", PStr [] in
                    let list = List.map (fun x -> x [attr]) list in
                    let str = List.map Str.primitive list in
                    let mod_ = Mod.structure str in
                    mk_helper (Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                end
                <* k"import" <* ng <*> list
            )
            <|>
            (
                mapping begin fun item ->
                    let attr = Location.mknoloc "val", PStr [] in
                    let mod_ = Mod.structure [Str.primitive @@ item [attr]] in
                    mk_helper (Incl.mk ?docs:None) mod_ |> helper_add_attr "ns.jsFfi"
                end
                <* k"import" <*> _use item
            )
        in

        let signature =
            Named.p "sig" begin
                let attribute = module_attribute >>| mk_na_helper Sig.attribute  in
                let extension = add_attrs (mapping @@ mk_helper Sig.extension <*> module_extension) in
                let exception_ = item_helper Sig.exception_ exception_sig in
                let external_ = item_helper Sig.value external_ in
                let include_ = item_helper Sig.include_ include_sig in
                let let_ = item_helper Sig.value value_sig in
                let modtype = item_helper Sig.modtype modtype_sig in
                let module_ = item_helper Sig.module_ module_ in
                let module_rec = item_nohelper module_rec in
                let open_ = item_helper Sig.open_ open_ in
                let type_ext = item_helper Sig.type_extension type_extension in

                let sig_item =
                    Named.p "sig:item" begin
                            attribute
                        <|> extension
                        <|> exception_ <|> external_
                        <|> include_ <|> let_
                        <|> modtype <|> module_ <|> module_rec
                        <|> open_
                        <|> type_ext <|> type_ Sig.type_
                    end
                in
                many @@ _use_item sig_item
            end
        in

        let value_str =
            Named.p "str:value" begin
                let lat =
                    k"type" >> seq 1 (_loc l_ident) << _s"."
                in

                let with_lat =
                    Named.p "str:value:vb:lat" begin
                        position >>= fun p1 ->
                        attrs_ >>= fun attrs ->
                        pattern >>= fun pat ->
                        position >>= fun p2 ->
                        _s":" >> ng >> lat >>= fun types ->
                        _use core_type_atom >>= fun typ ->
                        position >>= fun p3 ->
                        _s"=" >>
                            _use expression_arrow >>| fun expr ->

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
                in

                let helper =
                        with_lat
                    <|> (
                            mapping begin fun pattern expr ->
                                mk_helper2 (Vb.mk ?docs:None ?text:None) pattern expr
                            end
                            <*> use pattern_poly_constrainted <* _s"=" <*> _use expression_arrow
                        )
                in

                let value_binding_first =
                        (
                            export begin
                                k"let" >> ng >> helper
                            end
                        )
                    <|> (
                            mapping begin
                                helper_add_attr_loc "genType"
                            end
                            <*> loc_of @@ k"export" <* ng <*> helper
                        )
                in

                let value_binding_other =
                    _use @@ add_attrs (
                        k_"and" >> set_p1 @@ export helper
                    )
                in

                mapping begin fun first others ->
                    mk_na_helper2 Str.value Asttypes.Nonrecursive (first :: others)
                end
                <*> use @@ add_attrs value_binding_first <*>* value_binding_other
            end
        in

        let _module_binding =
            mapping begin fun a t b ->
                let b = Base.Option.value ~default:b (Base.Option.map ~f:(fun t -> Mod.constraint_ b t) t) in
                mk_helper2 (Mb.mk ?docs:None ?text:None) a b
            end
            <*> loc u_ident <*>? (_s":" >> _use modtype) <* ng <* o"=" <*> _use modexpr
        in

        let module_binding =
            k"module" >> ng >> _module_binding
        in

        let rec_module_binding =
            mapping cons
            <*> use @@ add_attrs (k"module" >> ng >> k"rec" >> ng >> _module_binding)
            <*>* _use @@ add_attrs (k"and" >> ng >> _module_binding)
        in

        let include_ =
            add_attrs (
                mapping @@ mk_helper @@ Incl.mk ?docs:None
                <* k"include" <*> _use modexpr
            )
        in

        let structure =
            Named.p "str" begin
                let structure_item =
                    Named.p "str:item" begin
                        value_str
                        <|> (rec_module_binding >>| mk_na_helper Str.rec_module)
                        <|> item_helper Str.module_ module_binding
                        <|> (module_attribute >>| mk_na_helper Str.attribute)
                        <|> item_helper Str.modtype modtype_str
                        <|> item_helper Str.type_extension type_extension
                        <|> type_ Str.type_
                        <|> item_helper Str.primitive external_
                        <|> item_helper Str.include_ import
                        <|> item_helper Str.include_ include_
                        <|> item_helper Str.open_ open_
                        <|> item_helper Str.exception_ exception_sig
                        <|> add_attrs (mapping @@ mk_helper Str.extension <*> module_extension)
                        <|> add_attrs (mapping @@ mk_helper Str.eval <*> use expression_arrow)
                    end
                in
                many @@ _use_item structure_item
            end
        in

        {
            signature;
            structure;
            attribute;
            extension;

            modexpr = (module Modexpr);
        }

    let with_print p =
        let%map res = p
        and _state = state_get
        in
        (*Res_diagnostics.printReport state.diagnostics "TODO";*)
        res

    let parse p state filename =
        parse_string p state ~filename (Res_io.readFile ~filename)

    let parse_interface = parse (with_print @@ signature << ng) State.default
    let parse_implementation = parse (with_print @@ structure << ng) State.default
end
