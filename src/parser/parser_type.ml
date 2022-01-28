open Base
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic
open APos

module Mapping = struct
    let type_declaration_kind_manifest =
        mapping begin fun name params manifest priv kind cstrs loc ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            Type.mk ~loc
                ?docs:None ?text:None ?params ?cstrs ~kind ?priv ~manifest name
        end

    let type_declaration_manifest =
        mapping begin fun name params priv manifest cstrs loc ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            Type.mk ~loc ?params ?cstrs ?priv ~manifest name
        end

    let type_declaration_kind =
        mapping begin fun name params priv kind cstrs loc ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            Type.mk ~loc ?params ?cstrs ~kind ?priv name
        end

    let type_declaration_abstract =
        mapping begin fun name params loc ->
            Type.mk ~loc ?params name
        end

    let type_extension =
        mapping begin fun attrs name params priv hd tail ->
            let priv = Opt.set Private priv in
            Te.mk ~attrs ?params ?priv name (hd::tail)
        end
end

module Make
        (Ext: EXT)
        (Utils: UTILS) (Constant: CONSTANT) (Core: CORE)
        : TYPE = struct

    open Ext
    open Utils
    open Core

    let x =

        fix_poly @@ fun getter ->
        (module struct
            let core_type_atom = getter.get @@ fun (module M: TYPE) -> M.core_type_atom
            let core_type_arrow = getter.get @@ fun (module M: TYPE) -> M.core_type_arrow
            let core_type = getter.get @@ fun (module M: TYPE) -> M.core_type
            let core_type_poly = getter.get @@ fun (module M: TYPE) -> M.core_type_poly
            let core_type_package =
                Named.p "typexpr:package" begin
                        with_loc & hlp2 Typ.package
                        >$loc u_longident >ng >k"with"
                        >$(
                            seq 1 ~sep:(ng >> k"and")
                                (
                                    mapping t2
                                    >ng >k"type" >ng >$loc l_longident
                                    >ng >s"=" >ng >$core_type_atom
                                )
                        )

                    ||  with_loc & mapping (fun a loc -> Typ.package ~loc a [])
                        >$loc u_longident
                end

            let var =
                with_loc & hlp Typ.var
                >s"\'" >$ident

            let any =
                with_loc & mapping (fun loc -> Typ.any ~loc ())
                >s"_"

            let constr =
                Named.p "typexpr:constr" begin
                        with_loc & hlp2 Typ.constr
                        >$loc l_longident >ng >s"<" >ng >$(seq 1 core_type ~sep ~trail) >ng >s">"

                    ||  with_loc & mapping (fun a loc -> Typ.constr ~loc a [])
                        >$loc l_longident
                end

            let tuple =
                with_loc & parens & hlp Typ.tuple
                >$(seq 2 core_type ~sep ~trail)

            let unit =
                with_loc & mapping (fun a loc -> Typ.constr ~loc a [])
                >$loc (s"()" >>$ Longident.Lident "unit")

            let bs_object =
                Named.p "typexpr:bso" begin
                    let object_field =
                            mapping begin fun attrs name typ -> Otag (name, attrs, typ) end
                            >$attrs_ >$loc Constant.String.string >ng >s":" >ng >$core_type_poly

                        ||  mapping begin fun typ -> Oinherit typ end
                            >s"..." >ng >$core_type
                    in

                        with_loc & mapping (fun loc -> Typ.object_ ~loc [] Closed)
                        >s"{" >ng >s"." >ng >s"}"

                    ||  with_loc & mapping (fun a loc -> Typ.object_ ~loc a Closed)
                        >s"{" >ng >$(seq 0 object_field ~sep ~trail) >ng >s"}"

                    ||  with_loc & mapping (fun a loc -> Typ.object_ ~loc a Open)
                        >s"{" >ng >s".." >ng >$(seq 0 object_field ~sep ~trail) >ng >s"}"

                    ||  with_loc & mapping (fun a loc -> Typ.object_ ~loc a Closed)
                        >s"{" >ng >s"." >ng >$(seq 0 object_field ~sep ~trail) >ng >s"}"
                end

            let variant =
                Named.p "typexpr:variant" begin
                    let constructor_arguments =
                            tuple
                        ||  s"(" >> ng >> core_type << ng << s")"
                    in

                    let row_field =
                            mapping begin fun attrs tag empty constrs ->
                                Rtag (tag, attrs, Option.is_some empty, constrs)
                            end
                            >$attrs_ >$loc variant_tag >ng >?(s"&">ng) >$(seq 1 constructor_arguments ~sep:(ng>s"&">ng))

                        ||  mapping begin fun attrs tag ->
                                Rtag (tag, attrs, true, [])
                            end
                            >$attrs_ >$loc variant_tag

                        || core_type >>| (fun x -> Rinherit x)
                    in

                    let rows = opt (s"|" << ng) >> seq 1 row_field ~sep:(ng>s"|">ng) in

                        with_loc & mapping (fun loc -> Typ.variant ~loc [] Open None)
                        >s"[" >ng >s">" >ng >s"]"

                    ||  with_loc & mapping begin fun list loc ->
                            Typ.variant ~loc list Open None
                        end
                        >s"[" >ng >s">" >ng >$rows >ng >s"]"

                    ||  with_loc & mapping begin fun list tags loc ->
                            let tags = if Option.is_some tags then tags else Some [] in
                            Typ.variant ~loc list Closed tags
                        end
                        >s"[" >ng >s"<" >ng >$rows >?(ng >> s">" >> seq 1 (ng >> variant_tag)) >ng >s"]"

                    ||  with_loc & mapping begin fun list loc ->
                            Typ.variant ~loc list Closed None
                        end
                        >s"[" >ng >$rows >ng >s"]"
                end

            let core_type_atom =
                Named.p "typexpr:atom" begin
                    typ_attrs @@ Peek.first
                    [ any
                    ; var
                    ; unit
                    ;
                        with_loc & parens & mapping typ_loc
                        >$core_type
                    ; tuple
                    ;
                        with_loc & mapping typ_loc
                        >k"module" >$parens(core_type_package)
                    ; bs_object
                    ; constr
                    ; variant
                    ;
                        with_loc & hlp Typ.extension
                        >$extension
                    ]
                end

            let alias p =
                fold_left_cont_0_1
                    p
                    (
                        mapping begin fun x loc_end prev ->
                            Typ.alias ~loc:{prev.ptyp_loc with loc_end} prev x
                        end
                        >ng >k"as" >ng >s"'" >$l_ident >$pos
                    )

            let aliased_atom = Named.p "typexpr:alias:atom" @@ alias core_type_atom

            let arrow =
                begin
                    let arrow_tail =
                        mapping begin fun typ label arg ->
                            Typ.arrow ~loc:(comb_location arg.ptyp_loc typ.ptyp_loc) label arg typ
                        end
                        >s"=>" >ng >$core_type_arrow
                    in

                    let with_args typ tail =
                            typ_attrs & mapping begin fun tag x opt tail ->
                                let label = if Option.is_some opt then Optional tag.txt else Labelled tag.txt in
                                let x = typ_add_attr "ns.namedArgLoc" ~loc:tag.loc x in
                                tail label x
                            end
                            >s"~" >ng >$loc l_ident >ng >s":" >ng >$typ_attrs typ >?(ng>s"=">ng>s"?") >ng >$tail

                        ||  mapping begin fun arg tail ->
                                tail Nolabel arg
                            end
                            >$typ >ng >$tail
                    in

                    let tail = fix @@ fun tail ->
                            s")" >> ng >> arrow_tail

                        ||  s"," >> ng >> s")" >> ng >> arrow_tail

                        ||  mapping begin fun typ label arg ->
                                let typ = typ_add_attr "bs" typ in
                                Typ.arrow label arg typ
                            end
                            >s"," >ng >s"." >ng >$with_args core_type tail

                        ||  mapping begin fun typ label arg ->
                                Typ.arrow label arg typ
                            end
                            >s"," >ng >$with_args core_type tail
                    in

                    Named.p "typexpr:arrow:all" begin
                            typ_attrs & s"(" >> ng >> s"." >> ng >> with_args core_type tail >>| typ_add_attr "bs"
                        ||  typ_attrs & s"(" >> ng >> with_args core_type tail
                        ||  Named.p "typexpr:arrow:onearg" (with_args aliased_atom arrow_tail)
                    end
                end

            let core_type_arrow =
                Named.p "typexpr:arrow"
                (arrow <|> core_type_atom)

            let core_type = Named.p "typexpr" @@ alias core_type_arrow

            let core_type_poly =
                Named.p "typexpr:poly" begin
                        with_loc & hlp2 Typ.poly
                        >$seq 1 (s"\'" >> loc l_ident) ~sep:ng >ng >s"." >ng >$core_type

                    ||  core_type
                end

            let label_declaration =
                Named.p "label_declraration" begin
                    with_loc & mapping begin fun attrs mut name typ loc ->
                        let mut = Base.Option.map mut ~f:(fun _ -> Asttypes.Mutable) in
                        let typ =
                            match typ with
                            | Some typ -> typ
                            | None -> Typ.constr Location.{txt = Longident.Lident name.txt; loc = name.loc} []
                        in

                        Type.field ~loc ~attrs ?info:None ?mut name typ
                    end
                    >$attrs_ >?(k"mutable">ng) >$loc l_ident >?(ng >> s":" >> ng >> core_type_poly)
                end

            let label_declarations =
                braces & seq 1 label_declaration ~sep ~trail

            let constr_args =
                    parens & mapping (fun x -> Pcstr_record x)
                    >$label_declarations >opt sep
                ||  parens & mapping (fun x -> Pcstr_tuple x)
                    >$seq 1 core_type ~sep ~trail

            let type_kind =
                let variant =
                    Named.p "type_kind:variant" begin
                        let constructor =
                            with_loc & mapping begin fun attrs name args res loc ->
                                Type.constructor ~loc ~attrs ?info:None ?args:(Some args) ?res name
                            end
                            >$attrs_ >$loc u_ident >$((ng >> constr_args) <|> return @@ Pcstr_tuple []) >?(ng >> s":" >> ng >> core_type_atom)
                        in

                        opt @@ s"|" >> seq 1 (ng >> constructor) ~sep:(ng >> s"|") >>| fun x -> Ptype_variant x
                    end
                in

                let record =
                    label_declarations >>| fun x -> Ptype_record x
                in

                let open_ = s".." >>$ Ptype_open in

                Named.p "type_kind" begin
                    variant <|> record <|> open_
                end

            let type_decl_params =
                let variance =
                        s"-" >>$ Contravariant << ng
                    ||  s"+" >>$ Covariant << ng
                    ||  return Invariant
                in

                let param =
                    mapping (fun v t -> (t, v))
                    >$variance >$(any <|> var)
                in

                chevrons & seq 1 param ~sep ~trail

            let type_decl_constraints =
                seq 1 ~sep:ng
                (
                    with_loc & mapping t3
                    >k"constraint" >ng >$core_type_atom >ng >s"=" >ng >$core_type
                )


            let type_declaration =
                Named.p "type_declaration" begin
                        Named.p "type_declaration:mankind" &
                        with_loc & Mapping.type_declaration_kind_manifest
                        >$loc l_ident >ng >?(type_decl_params>ng) >s"=" >ng >$core_type
                        >ng >s"=" >ng >?(k"private">ng) >$type_kind >?(ng >> type_decl_constraints)

                    ||  Named.p "type_declaration:manifest" &
                        with_loc & Mapping.type_declaration_manifest
                        >$loc l_ident >ng >?(type_decl_params>ng) >s"=" >ng >?(k"private">ng) >$core_type
                        >?(ng >> type_decl_constraints)

                    ||  Named.p "type_declaration:kind" &
                        with_loc & Mapping.type_declaration_kind
                        >$loc l_ident >ng >?(type_decl_params>ng) >s"=" >ng >?(k"private">ng)
                        >$type_kind >?(ng >> type_decl_constraints)

                    ||  Named.p "type_declaration:abstract" &
                        with_loc & Mapping.type_declaration_abstract
                        >$loc l_ident >?(ng >> type_decl_params)
                end

            let _extension_kind =
                let extension_kind =
                        constr_args >>| (fun x -> Pext_decl (x, None))
                    ||  s"=" >> ng >> loc u_longident >>| (fun x -> Pext_rebind x)
                    ||  s":" >> ng >> core_type >>| (fun x -> Pext_decl (Pcstr_tuple [], Some x))
                in

                Named.p "typexr:constructor:kind" begin
                        ng >> extension_kind
                    ||  return @@ Pext_decl (Pcstr_tuple [], None)
                end

            let type_extension_constructor =
                Named.p "typext:constructor" begin
                    with_loc & hlp2_a (Te.constructor ?docs:None ?info:None)
                    >$attrs_ >$loc u_ident >$_extension_kind
                end

            let type_extension =
                Named.p "typext" &
                Mapping.type_extension
                >$attrs_ >k"type" >ng >$loc l_longident >ng <*>?(type_decl_params>ng) >s"+=" >ng >?(k"private">ng)
                >opt(s"|">ng) >$type_extension_constructor >*(ng >> s"|" >> ng >> type_extension_constructor)
        end: TYPE)

    let core_type_atom = let (module M) = x in M.core_type_atom
    let core_type_arrow = let (module M) = x in M.core_type_arrow
    let core_type = let (module M) = x in M.core_type
    let core_type_poly = let (module M) = x in M.core_type_poly
    let core_type_package = let (module M) = x in M.core_type_package
    let type_extension_constructor = let (module M) = x in M.type_extension_constructor
    let type_extension = let (module M) = x in M.type_extension
    let type_decl_params = let (module M) = x in M.type_decl_params
    let type_decl_constraints = let (module M) = x in M.type_decl_constraints
    let type_declaration = let (module M) = x in M.type_declaration
end
