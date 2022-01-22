open Base
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic
open APos

module Mapping = struct
    let label_declaration =
        mapping begin fun mut name typ ->
            let mut = Base.Option.map mut ~f:(fun _ -> Asttypes.Mutable) in
            let typ =
                match typ with
                | Some typ -> typ
                | None -> Typ.constr Location.{txt = Longident.Lident name.txt; loc = name.loc} []
            in

            mk_helper2 (Type.field ?info:None ?mut) name typ
        end

    let type_declaration_kind_manifest =
        mapping begin fun name params manifest priv kind cstrs ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            mk_helper ~f:(Type.mk
                ?docs:None ?text:None ?params ?cstrs ?kind:(Some kind) ?priv ?manifest:(Some manifest)) name
        end

    let type_declaration_manifest =
        mapping begin fun name params priv manifest cstrs ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            mk_helper ~f:(Type.mk
                ?docs:None ?text:None ?params ?cstrs ?kind:None ?priv ?manifest:(Some manifest)) name
        end

    let type_declaration_kind =
        mapping begin fun name params priv kind cstrs ->
            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
            mk_helper ~f:(Type.mk
                ?docs:None ?text:None ?params ?cstrs ?kind:(Some kind) ?priv ?manifest:None) name
        end

    let type_declaration_abstract =
        mapping begin fun name params ->
            mk_helper ~f:(Type.mk
                ?docs:None ?text:None ?params ?cstrs:None ?kind:None ?priv:None ?manifest:None) name
        end

    let type_extension =
        mapping begin fun name params priv hd tail ->
            let priv = Opt.set Private priv in
            helper @@ fun ~p1:_ ~p2:_ ~attrs ->
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
            let core_type_package = Named.p "typexpr:package"
                begin
                        (
                            mapping @@ mk_helper2 Typ.package
                            <*> loc u_longident << -k"with"
                            <*> (
                                    seq 1 ~sep:(-k"and")
                                        (mapping t2
                                            << -k"type" <*> -loc l_longident
                                            << -s"=" <*> -use core_type_atom
                                        )
                                )
                        )
                    <|> (
                            mapping (fun a -> mk_helper2 Typ.package a [])
                            <*> loc u_longident
                        )
                end

            let var =
                mapping (mk_helper ~f:Typ.var)
                << s"\'" <*> ident

            let any = s"_" >>$ mk_helper ~f:Typ.any ()

            let constr =
                Named.p "typexpr:constr" begin
                    (
                        mapping @@ mk_helper2 Typ.constr
                        <*> loc l_longident << -s"<" <*> seq 1 (-use core_type) ~sep:(-s",") ~trail << -s">"
                    )
                    <|>
                    (
                        mapping @@ (fun a -> mk_helper2 Typ.constr a [])
                        <*> loc l_longident
                    )

                end

            let tuple =
                mapping (mk_helper ~f:Typ.tuple)
                << s"(" <*> seq 2 (-use core_type) ~sep:(-s",") ~trail << -s")"

            let unit =
                mapping @@ (fun a -> mk_helper2 Typ.constr a [])
                <*> loc (s"()" >>$ Longident.Lident "unit")

            let bs_object =
                Named.p "typexpr:bso" begin
                    let object_field =
                        (
                            mapping begin fun attrs name typ -> Otag (name, attrs, typ) end
                            <*> attrs_ <*> loc Constant.String.string << -s":" <*> -use core_type_poly
                        )
                        <|>
                        (
                            mapping begin fun typ -> Oinherit typ end
                            << s"..." <*> -use core_type
                        )
                    in

                    (s"{" >> -s"." >> -s"}" >>$ (mk_helper2 Typ.object_ [] Closed))
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Closed)
                        << s"{"
                        <*> seq 0 ~sep:(-s",") ~trail (ng >> object_field)
                        << -s"}"
                    )
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Open)
                        << s"{" << -s".."
                        <*> seq 0 ~sep:(-s",") ~trail (ng >> object_field)
                        << -s"}"
                    )
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Closed)
                        << s"{" << -s"."
                        <*> seq 0 ~sep:(-s",") ~trail (ng >> object_field)
                        << -s"}"
                    )
                end

            let variant =
                Named.p "typexpr:variant" begin
                    let constructor_arguments =
                            use tuple
                        <|> (s"(" >> -use core_type << -s")")
                    in

                    let row_field =
                        (
                            mapping begin fun attrs tag empty constrs ->
                                Rtag (tag, attrs, Option.is_some empty, constrs)
                            end
                            <*> attrs_ <*> loc variant_tag <*>? -s"&" <*> seq 1 (-constructor_arguments) ~sep:(-s"&")
                        )
                        <|>
                        (
                            mapping begin fun attrs tag ->
                                Rtag (tag, attrs, true, [])
                            end
                            <*> attrs_ <*> loc variant_tag
                        )
                        <|>
                        (use core_type >>| fun x -> Rinherit x)
                    in

                    let rows = opt @@ -s"|" >> seq 1 (-row_field) ~sep:(-s"|") in

                    (s"[" >> -s">" >> -s"]" >>$ (mk_helper3 Typ.variant [] Open None))
                    <|>
                    (
                        mapping begin fun list ->
                            mk_helper3 Typ.variant list Open None
                        end
                        << s"[" << -s">" <*> rows << -s"]"
                    )
                    <|>
                    (
                        mapping begin fun list tags ->
                            let tags = if Option.is_some tags then tags else Some [] in
                            mk_helper3 Typ.variant list Closed tags
                        end
                        << s"[" << -s"<" <*> rows <*>? (-s">" >> seq 1 (ng >> variant_tag)) << -s"]"
                    )
                    <|>
                    (
                        mapping begin fun list ->
                            mk_helper3 Typ.variant list Closed None
                        end
                        << s"[" <*> rows << -s"]"
                    )
                end

            let core_type_atom =
                Named.p "typexpr:atom" @@ add_attrs begin
                    Peek.first
                    [ any
                    ; var
                    ; unit
                    ; s"(" >> -core_type << -s")"
                    ; tuple
                    ; k"module" >> -s"(" >> -core_type_package << -s")"
                    ; bs_object
                    ; constr
                    ; variant
                    ; mapping (mk_helper ~f:Typ.extension) <*> extension
                    ]
                end

            let alias p =
                fold_left_0_1
                    (mapping t3 <*> pos <*> p <*> pos)
                    (mapping t2 << -k"as" << -s"'" <*> (l_ident >>| fun x prev -> mk_helper2 Typ.alias prev x) <*> pos)
                    ~f:begin fun (p1, hlp, p2) (x, p3) ->
                        let prev = apply hlp p1 p2 in
                        p1, x prev, p3
                    end
                    ~fr:(fun (_, x, _) -> x)

            let aliased_atom = Named.p "typexpr:alias:atom" @@ alias core_type_atom

            let arrow =
                begin
                    let _arrow_tail =
                        -s"=>" >>
                        (-use core_type_arrow >>| fun typ label arg -> mk_helper3 Typ.arrow label arg typ)
                    in

                    let with_args typ _tail =
                        add_attrs (
                            mapping begin fun tag p1 x p2 opt tail ->
                                let label = if Option.is_some opt then Optional tag.txt else Labelled tag.txt in
                                let x = helper_add_attr "ns.namedArgLoc" ~loc:tag.loc x in
                                let arg = apply x p1 p2 in
                                tail label arg
                            end
                            << s"~" <*> -loc l_ident << -s":" << ng <*> pos <*> add_attrs typ <*> pos <*>? (-s"=" << -s"?") <*> _tail
                        )
                        <|>
                        (
                            mapping begin fun arg tail ->
                                tail Nolabel arg
                            end
                            <*> use typ <*> _tail
                        )
                    in

                    let tail = fix @@ fun tail ->
                            (s")" >> _arrow_tail)
                        <|> (s"," >> -s")" >> _arrow_tail)
                        <|> (s"," >> -s"." >> -use (with_args core_type tail >>| helper_add_attr "bs") >>| fun typ label arg ->
                                mk_helper3 Typ.arrow label arg typ
                            )
                        <|> (s"," >> -use (with_args core_type tail) >>| fun typ label arg -> mk_helper3 Typ.arrow label arg typ)
                    in

                    Named.p "typexpr:arrow:all" begin
                            add_attrs (s"(" >> -s"." >> -with_args core_type tail >>| helper_add_attr "bs")
                        <|> add_attrs (s"(" >> -with_args core_type tail)
                        <|> Named.p "typexpr:arrow:onearg" (with_args aliased_atom _arrow_tail)
                    end
                end

            let core_type_arrow =
                Named.p "typexpr:arrow"
                (arrow <|> core_type_atom)

            let core_type = Named.p "typexpr" @@ alias core_type_arrow

            let core_type_poly =
                Named.p "typexpr:poly" begin
                    (
                        mapping @@ mk_helper2 Typ.poly
                        <*> seq 1 (s"\'" >> loc l_ident) ~sep:ng
                        << -s"." <*> -use core_type
                    )
                    <|>
                    core_type
                end

            let label_declaration =
                Named.p "label_declraration" @@ add_attrs begin
                    Mapping.label_declaration
                    <*>? (k"mutable" << ng) <*> loc l_ident <*>? (-s":" >> -use core_type_poly)
                end

            let label_declarations =
                s"{" >> seq 1 ~sep:(-s",") ~trail @@ -use label_declaration << -s"}"

            let constr_args =
                    (s"(" >> -label_declarations << opt @@ -s"," << -s")" >>| fun x -> Pcstr_record x)
                <|> (s"(" >> seq 1 (-use core_type) ~sep:(-s",") ~trail << opt @@ -s"," << -s")" >>| fun x -> Pcstr_tuple x)

            let type_kind =
                let variant =
                    Named.p "type_kind:variant" begin
                        let constructor =
                            mapping begin fun name args res ->
                                mk_helper ~f:(Type.constructor ?info:None ?args:(Some args) ?res) name
                            end
                            <*> loc u_ident <*> (-constr_args <|> return @@ Pcstr_tuple []) <*>? (-s":" >> -use core_type_atom)
                        in

                        opt @@ s"|" >> seq 1 (-use (add_attrs constructor)) ~sep:(-s"|") >>| fun x -> Ptype_variant x
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
                        (s"-" >>$ Contravariant << ng)
                    <|> (s"+" >>$ Covariant << ng)
                    <|> return Invariant
                in

                let param =
                    mapping (fun v t -> (t, v))
                    <*> variance <*> use (any <|> var)
                in

                s"<" >> seq 1 ~sep:(-s",") ~trail (-param) << -s">"

            let type_decl_constraints =
                seq 1 (
                    mapping begin fun p1 t1 t2 p2 -> t1, t2, make_location p1 p2 end
                    <*> -pos << k"constraint" <*> -use core_type_atom << -s"=" <*> -use core_type <*> pos
                )

            let type_declaration =
                Named.p "type_declaration" begin
                    Named.p "type_declaration:mankind" (
                        Mapping.type_declaration_kind_manifest
                        <*> loc l_ident <*>? -type_decl_params << -s"=" <*> -use core_type
                        << -s"=" <*>? -k"private" <*> -type_kind <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:manifest" (
                        Mapping.type_declaration_manifest
                        <*> loc l_ident <*>? -type_decl_params << -s"=" <*>? -k"private" <*> -use core_type
                        <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:kind" (
                        Mapping.type_declaration_kind
                        <*> loc l_ident <*>? -type_decl_params
                        << -s"=" <*>? -k"private" <*> -type_kind <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:abstract" (
                        Mapping.type_declaration_abstract
                        <*> loc l_ident <*>? -type_decl_params
                    )
                end

            let _extension_kind =
                let extension_kind =
                        (constr_args >>| fun x -> Pext_decl (x, None))
                    <|> (s"=" >> -loc u_longident >>| fun x -> Pext_rebind x)
                    <|> (s":" >> -use core_type >>| fun x -> Pext_decl (Pcstr_tuple [], Some x))
                in

                Named.p "typexr:constructor:kind" begin
                        -extension_kind
                    <|> return @@ Pext_decl (Pcstr_tuple [], None)
                end

            let type_extension_constructor =
                Named.p "typext:constructor" @@ add_attrs begin
                    mapping (mk_helper2 (Te.constructor ?docs:None ?info:None))
                    <*> loc u_ident <*> _extension_kind
                end

            let type_extension =
                Named.p "typext" @@ add_attrs begin
                    Mapping.type_extension
                    << k"type" <*> -loc l_longident <*>? type_decl_params << -s"+=" <*>? -k"private"
                    << opt @@ -s"|" <*> -use type_extension_constructor <*>* (-s"|" >> -use type_extension_constructor)
                end
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
