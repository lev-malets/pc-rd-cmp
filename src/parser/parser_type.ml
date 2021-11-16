
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic.Angstrom
open Basic

module Make
        (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser)
        (Utils: UTILS) (Constant: CONSTANT) (Core: CORE)
        : TYPE = struct

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
                        <*> loc u_longident <* _k"with" <*> (
                            seq 1 ~sep:(_k"and")
                            (both
                                (_k"type" >> _loc l_longident)
                                (_s"=" >> _use core_type_atom))
                        )
                    )
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.package a [])
                        <*> loc u_longident
                    )
                end

            let var =
                mapping (mk_helper Typ.var)
                <* s"\'" <*> ident

            let any = s"_" >>$ mk_helper Typ.any ()

            let constr =
                Named.p "typexpr:constr" begin
                    (
                        mapping @@ mk_helper2 Typ.constr
                        <*> loc l_longident <* _s"<" <*> seq 1 (_use core_type) ~sep:(_s",") ~trail <* _s">"
                    )
                    <|>
                    (
                        mapping @@ (fun a -> mk_helper2 Typ.constr a [])
                        <*> loc l_longident
                    )

                end

            let tuple =
                mapping (mk_helper Typ.tuple)
                <* s"(" <*> seq 2 (_use core_type) ~sep:(_s",") ~trail <* _s")"

            let unit =
                mapping @@ (fun a -> mk_helper2 Typ.constr a [])
                <*> loc (s"()" >>$ Longident.Lident "unit")

            let bs_object =
                Named.p "typexpr:bso" begin
                    let object_field =
                        (
                            mapping begin fun attrs name typ -> Otag (name, attrs, typ) end
                            <*> attrs_ <*> loc Constant.String.string <* _s":" <*> _use core_type_poly
                        )
                        <|>
                        (
                            mapping begin fun typ -> Oinherit typ end
                            <* s"..." <*> _use core_type
                        )
                    in

                    (s"{" >> _s"." >> _s"}" >>$ (mk_helper2 Typ.object_ [] Closed))
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Closed)
                        <* s"{"
                        <*> seq 0 ~sep:(_s",") ~trail (ng >> object_field)
                        <* _s"}"
                    )
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Open)
                        <* s"{" <* _s".."
                        <*> seq 0 ~sep:(_s",") ~trail (ng >> object_field)
                        <* _s"}"
                    )
                    <|>
                    (
                        mapping (fun a -> mk_helper2 Typ.object_ a Closed)
                        <* s"{" <* _s"."
                        <*> seq 0 ~sep:(_s",") ~trail (ng >> object_field)
                        <* _s"}"
                    )
                end

            let variant =
                Named.p "typexpr:variant" begin
                    let constructor_arguments =
                            use tuple
                        <|> (s"(" >> _use core_type << _s")")
                    in

                    let row_field =
                        (
                            mapping begin fun attrs tag empty constrs ->
                                Rtag (tag, attrs, empty <> None, constrs)
                            end
                            <*> attrs_ <*> loc variant_tag <*>? _s"&" <*> seq 1 (ng >> constructor_arguments) ~sep:(_s"&")
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

                    let rows = opt @@ _s"|" >> seq 1 (ng >> row_field) ~sep:(_s"|") in

                    (s_"[" >> _s">" >> _s"]" >>$ (mk_helper3 Typ.variant [] Open None))
                    <|>
                    (
                        mapping begin fun list ->
                            mk_helper3 Typ.variant list Open None
                        end
                        << s_"[" << _s">" <*> rows << _s"]"
                    )
                    <|>
                    (
                        mapping begin fun list tags ->
                            let tags = if tags <> None then tags else Some [] in
                            mk_helper3 Typ.variant list Closed tags
                        end
                        << s_"[" << _s"<" <*> rows <*>? (_s">" >> seq 1 (ng >> variant_tag)) << _s"]"
                    )
                    <|>
                    (
                        mapping begin fun list ->
                            mk_helper3 Typ.variant list Closed None
                        end
                        << s_"[" <*> rows << _s"]"
                    )
                end

            let core_type_atom =
                Named.p "typexpr:atom" @@ add_attrs begin
                    peek_char_fail >>= function
                    | '_' -> any
                    | '\'' -> var
                    | '(' -> unit <|> (s_"(" >> core_type << _s")") <|> tuple
                    | _ ->
                        (k"module" >> _s_"(" >> core_type_package << _s")")
                        <|>
                        bs_object
                        <|>
                        constr
                        <|>
                        variant
                        <|>
                        (mapping (mk_helper Typ.extension) <*> extension)
                end

            let alias p =
                fold_left_0_1
                    (mapping t3 <*> pos <*> p <*> pos)
                    (both (_k"as" >> _s"'" >> l_ident >>| fun x prev -> mk_helper2 Typ.alias prev x) pos)
                    ~f:begin fun (p1, hlp, p2) (x, p3) ->
                        let prev = apply hlp p1 p2 in
                        p1, x prev, p3
                    end
                    ~fr:(fun (_, x, _) -> x)

            let aliased_atom = Named.p "typexpr:alias:atom" @@ alias core_type_atom

            let arrow =
                Named.p "typexpr:arrow" begin
                    let arrow_tail =
                        s"=>" >>
                        map (_use core_type_arrow) ~f:(fun typ label arg -> mk_helper3 Typ.arrow label arg typ)
                    in

                    let with_args typ tail =
                        add_attrs (
                            mapping begin fun tag p1 x p2 opt tail ->
                                let label = if opt <> None then Optional tag.txt else Labelled tag.txt in
                                let x = helper_add_attr_loc "ns.namedArgLoc" tag.loc x in
                                let arg = apply x p1 p2 in
                                tail label arg
                            end
                            << s_"~" <*> loc l_ident << _s":" << ng <*> pos <*> add_attrs typ <*> pos <*>? (_s"=" << _s"?") << ng <*> tail
                        )
                        <|>
                        (
                            mapping begin fun arg tail ->
                                tail Nolabel arg
                            end
                            <*> use typ << ng <*> tail
                        )
                    in

                    let tail = fix @@ fun tail ->
                        (s"," >> _s"." >> _use (with_args core_type tail >>| helper_add_attr "bs") >>| fun typ label arg ->
                            mk_helper3 Typ.arrow label arg typ
                        )
                        <|>
                        (s"," >> _use (with_args core_type tail) >>| fun typ label arg -> mk_helper3 Typ.arrow label arg typ)
                        <|>
                        (s")" >> ng >> arrow_tail)
                    in

                    add_attrs (s"(" >> _s"." >> ng >> with_args core_type tail >>| helper_add_attr "bs")
                    <|>
                    add_attrs (s"(" >> ng >> with_args core_type tail)
                    <|>
                    Named.p "typexpr:arrow:onearg" (with_args aliased_atom arrow_tail)
                end

            let core_type_arrow = arrow <|> core_type_atom

            let core_type = Named.p "typexpr:alias:arrow" @@ alias core_type_arrow

            let core_type_poly =
                Named.p "typexpr:poly" begin
                    (
                        mapping @@ mk_helper2 Typ.poly
                        <*> seq 1 (s"\'" >> loc l_ident) ~sep:ng
                        << _s"." <*> _use core_type
                    )
                    <|>
                    core_type
                end

            let label_declaration =
                Named.p "label_declraration" @@ add_attrs begin
                    mapping begin fun mut name typ ->
                        let mut = Base.Option.map mut ~f:(fun _ -> Asttypes.Mutable) in
                        let typ =
                            match typ with
                            | Some typ -> typ
                            | None -> Typ.constr Location.{txt = Longident.Lident name.txt; loc = name.loc} []
                        in

                        mk_helper2 (Type.field ?info:None ?mut) name typ
                    end
                    <*>? (k"mutable" << ng) <*> loc l_ident <*>? (_s":" >> _use core_type_poly)
                end

            let label_declarations =
                s_"{" >> seq 1 ~sep:(_s",") ~trail @@ _use label_declaration << _s"}"

            let constr_args =
                    (s_"(" >> label_declarations << opt @@ _s"," << _s")" >>| fun x -> Pcstr_record x)
                <|> (s_"(" >> seq 1 (_use core_type) ~sep:(_s",") ~trail << opt @@ _s"," << _s")" >>| fun x -> Pcstr_tuple x)

            let type_kind =
                let variant =
                    Named.p "type_kind:variant" begin
                        let constructor =
                            mapping begin fun name args res ->
                                mk_helper (Type.constructor ?info:None ?args:(Some args) ?res) name
                            end
                            <*> loc u_ident <*> ((ng >> constr_args) <|> return @@ Pcstr_tuple []) <*>? (_s":" >> _use core_type_atom)
                        in

                        opt @@ s"|" >> seq 1 (_use @@ add_attrs constructor) ~sep:(_s"|") >>| fun x -> Ptype_variant x
                    end
                in

                let record =
                    label_declarations >>| fun x -> Ptype_record x
                in

                let open_ = s".." >>$ Ptype_open in

                variant <|> record <|> open_

            let type_decl_params =
                let variance =
                    (s"-" >>$ Contravariant << ng)
                    <|>
                    (s"+" >>$ Covariant << ng)
                    <|>
                    return Invariant
                in

                let param =
                    mapping (fun v t -> (t, v))
                    <*> variance <*> use (any <|> var)
                in

                s_"<" >> seq 1 ~sep:(_s",") ~trail (ng >> param) << _s">"

            let type_decl_constraints =
                many1 (
                    mapping begin fun p1 t1 t2 p2 ->
                        t1, t2, make_location p1 p2
                    end
                    << ng <*> position << k"constraint" <*> _use core_type_atom << _s"=" <*> _use core_type <*> position
                )

            let type_declaration =
                Named.p "type_declaration" begin
                    Named.p "type_declaration:mankind" (
                        mapping begin fun name params manifest priv kind cstrs ->
                            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
                            mk_helper (Type.mk
                                ?docs:None ?text:None ?params ?cstrs ?kind:(Some kind) ?priv ?manifest:(Some manifest)) name
                        end
                        <*> loc l_ident <*>? (ng >> type_decl_params) << _s"=" <*> _use core_type
                        << _s"=" <*>? (ng >> k"private") << ng <*> type_kind <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:manifest" (
                        mapping begin fun name params priv manifest cstrs ->
                            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
                            mk_helper (Type.mk
                                ?docs:None ?text:None ?params ?cstrs ?kind:None ?priv ?manifest:(Some manifest)) name
                        end
                        <*> loc l_ident <*>? (ng >> type_decl_params) <* _s"=" <*>? _k"private" <*> _use core_type
                        <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:kind" (
                        mapping begin fun name params priv kind cstrs ->
                            let priv = Base.Option.map priv ~f:(fun _ -> Asttypes.Private) in
                            mk_helper (Type.mk
                                ?docs:None ?text:None ?params ?cstrs ?kind:(Some kind) ?priv ?manifest:None) name
                        end
                        <*> loc l_ident <*>? (ng >> type_decl_params)
                        << _s"=" <*>? _k"private" << ng <*> type_kind <*>? type_decl_constraints
                    )
                    <|>
                    Named.p "type_declaration:abstract" (
                        mapping begin fun name params ->
                            mk_helper (Type.mk
                                ?docs:None ?text:None ?params ?cstrs:None ?kind:None ?priv:None ?manifest:None) name
                        end
                        <*> loc l_ident <*>? (ng >> type_decl_params)
                    )
                end

            let _extension_kind =
                let extension_kind =
                    (constr_args >>| fun x -> Pext_decl (x, None))
                    <|>
                    (s"=" >> _loc u_longident >>| fun x -> Pext_rebind x)
                    <|>
                    (s":" >> _use core_type >>| fun x -> Pext_decl (Pcstr_tuple [], Some x))
                in

                Named.p "typexr:constructor:kind" begin
                    (ng >> extension_kind)
                    <|>
                    return @@ Pext_decl (Pcstr_tuple [], None)
                end

            let type_extension_constructor =
                Named.p "typext:constructor" @@ add_attrs begin
                    mapping (mk_helper2 (Te.constructor ?docs:None ?info:None))
                    <*> loc u_ident <*> _extension_kind
                end

            let type_extension =
                Named.p "typext" @@ add_attrs begin
                    mapping begin fun name params priv hd tail ->
                        let priv = Opt.set Private priv in
                        helper @@ fun ~p1:_ ~p2:_ ~attrs ->
                            Te.mk ~attrs ?params ?priv name (hd::tail)
                    end
                    <* k"type" <*> _loc l_longident <*>? type_decl_params <* _s"+=" <*>? _k"private"
                    <* opt @@ _s"|" <*> _use type_extension_constructor <*>* (_s"|" >> _use type_extension_constructor)
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
