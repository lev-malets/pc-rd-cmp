
open Sigs
open Parsetree
open Ast_helper
open Basic
open Basic.Angstrom

module Make
        (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser)
        (Utils: UTILS) (Core: CORE) (Type: TYPE) (Expression: EXPRESSION)
        : MODEXPR = struct

    open Utils
    open Core
    open Type
    open Expression

    let x = fix_poly @@ fun getter ->
        (module struct
            let modexpr = getter.get @@ fun (module M: MODEXPR) -> M.modexpr
            let _modtype = getter.get @@ fun (module M: MODEXPR) -> M.modtype
            let modtype_functor = getter.get @@ fun (module M: MODEXPR) -> M.modtype_functor
            let modtype_with = getter.get @@ fun (module M: MODEXPR) -> M.modtype_with

            let modtype_atom =
                Named.p "modtype:atom" begin
                        (s_"(" >> modtype_with << _s")")
                    <|> (mapping (mk_helper ~f:Mty.signature) <* s_"{" <*> signature <* _s"}")
                    <|> (mapping (mk_helper ~f:Mty.extension) <*> extension)
                    <|> (mapping (mk_helper ~f:Mty.typeof_) <* k"module" <* _k"type" <* _k"of" <*> _use modexpr)
                    <|> (mapping (mk_helper ~f:Mty.ident) <*> loc longident)
                end

            let modtype = add_attrs @@ modtype_atom

            let modtype_functor =
                let _tail = _s"=>" >> _use modtype_functor in

                let with_functor_args = fix @@ fun with_functor_args ->
                    let tail =
                            (s")" >> _tail)
                        <|> (s"," >> _s")" >> _tail)
                        <|> (s"," >> _use with_functor_args)
                    in

                        add_attrs (
                            mapping (mk_helper3 Mty.functor_)
                            <*> loc u_ident <* _s":" <*> (_use modtype >>| some) <* ng <*> tail
                        )
                    <|> add_attrs (
                            mapping (mk_helper3 Mty.functor_)
                            <*> loc (s"_" >>$ "_") <*>? (_s":" >> _use modtype) <* ng <*> tail
                        )
                    <|> (
                            mapping (mk_helper3 Mty.functor_ @@ Location.mknoloc "_")
                            <*> (_use modtype >>| some) <* ng <*> tail
                        )
                in

                Named.p "modtype:functor" begin
                        add_attrs (s"(" >> ng >> add_attrs with_functor_args)
                    <|> add_attrs (
                            mapping (fun a b -> mk_helper3 Mty.functor_ a None b)
                            <*> loc (s"()" >>$ "*") <*> _tail
                        )
                    <|> add_attrs (
                            mapping (mk_helper3 Mty.functor_ @@ Location.mknoloc "_")
                            <*> (use modtype >>| some) <*> _tail
                        )
                    <|> modtype
                end

            let modtype_with =
                let type_decl eq =
                    mapping begin fun params manifest cstrs name ->
                        mk_helper ~f:(Ast_helper.Type.mk
                            ?docs:None ?text:None ?params ?cstrs ?kind:None ?priv:None ?manifest:(Some manifest)) name
                    end
                    <*>? type_decl_params <* _s eq <*> _use core_type <*>? type_decl_constraints
                in

                let with_constraint =
                    Named.p "with constraint" begin
                        begin
                            mapping begin fun name decl ->
                                let str = match [@warning "-8"] name.Location.txt with
                                    | Longident.Lident s -> s
                                    | Ldot (_, s) -> s
                                in
                                let decl = decl @@ Location.mkloc str name.loc in
                                Pwith_type (name, apply decl name.loc.loc_start name.loc.loc_end)
                            end
                            << k"type" << ng <*> loc l_longident << ng <*> type_decl "="
                        end
                        <|>
                        begin
                            mapping begin fun name decl ->
                                let str = match [@warning "-8"] name.Location.txt with
                                    | Longident.Lident s -> s
                                    | Ldot (_, s) -> s
                                in
                                let decl = decl @@ Location.mkloc str name.loc in
                                Pwith_typesubst (name, apply decl name.loc.loc_start name.loc.loc_end)
                            end
                            << k"type" << ng <*> loc l_longident << ng <*> type_decl ":="
                        end
                        <|>
                        begin
                            mapping begin fun m1 m2 -> Pwith_module (m1, m2) end
                            << k"module" << ng <*> loc u_longident << _s"=" << ng <*> loc u_longident
                        end
                        <|>
                        begin
                            mapping begin fun m1 m2 -> Pwith_modsubst (m1, m2) end
                            << k"module" << ng <*> loc u_longident << _s":=" << ng <*> loc u_longident
                        end
                    end
                in

                mapping begin fun p1 typ p2 constrs ->
                    let typ = apply typ p1 p2 in
                    mk_helper2 Mty.with_ typ constrs
                end
                <*> pos <*> modtype_functor <*> pos <* _k"with" <*> seq 1 ~sep:(_k"and") (ng >> with_constraint)

            let modtype_with =
                Named.p "modtype:with" @@
                (modtype_with <|> modtype_functor)

            let modexpr_constrainted =
                Named.p "modexpr:constrainted" @@
                fold_hlp_left_0_1 ~f:(mk_helper2 Mod.constraint_)
                    modexpr (_s":" >> _use modtype)

            let atom =
                let unpack_constr =
                    fold_hlp_left_0_1 ~f:(mk_helper2 Exp.constraint_)
                        expression (_s":" >> _use core_type_package)
                in
                    (mapping (mk_helper ~f:Mod.unpack) <* k"unpack" <* _s"(" <*> _use unpack_constr <* _s")")
                <|> (mapping (mk_helper ~f:Mod.ident) <*> loc u_longident)
                <|> (s_"(" >> add_attrs modexpr_constrainted << _s")")
                <|> (mapping (mk_helper ~f:Mod.extension) <*> extension)

            let apply =
                let genarg =
                    mapping (mk_helper ~f:Mod.structure [])
                    <* s"(" <* _s")"
                in

                let arg = genarg <|> modexpr_constrainted in

                let args =
                    fold_left_cont_0_n
                        (
                            mapping begin fun arg p2 ->
                                (fun prev -> mk_helper2 Mod.apply prev arg), p2
                            end
                            <*> _use arg <*> pos
                        )
                        (
                            mapping begin fun arg p3 (cont, p2) ->
                                begin fun prev ->
                                    let prev = apply (cont prev) prev.pmod_loc.loc_start p2 in
                                    mk_helper2 Mod.apply prev arg
                                end, p3
                            end
                            <* _s"," <*> _use arg <*> pos
                        )
                    >>| fun (x, _) -> x
                in

                let args =
                    (s"(" *> args <* opt (_s",") <* _s")")
                    <|>
                    (s"(" >> _s")" >>$ fun prev -> mk_helper2 Mod.apply prev (Mod.structure []))
                in

                add_attrs @@ (
                        fold_left_cont_0_n
                        (
                            mapping t3
                            <*> pos <*> atom <*> pos
                        )
                        (
                            mapping begin fun cont p3 (p1, expr, p2) ->
                                p1, cont @@ apply expr p1 p2, p3
                            end
                            <*> args <*> pos
                        )
                        >>| fun (_, x, _) -> x
                )

            let functor_ =
                let _tail =
                    mapping begin fun mt me ->
                        Mod.constraint_ ~loc:me.pmod_loc me mt
                    end
                    <* _s":" <*> _use modtype <* _s"=>" <*> _use modexpr
                    <|>
                    (_s"=>" >> _use modexpr)
                in

                let arg_loop = fix @@ fun arg_loop ->

                    let _tail =
                            (opt (s",") >> _s")" >> _tail)
                        <|> (s"," >> _use arg_loop)
                    in

                    add_attrs (
                            (mapping (mk_helper3 Mod.functor_) <*> loc u_ident <*>? (_s":" >> _use modtype) <*> _tail)
                        <|> (mapping (fun a b -> mk_helper3 Mod.functor_ a None b) <*> loc (s"()" >>$ "*") <*> _tail)
                    )
                in

                add_attrs
                (
                    (mapping (fun a b -> mk_helper3 Mod.functor_ a None b) <*> loc (s"()" >>$ "*") <*> _tail)
                    <|>
                    (s"(" >> ng >> arg_loop)
                )

            let structure =
                mapping (mk_helper ~f:Mod.structure)
                <* s_"{" <*> structure <* _s"}"

            let modexpr =
                Named.p "modexpr" begin
                    functor_ <|> apply <|> structure
                end
        end : MODEXPR)

    let modexpr = let (module M) = x in M.modexpr
    let modexpr_constrainted = let (module M) = x in M.modexpr_constrainted
    let modtype = let (module M) = x in M.modtype
    let modtype_functor = let (module M) = x in M.modtype_functor
    let modtype_with = let (module M) = x in M.modtype_with
end
