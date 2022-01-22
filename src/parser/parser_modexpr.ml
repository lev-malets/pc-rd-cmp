open Base
open Sigs
open Parsetree
open Ast_helper
open Basic
open APos

module Make
        (Ext: EXT)
        (Utils: UTILS) (Core: CORE) (Type: TYPE) (Expression: EXPRESSION)
        : MODEXPR = struct

    open Ext
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
                    Peek.first
                    [ s"(" >> -modtype_with << -s")"
                    ; mapping (mk_helper ~f:Mty.signature) << s"{" <*> -signature << -s"}"
                    ; mapping (mk_helper ~f:Mty.extension) <*> extension
                    ; mapping (mk_helper ~f:Mty.typeof_) << k"module" << -k"type" << -k"of" <*> -use modexpr
                    ; mapping (mk_helper ~f:Mty.ident) <*> loc longident
                    ]
                end

            let modtype = add_attrs @@ modtype_atom

            let modtype_functor =
                let _tail = -s"=>" >> -use modtype_functor in

                let with_functor_args = fix @@ fun with_functor_args ->
                    let tail =
                            (s")" >> _tail)
                        <|> (s"," >> -s")" >> _tail)
                        <|> (s"," >> -use with_functor_args)
                    in

                        add_attrs (
                            mapping (mk_helper3 Mty.functor_)
                            <*> loc u_ident << -s":" <*> (-use modtype >>| some) << ng <*> tail
                        )
                    <|> add_attrs (
                            mapping (mk_helper3 Mty.functor_)
                            <*> loc (s"_" >>$ "_") <*>? (-s":" >> -use modtype) << ng <*> tail
                        )
                    <|> (
                            mapping (mk_helper3 Mty.functor_ @@ Location.mknoloc "_")
                            <*> (-use modtype >>| some) << ng <*> tail
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
                    <*>? type_decl_params << -s eq <*> -use core_type <*>? type_decl_constraints
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
                            << k"module" << ng <*> loc u_longident << -s"=" << ng <*> loc u_longident
                        end
                        <|>
                        begin
                            mapping begin fun m1 m2 -> Pwith_modsubst (m1, m2) end
                            << k"module" << ng <*> loc u_longident << -s":=" << ng <*> loc u_longident
                        end
                    end
                in

                mapping begin fun p1 typ p2 constrs ->
                    let typ = apply typ p1 p2 in
                    mk_helper2 Mty.with_ typ constrs
                end
                <*> pos <*> modtype_functor <*> pos << -k"with" <*> seq 1 ~sep:(-k"and") (-with_constraint)

            let modtype_with =
                Named.p "modtype:with"
                (modtype_with <|> modtype_functor)

            let modexpr_constrainted =
                Named.p "modexpr:constrainted" @@
                fold_hlp_left_0_1 ~f:(mk_helper2 Mod.constraint_)
                    modexpr (-s":" >> -use modtype)

            let atom =
                Named.p "modexpr:atom" begin
                    let unpack_constr =
                        fold_hlp_left_0_1 ~f:(mk_helper2 Exp.constraint_)
                            expression (-s":" >> -use core_type_package)
                    in

                    Peek.first
                    [ mapping (mk_helper ~f:Mod.unpack) << k"unpack" << -s"(" <*> -use unpack_constr << -s")"
                    ; mapping (mk_helper ~f:Mod.ident) <*> loc u_longident
                    ; s"(" >> -add_attrs modexpr_constrainted << -s")"
                    ; mapping (mk_helper ~f:Mod.extension) <*> extension
                    ]
                end

            let apply =
                Named.p "modexpr:apply" begin
                    let genarg =
                        mapping (mk_helper ~f:Mod.structure [])
                        << s"(" << -s")"
                    in

                    let arg = genarg <|> modexpr_constrainted in

                    let args =
                        fold_left_cont_0_n
                            (
                                mapping begin fun arg p2 ->
                                    (fun prev -> mk_helper2 Mod.apply prev arg), p2
                                end
                                <*> -use arg <*> pos
                            )
                            (
                                mapping begin fun arg p3 (cont, p2) ->
                                    begin fun prev ->
                                        let prev = apply (cont prev) prev.pmod_loc.loc_start p2 in
                                        mk_helper2 Mod.apply prev arg
                                    end, p3
                                end
                                << -s"," <*> -use arg <*> pos
                            )
                        >>| fun (x, _) -> x
                    in

                    let args =
                            (s"(" >> args << opt (-s",") << -s")")
                        <|> (s"(" >> -s")" >>$ fun prev -> mk_helper2 Mod.apply prev (Mod.structure []))
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
                end

            let functor_ =
                Named.p "modexpr:functor" begin
                    let tail =
                            (
                                mapping begin fun mt me ->
                                    Mod.constraint_ ~loc:me.pmod_loc me mt
                                end
                                << s":" <*> -use modtype << -s"=>" <*> -use modexpr
                            )
                        <|> (s"=>" >> -use modexpr)
                    in

                    let arg_loop = fix @@ fun arg_loop ->
                        let tail =
                                (opt (s",") >> -s")" >> -tail)
                            <|> (s"," >> -use arg_loop)
                        in

                        add_attrs (
                                (
                                    mapping (mk_helper3 Mod.functor_)
                                    <*> loc u_ident <*>? (-s":" >> -use modtype) <*> -tail
                                )
                            <|> (
                                    mapping (fun a b -> mk_helper3 Mod.functor_ a None b)
                                    <*> loc (s"()" >>$ "*") <*> -tail
                                )
                        )
                    in

                    add_attrs
                    (
                            (
                                mapping (fun a b -> mk_helper3 Mod.functor_ a None b)
                                <*> loc (s"()" >>$ "*") <*> -tail
                            )
                        <|> (s"(" >> -arg_loop)
                    )
                end

            let structure =
                Named.p "modexpr:structure" begin
                    mapping (mk_helper ~f:Mod.structure)
                    << s"{" <*> -structure << -s"}"
                end

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
