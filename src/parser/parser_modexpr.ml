open Base
open Sigs
open Parsetree
open Ast_helper
open Basic
open APos

module Make
        (Ext: EXT)
        (Utils: UTILS) (Core: CORE) (Type: TYPE) (Expression: EXPRESSION) (Modtype: MODTYPE)
        : MODEXPR = struct

    open Ext
    open Utils
    open Core
    open Type
    open Expression
    open Modtype

    let x = fix_poly @@ fun getter ->
        (module struct
            let modexpr = getter.get @@ fun (module M: MODEXPR) -> M.modexpr

            let modexpr_constrainted =
                Named.p "modexpr:constrainted" @@
                fold_left_cont_0_1
                    modexpr
                    (
                        mapping begin fun typ prev ->
                            Mod.constraint_ ~loc:(comb_location prev.pmod_loc typ.pmty_loc) prev typ
                        end
                        >ng >s":" >ng >$modtype
                    )

            let atom =
                Named.p "modexpr:atom" begin
                    let unpack_constr =
                        fold_left_cont_0_1
                            expression
                            (
                                mapping begin fun typ prev ->
                                    Exp.constraint_ ~loc:(comb_location prev.pexp_loc typ.ptyp_loc) prev typ
                                end
                                >ng >s":" >ng >$core_type_package
                            )
                    in

                    Peek.first
                    [
                        with_loc & hlp Mod.unpack
                        >k"unpack" >ng >s"(" >ng >$unpack_constr >ng >s")"
                    ;
                        with_loc & hlp Mod.ident
                        >$loc u_longident
                    ;
                        with_loc & parens & mapping mod_loc
                        >$mod_attrs modexpr_constrainted
                    ;
                        with_loc & hlp Mod.extension
                        >$ extension
                    ]
                end

            let apply =
                Named.p "modexpr:apply" begin
                    let genarg =
                        with_loc & mapping (fun loc -> Mod.structure ~loc [])
                        >s"(" >ng >s")"
                    in

                    let arg = genarg <|> modexpr_constrainted in

                    let args =
                        fold_left_cont_0_n
                            (
                                mapping begin fun arg prev ->
                                    Mod.apply
                                        ~loc:(comb_location prev.pmod_loc arg.pmod_loc)
                                        prev arg
                                end
                                >$arg
                            )
                            (
                                mapping begin fun arg cont prev ->
                                    let prev = cont prev in
                                    Mod.apply
                                        ~loc:(comb_location prev.pmod_loc arg.pmod_loc)
                                        prev arg
                                end
                                >ng >s"," >ng >$arg
                            )
                    in

                    let args =
                            s"(" >> args << opt sep << ng << s")"

                        ||  mapping begin fun loc_end prev ->
                                Mod.apply ~loc:{prev.pmod_loc with loc_end} prev (Mod.structure [])
                            end
                            >s"(" >ng >s")" >$pos
                    in

                    mod_attrs & fold_left_cont_0_n atom args
                end

            let functor_ =
                Named.p "modexpr:functor" begin
                    let tail =
                            mapping begin fun mt me ->
                                Mod.constraint_ ~loc:me.pmod_loc me mt
                            end
                            >s":" >ng >$modtype >ng >s"=>" >ng >$modexpr

                        ||  s"=>" >> ng >> modexpr
                    in

                    let arg_loop = fix @@ fun arg_loop ->
                        let tail =
                                opt (s",") >> ng >> s")" >> ng >> tail
                            ||  s"," >> ng >> arg_loop
                        in

                            mod_attrs & with_loc & hlp3 Mod.functor_
                            >$loc u_ident >?(ng >> s":" >> ng >> modtype) >ng >$tail

                        ||  mod_attrs & with_loc & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
                            >$loc (s"()" >>$ "*") >ng >$tail
                    in

                        mod_attrs & with_loc & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
                        >$loc (s"()" >>$ "*") >ng >$tail

                    ||  mod_attrs &
                        s"(" >> ng >> arg_loop
                end

            let structure =
                Named.p "modexpr:structure" begin
                    with_loc & hlp Mod.structure
                    >s"{" >ng >$structure >ng >s"}"
                end

            let modexpr =
                Named.p "modexpr" begin
                    functor_ <|> apply <|> structure
                end
        end : MODEXPR)

    let modexpr = let (module M) = x in M.modexpr
    let modexpr_constrainted = let (module M) = x in M.modexpr_constrainted
end
