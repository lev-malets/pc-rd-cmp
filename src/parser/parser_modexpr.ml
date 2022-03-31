open Base
open Sigs
open Parsetree
open Ast_helper
open Basic

module Make
        (Basic: BASIC)
        (Core: CORE with module Comb = Basic.Comb)
        (Type: TYPE with module Comb = Basic.Comb)
        (Expression: EXPRESSION with module Comb = Basic.Comb)
        (Modtype: MODTYPE with module Comb = Basic.Comb)
        = struct
    open Basic
    open Core
    open Type
    open Expression
    open Modtype
    open Comb

    module Comb = Comb
    module type THIS = MODEXPR with module Comb = Basic.Comb

    let x = fix_poly @@ fun getter ->
        (module struct
            module Comb = Comb

            let modexpr = getter.get @@ fun (module M: THIS) -> M.modexpr

            let modexpr_constrainted =
                named "modexpr:constrainted" @@
                fold_left_cont_0_1
                    modexpr
                    (
                        mapping begin fun typ prev ->
                            Mod.constraint_ ~loc:(loc_comb prev.pmod_loc typ.pmty_loc) prev typ
                        end
                        -ng -colon -ng +modtype
                    )

            let atom =
                named "modexpr:atom" begin
                    let unpack_constr =
                        fold_left_cont_0_1
                            expression
                            (
                                mapping begin fun typ prev ->
                                    Exp.constraint_ ~loc:(loc_comb prev.pexp_loc typ.ptyp_loc) prev typ
                                end
                                -ng -colon -ng +core_type_package
                            )
                    in

                    peek_first
                    [
                        with_loc & hlp Mod.unpack
                        -unpack -ng -l_paren -ng +unpack_constr -ng -r_paren
                    ;
                        with_loc & hlp Mod.ident
                        +loc u_longident
                    ;
                        with_loc & parens & mapping mod_loc
                        +mod_attrs modexpr_constrainted
                    ;
                        with_loc & hlp Mod.extension
                        +extension
                    ]
                end

            let apply =
                named "modexpr:apply" begin
                    let genarg =
                        with_loc & mapping (fun loc -> Mod.structure ~loc [])
                        -l_paren -ng -r_paren
                    in

                    let arg = genarg <|> modexpr_constrainted in

                    let args =
                        fold_left_cont_0_n
                            (
                                mapping begin fun arg prev ->
                                    Mod.apply
                                        ~loc:(loc_comb prev.pmod_loc arg.pmod_loc)
                                        prev arg
                                end
                                +arg
                            )
                            (
                                mapping begin fun arg cont prev ->
                                    let prev = cont prev in
                                    Mod.apply
                                        ~loc:(loc_comb prev.pmod_loc arg.pmod_loc)
                                        prev arg
                                end
                                -ng -comma -ng +arg
                            )
                    in

                    let args =
                        choice [
                            l_paren >> args << opt sep << ng << r_paren
                        ;
                            mapping begin fun loc_end prev ->
                                Mod.apply ~loc:{prev.pmod_loc with loc_end} prev (Mod.structure [])
                            end
                            -l_paren -ng -r_paren  +pos
                        ]
                    in

                    mod_attrs & fold_left_cont_0_n atom args
                end

            let functor_ =
                named "modexpr:functor" begin
                    let tail =
                        choice [
                            mapping begin fun mt me ->
                                Mod.constraint_ ~loc:me.pmod_loc me mt
                            end
                            -colon -ng +modtype -ng -arrow -ng +modexpr
                        ;
                            arrow >> ng >> modexpr
                        ]
                    in

                    let arg_loop = fix @@ fun arg_loop ->
                        let tail =
                            choice
                            [ opt comma >> ng >> r_paren >> ng >> tail
                            ; comma >> ng >> arg_loop
                            ]
                        in

                        choice [
                            mod_attrs & with_loc & hlp3 Mod.functor_
                            +loc u_ident +opt(ng >> colon >> ng >> modtype) -ng +tail
                        ;
                            mod_attrs & with_loc & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
                            +loc (l_paren >> ng >> r_paren >>$ "*") -ng +tail
                        ]
                    in

                    choice [
                        mod_attrs & with_loc & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
                        +loc (l_paren >> ng >> r_paren >>$ "*") -ng +tail
                    ;
                        mod_attrs &
                        l_paren >> ng >> arg_loop
                    ]
                end

            let structure =
                named "modexpr:structure" begin
                    with_loc & hlp Mod.structure
                    -l_brace -ng +structure -ng -r_brace
                end

            let modexpr =
                named "modexpr" begin
                    functor_ <|> apply <|> structure
                end
        end : THIS)

    let modexpr = let (module M) = x in M.modexpr
    let modexpr_constrainted = let (module M) = x in M.modexpr_constrainted
end
