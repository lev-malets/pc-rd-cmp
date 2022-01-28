open Base
open Sigs
open Parsetree
open Ast_helper
open Basic
open APos

module Make
        (Ext: EXT)
        (Utils: UTILS) (Core: CORE) (Type: TYPE) (Expression: EXPRESSION) (Modexpr: MODEXPR)
        : MODTYPE = struct

    open Ext
    open Utils
    open Core
    open Type
    open Modexpr

    let x = fix_poly @@ fun getter ->
        (module struct
            let _modtype = getter.get @@ fun (module M: MODTYPE) -> M.modtype
            let modtype_functor = getter.get @@ fun (module M: MODTYPE) -> M.modtype_functor
            let modtype_with = getter.get @@ fun (module M: MODTYPE) -> M.modtype_with

            let modtype_atom =
                Named.p "modtype:atom" begin
                    Peek.first
                    [
                        with_loc & parens & mapping mty_loc
                        >$modtype_with
                    ;
                        with_loc & hlp Mty.signature
                        >s"{" >ng >$signature >ng >s"}"
                    ;
                        with_loc & hlp Mty.extension
                        >$extension
                    ;
                        with_loc & hlp Mty.typeof_
                        >k"module" >ng >k"type" >ng >k"of" >ng >$modexpr
                    ;
                        with_loc & hlp Mty.ident
                        >$loc longident
                    ]
                end

            let modtype = mty_attrs modtype_atom

            let modtype_functor =
                let _tail = ng >> s"=>" >> ng >> modtype_functor in

                let with_functor_args = fix @@ fun with_functor_args ->
                    let tail =
                            s")" >> _tail
                        ||  s"," >> ng >> s")" >> _tail
                        ||  s"," >> ng >> with_loc with_functor_args
                    in

                        mapping (fun attrs n typ tail loc -> Mty.functor_ ~loc ~attrs n (Some typ) tail)
                        >$attrs_ >$loc u_ident >ng >s":" >ng >$modtype >ng >$tail

                    ||  mapping (fun attrs n typ tail loc -> Mty.functor_ ~loc ~attrs n typ tail)
                        >$attrs_ >$loc (s"_" >>$ "_") >?(ng >> s":" >> ng >> modtype) >ng >$tail

                    ||  mapping (fun attrs typ tail loc -> Mty.functor_ ~loc ~attrs (Location.mknoloc "_") (Some typ) tail)
                        >$attrs_ >ng >$modtype >ng >$tail
                in

                Named.p "modtype:functor" begin
                        mty_attrs & with_loc &
                        s"(" >> ng >> with_functor_args

                    ||  mty_attrs & with_loc & mapping (fun a b loc -> Mty.functor_ ~loc a None b)
                        >$loc (s"()" >>$ "*") >$_tail

                    ||  mty_attrs & with_loc & mapping (fun a b loc -> Mty.functor_ ~loc (Location.mknoloc "_") (Some a) b)
                        >$modtype >$_tail

                    ||  modtype
                end

            let modtype_with =
                let type_decl eq =
                    mapping begin fun params manifest cstrs p2 name p1 ->
                        let loc = make_location p1 p2 in
                        Ast_helper.Type.mk ~loc
                            ?docs:None ?text:None ?params ?cstrs ?kind:None ?priv:None ~manifest name
                    end
                    >?type_decl_params >ng >s eq >ng >$core_type >?(ng >> type_decl_constraints) >$pos
                in

                let with_constraint =
                    Named.p "with constraint" begin
                            mapping begin fun name decl ->
                                let str = match [@warning "-8"] name.Location.txt with
                                    | Longident.Lident s -> s
                                    | Ldot (_, s) -> s
                                in
                                let decl = decl @@ Location.mkloc str name.loc in
                                Pwith_type (name, decl name.loc.loc_start)
                            end
                            >k"type" >ng >$loc l_longident >ng >$type_decl "="

                        ||  mapping begin fun name decl ->
                                let str = match [@warning "-8"] name.Location.txt with
                                    | Longident.Lident s -> s
                                    | Ldot (_, s) -> s
                                in
                                let decl = decl {name with txt = str} in
                                Pwith_typesubst (name, decl name.loc.loc_start)
                            end
                            >k"type" >ng >$loc l_longident >ng >$type_decl ":="

                        ||  mapping begin fun m1 m2 -> Pwith_module (m1, m2) end
                            >k"module" >ng >$loc u_longident >ng >s"=" >ng >$loc u_longident

                        ||  mapping begin fun m1 m2 -> Pwith_modsubst (m1, m2) end
                            >k"module" >ng >$loc u_longident >ng >s":=" >ng >$loc u_longident
                    end
                in

                with_loc & hlp2 Mty.with_
                >$modtype_functor >ng >k"with" >$(seq 1 ~sep:(ng >> k"and") (ng >> with_constraint))

            let modtype_with =
                Named.p "modtype:with"
                (modtype_with <|> modtype_functor)
        end : MODTYPE)

    let modtype = let (module M) = x in M.modtype
    let modtype_functor = let (module M) = x in M.modtype_functor
    let modtype_with = let (module M) = x in M.modtype_with
end
