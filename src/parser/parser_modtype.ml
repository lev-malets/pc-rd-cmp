open Base
open Sigs
open Parsetree
open Ast_helper
open Basic

module Make
    (Basic : BASIC)
    (Core : CORE with module Comb = Basic.Comb)
    (Type : TYPE with module Comb = Basic.Comb)
    (Expression : EXPRESSION with module Comb = Basic.Comb)
    (Modexpr : MODEXPR with module Comb = Basic.Comb) =
struct
  open Basic
  open Core
  open Type
  open Modexpr
  open Pc
  open Comb
  module Comb = Comb

  module type THIS = MODTYPE with module Comb = Basic.Comb

  let x =
    fix_poly @@ fun getter : (module THIS) ->
    (module struct
      module Comb = Comb

      let _modtype = getter.get @@ fun (module M : THIS) -> M.modtype

      let modtype_functor = getter.get @@ fun (module M) -> M.modtype_functor

      let modtype_with = getter.get @@ fun (module M) -> M.modtype_with

      let modtype_atom =
        choice ~name:"modtype:atom"
          [
            with_loc & parens & (mapping mty_loc + modtype_with);
            with_loc & (hlp Mty.signature - l_brace - ng + signature - ng - r_brace);
            with_loc & (hlp Mty.extension + extension);
            with_loc & (hlp Mty.typeof_ - module' - ng - type' - ng - of' - ng + modexpr);
            with_loc & (hlp Mty.ident + loc longident);
          ]

      let modtype = mty_attrs modtype_atom

      let modtype_functor =
        let tail = arrow >> ng >> modtype_functor in

        let with_functor_args =
          fix @@ fun with_functor_args ->
          let tail =
            choice
              [ r_paren >> ng >> tail; comma >> ng >> r_paren >> ng >> tail; comma >> ng >> with_loc with_functor_args ]
          in

          choice
            [
              mapping (fun attrs n typ tail loc -> Mty.functor_ ~loc ~attrs n (Some typ) tail)
              + attrs_ + loc u_ident - ng - colon - ng + modtype - ng + tail;
              mapping (fun attrs n typ tail loc -> Mty.functor_ ~loc ~attrs n typ tail)
              + attrs_
              + loc (_' >>$ "_")
              + opt (ng >> colon >> ng >> modtype)
              - ng + tail;
              mapping (fun attrs typ tail loc -> Mty.functor_ ~loc ~attrs (Location.mknoloc "_") (Some typ) tail)
              + attrs_ - ng + modtype - ng + tail;
            ]
        in

        named "modtype:functor"
        & choice
            [
              mty_attrs & with_loc & l_paren >> ng >> with_functor_args;
              mty_attrs & with_loc
              & mapping (fun a b loc -> Mty.functor_ ~loc a None b)
                + loc (l_paren >> ng >> r_paren >>$ "*")
                - ng + tail;
              mty_attrs & with_loc
              & (mapping (fun a b loc -> Mty.functor_ ~loc (Location.mknoloc "_") (Some a) b) + modtype - ng + tail);
              modtype;
            ]

      let modtype_with =
        let type_decl eq =
          mapping (fun params manifest cstrs p2 name p1 ->
              let loc = loc_mk p1 p2 in
              Ast_helper.Type.mk ~loc ?docs:None ?text:None ?params ?cstrs ?kind:None ?priv:None ~manifest name)
          + opt type_decl_params - ng - eq - ng + core_type
          + opt (ng >> type_decl_constraints)
          + pos
        in

        let with_constraint =
          named "with constraint"
          & choice
              [
                mapping (fun name decl ->
                    let str =
                      match[@warning "-8"] name.Location.txt with Longident.Lident s -> s | Ldot (_, s) -> s
                    in
                    let decl = decl @@ Location.mkloc str name.loc in
                    Pwith_type (name, decl name.loc.loc_start))
                - type' - ng + loc l_longident - ng + type_decl eq;
                mapping (fun name decl ->
                    let str =
                      match[@warning "-8"] name.Location.txt with Longident.Lident s -> s | Ldot (_, s) -> s
                    in
                    let decl = decl { name with txt = str } in
                    Pwith_typesubst (name, decl name.loc.loc_start))
                - type' - ng + loc l_longident - ng + type_decl colon_eq;
                mapping (fun m1 m2 -> Pwith_module (m1, m2))
                - module' - ng + loc u_longident - ng - eq - ng + loc u_longident;
                mapping (fun m1 m2 -> Pwith_modsubst (m1, m2))
                - module' - ng + loc u_longident - ng - colon_eq - ng + loc u_longident;
              ]
        in

        with_loc & (hlp2 Mty.with_ + modtype_functor - ng - with' + seq ~n:1 ~sep:(ng >> and') (ng >> with_constraint))

      let modtype_with = named "modtype:with" (modtype_with <|> modtype_functor)
    end)

  let modtype =
    let (module M) = x in
    M.modtype

  let modtype_functor =
    let (module M) = x in
    M.modtype_functor

  let modtype_with =
    let (module M) = x in
    M.modtype_with
end
