open Base
open Sigs
open Compilerlibs406
open Asttypes
open Parsetree
open Ast_helper
open Basic

module Make (Basic : BASIC) (Core : CORE with module Comb = Basic.Comb) = struct
  open Basic
  open Core
  open Pc
  open Comb
  module Comb = Comb

  module type THIS = TYPE with module Comb = Basic.Comb

  let x =
    fix_gen
    @@ fun getter : (module THIS) ->
    (module struct
      module Comb = Comb

      let core_type_atom =
        getter.get @@ fun (module M : THIS) -> M.core_type_atom

      let core_type_fun = getter.get @@ fun (module M) -> M.core_type_fun
      let core_type = getter.get @@ fun (module M) -> M.core_type
      let core_type_poly = getter.get @@ fun (module M) -> M.core_type_poly

      let core_type_package =
        named "typexpr:package"
        & choice
            [ with_loc
              & hlp2 Typ.package <*> loc u_longident <<. with'
                <*> seq ~n:1 ~sep:(ng >> and')
                      (t2 <<. type' <*>. loc l_longident <<. eq
                     <*>. core_type_atom)
            ; with_loc
              & mapping (fun a loc -> Typ.package ~loc a []) <*> loc u_longident
            ]

      let var = with_loc & hlp Typ.var <*> type_var
      let any = with_loc & mapping (fun loc -> Typ.any ~loc ()) << _'

      let constr =
        named "typexpr:constr"
        & choice
            [ with_loc
              & hlp2 Typ.constr <*> loc l_longident <<. lt << ng
                <*> seq ~n:1 core_type ~sep ~trail:true
                <<. gt
            ; with_loc
              & mapping (fun a loc -> Typ.constr ~loc a []) <*> loc l_longident
            ]

      let tuple =
        with_loc & parens
        & hlp Typ.tuple <*> seq ~n:2 core_type ~sep ~trail:true

      let unit =
        with_loc
        & mapping (fun a loc -> Typ.constr ~loc a [])
          <*> loc (l_paren >> ng >> r_paren >>$ Longident.Lident "unit")

      let bs_object =
        let object_field =
          choice
            [ mapping (fun attrs name typ -> Otag (name, attrs, typ))
              <*> attrs_ <*> loc string_raw <<. colon << ng <*> core_type_poly
            ; mapping (fun typ -> Oinherit typ) << ellipsis <*>. core_type ]
        in
        named "typexpr:bso"
        & choice
            [ with_loc
              & mapping (fun loc -> Typ.object_ ~loc [] Closed)
                << l_brace <<. dot <<. r_brace
            ; with_loc
              & mapping (fun a loc -> Typ.object_ ~loc a Closed)
                << l_brace << ng
                <*> seq object_field ~sep ~trail:true
                <<. r_brace
            ; with_loc
              & mapping (fun a loc -> Typ.object_ ~loc a Open)
                << l_brace <<. dot_dot << ng
                <*> seq object_field ~sep ~trail:true
                <<. r_brace
            ; with_loc
              & mapping (fun a loc -> Typ.object_ ~loc a Closed)
                << l_brace <<. dot << ng
                <*> seq object_field ~sep ~trail:true
                <<. r_brace ]

      let variant =
        named "typexpr:variant"
          (let constructor_arguments =
             choice [tuple; l_paren >> ng >> core_type <<. r_paren]
           in
           let row_field =
             choice
               [ mapping (fun attrs tag empty constrs ->
                     Rtag (tag, attrs, Option.is_some empty, constrs))
                 <*> attrs_ <*> loc variant_tag << ng
                 <*> opt (ampersand << ng)
                 <*> seq ~n:1 constructor_arguments ~sep:(ng << ampersand << ng)
               ; mapping (fun attrs tag -> Rtag (tag, attrs, true, []))
                 <*> attrs_ <*> loc variant_tag
               ; (core_type >>| fun x -> Rinherit x) ]
           in
           let rows =
             opt (pipe << ng) >> seq ~n:1 row_field ~sep:(ng << pipe << ng)
           in
           choice
             [ with_loc
               & mapping (fun loc -> Typ.variant ~loc [] Open None)
                 << l_bracket <<. gt <<. r_bracket
             ; with_loc
               & mapping (fun list loc -> Typ.variant ~loc list Open None)
                 << l_bracket <<. gt <*>. rows <<. r_bracket
             ; with_loc
               & mapping (fun list tags loc ->
                     let tags = if Option.is_some tags then tags else Some [] in
                     Typ.variant ~loc list Closed tags)
                 << l_bracket <<. lt <*>. rows
                 <*> opt (ng >> gt >> seq ~n:1 (ng >> variant_tag))
                 <<. r_bracket
             ; with_loc
               & mapping (fun list loc -> Typ.variant ~loc list Closed None)
                 << l_bracket <*>. rows <<. r_bracket ])

      let core_type_atom =
        named "typexpr:atom" & typ_attrs
        & choice ~name:"typexpr:atom:noattrs"
            [ any
            ; var
            ; unit
            ; with_loc & parens & mapping typ_loc <*> core_type
            ; tuple
            ; with_loc & mapping typ_loc << module' <*> parens core_type_package
            ; bs_object
            ; constr
            ; variant
            ; with_loc & hlp Typ.extension <*> extension ]

      let alias p =
        fold_left_cont_0_1 p
          (mapping (fun x loc_end prev ->
               Typ.alias ~loc:{prev.ptyp_loc with loc_end} prev x)
          <<. as' <*>. type_var <*> pos)

      let aliased_atom = named "typexpr:alias:atom" @@ alias core_type_atom

      let arrow =
        let arrow_tail =
          mapping (fun typ label arg ->
              Typ.arrow ~loc:(loc_comb arg.ptyp_loc typ.ptyp_loc) label arg typ)
          << arrow <*>. core_type_fun
        in
        let with_args typ tail =
          choice
            [ typ_attrs
              & mapping (fun tag x opt tail ->
                    let label =
                      if Option.is_some opt then Optional tag.txt
                      else Labelled tag.txt
                    in
                    let x = typ_add_attr "ns.namedArgLoc" ~loc:tag.loc x in
                    tail label x)
                << tilda <*>. loc l_ident <<. colon << ng <*> typ_attrs typ
                <*> opt (ng << eq <<. question)
                <*>. tail
            ; mapping (fun arg tail -> tail Nolabel arg) <*> typ <*>. tail ]
        in
        let tail =
          fix
          @@ fun tail ->
          choice
            [ r_paren >> ng >> arrow_tail
            ; comma >> ng >> r_paren >> ng >> arrow_tail
            ; mapping (fun typ label arg ->
                  let typ = typ_add_attr "bs" typ in
                  Typ.arrow label arg typ)
              << comma <<. dot <*>. with_args core_type tail
            ; mapping (fun typ label arg -> Typ.arrow label arg typ)
              << comma <*>. with_args core_type tail ]
        in
        named "typexpr:arrow:all"
        & choice
            [ typ_attrs
              & l_paren >> ng >> dot >> ng >> with_args core_type tail
                >>| typ_add_attr "bs"
            ; typ_attrs & l_paren >> ng >> with_args core_type tail
            ; named "typexpr:arrow:onearg" (with_args aliased_atom arrow_tail)
            ]

      let core_type_fun = named "typexpr:arrow" @@ choice [arrow; core_type_atom]
      let core_type = named "typexpr" & alias core_type_fun

      let core_type_poly =
        named "typexpr:poly"
        & choice
            [ with_loc
              & hlp2 Typ.poly
                <*> seq ~n:1 (loc type_var) ~sep:ng
                <<. dot <*>. core_type
            ; core_type ]

      let label_declaration =
        named "label_declraration"
          (with_loc
          & mapping (fun attrs mut name q typ loc ->
                let mut = Base.Option.map mut ~f:(fun _ -> Asttypes.Mutable) in
                let typ =
                  match typ with
                  | Some typ -> typ
                  | None ->
                      Typ.constr
                        Location.
                          {txt = Longident.Lident name.txt; loc = name.loc}
                        []
                in
                let attrs =
                  if Option.is_none q then attrs
                  else Hc.attr "ns.optional" :: attrs
                in
                Type.field ~loc ~attrs ?info:None ?mut name typ)
            <*> attrs_
            <*> opt (mutable' << ng)
            <*> loc l_ident
            <*> opt (ng >> question)
            <*> opt (ng >> colon >> ng >> core_type_poly))

      let label_declarations =
        braces & seq ~n:1 label_declaration ~sep ~trail:true

      let constr_args =
        choice
          [ parens
            & mapping (fun x -> Pcstr_record x)
              <*> label_declarations << opt sep
          ; parens
            & mapping (fun x -> Pcstr_tuple x)
              <*> seq ~n:1 core_type ~sep ~trail:true ]

      let type_kind =
        let variant =
          named "type_kind:variant"
            (let constructor =
               with_loc
               & mapping (fun attrs name args res loc ->
                     Type.constructor ~loc ~attrs ?info:None ?args:(Some args)
                       ?res name)
                 <*> attrs_ <*> loc u_ident
                 <*> choice [ng >> constr_args; return @@ Pcstr_tuple []]
                 <*> opt (ng >> colon >> ng >> core_type_atom)
             in
             opt @@ pipe
             >> seq ~n:1 (ng >> constructor) ~sep:(ng >> pipe)
             >>| fun x -> Ptype_variant x)
        in
        let record = label_declarations >>| fun x -> Ptype_record x in
        let open' = dot_dot >>$ Ptype_open in
        named "type_kind" @@ choice [variant; record; open']

      let type_decl_params =
        let variance =
          choice
            [ minus >>$ Contravariant << ng
            ; plus >>$ Covariant << ng
            ; return Invariant ]
        in
        let param =
          mapping (fun v t -> (t, v)) <*> variance <*> choice [any; var]
        in
        chevrons & seq ~n:1 param ~sep ~trail:true

      let type_decl_constraints =
        seq ~n:1 ~sep:ng
          (with_loc
          & t3 << constraint' <*>. core_type_atom <<. eq << ng <*> core_type)

      let type_declaration =
        named "type_declaration"
        & choice
            [ named "type_declaration:mankind"
              & with_loc
              & mapping (fun name params manifest priv kind cstrs loc ->
                    let priv =
                      Base.Option.map priv ~f:(fun _ -> Asttypes.Private)
                    in
                    Type.mk ~loc ?docs:None ?text:None ?params ?cstrs ~kind
                      ?priv ~manifest name)
                <*> loc l_ident << ng
                <*> opt (type_decl_params << ng)
                << eq <*>. core_type <<. eq << ng
                <*> opt (private' << ng)
                <*> type_kind
                <*> opt (ng >> type_decl_constraints)
            ; named "type_declaration:manifest"
              & with_loc
              & mapping (fun name params priv manifest cstrs loc ->
                    let priv =
                      Base.Option.map priv ~f:(fun _ -> Asttypes.Private)
                    in
                    Type.mk ~loc ?params ?cstrs ?priv ~manifest name)
                <*> loc l_ident << ng
                <*> opt (type_decl_params << ng)
                << eq << ng
                <*> opt (private' << ng)
                <*> core_type
                <*> opt (ng >> type_decl_constraints)
            ; named "type_declaration:kind"
              & with_loc
              & mapping (fun name params priv kind cstrs loc ->
                    let priv =
                      Base.Option.map priv ~f:(fun _ -> Asttypes.Private)
                    in
                    Type.mk ~loc ?params ?cstrs ~kind ?priv name)
                <*> loc l_ident << ng
                <*> opt (type_decl_params << ng)
                << eq << ng
                <*> opt (private' << ng)
                <*> type_kind
                <*> opt (ng >> type_decl_constraints)
            ; named "type_declaration:abstract"
              & with_loc
              & mapping (fun name params loc -> Type.mk ~loc ?params name)
                <*> loc l_ident
                <*> opt (ng >> type_decl_params) ]

      let _extension_kind =
        let extension_kind =
          choice
            [ (constr_args >>| fun x -> Pext_decl (x, None))
            ; (eq >> ng >> loc u_longident >>| fun x -> Pext_rebind x)
            ; (colon >> ng >> core_type
              >>| fun x -> Pext_decl (Pcstr_tuple [], Some x)) ]
        in
        named "typexr:constructor:kind"
        & choice
            [ng >> extension_kind; return @@ Pext_decl (Pcstr_tuple [], None)]

      let type_extension_constructor =
        named "typext:constructor"
          (with_loc
          & hlp2_a (Te.constructor ?docs:None ?info:None)
            <*> attrs_ <*> loc u_ident <*> _extension_kind)

      let type_extension =
        named "typext"
        & mapping (fun attrs name params priv hd tail ->
              let priv = Opt.set Private priv in
              Te.mk ~attrs ?params ?priv name (hd :: tail))
          <*> attrs_ << type' <*>. loc l_longident << ng
          <*> opt (type_decl_params << ng)
          << plus_eq << ng
          <*> opt (private' << ng)
          << opt (pipe << ng)
          <*> type_extension_constructor
          <*> seq (ng >> pipe >> ng >> type_extension_constructor)
    end)

  let core_type_atom =
    let (module M) = x in
    M.core_type_atom

  let core_type_fun =
    let (module M) = x in
    M.core_type_fun

  let core_type =
    let (module M) = x in
    M.core_type

  let core_type_poly =
    let (module M) = x in
    M.core_type_poly

  let core_type_package =
    let (module M) = x in
    M.core_type_package

  let type_extension_constructor =
    let (module M) = x in
    M.type_extension_constructor

  let type_extension =
    let (module M) = x in
    M.type_extension

  let type_decl_params =
    let (module M) = x in
    M.type_decl_params

  let type_decl_constraints =
    let (module M) = x in
    M.type_decl_constraints

  let type_declaration =
    let (module M) = x in
    M.type_declaration
end
