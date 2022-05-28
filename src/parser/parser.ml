open Base
open Basic
open Parsetree
open Ast_helper
open Asttypes

module Make (BasicBase : Sigs.BASIC_BASE) : Sigs.PARSER = struct
  module Basic = Parser_basic.Make (BasicBase)
  open Basic
  open Pc
  module Comb = Comb
  open Comb

  type parsers = {
    signature : signature Comb.t;
    structure : structure Comb.t;
    payload : payload Comb.t;
    modexpr : (module MODEXPR);
  }

  let parsers =
    fix_poly @@ fun getter ->
    let payload = getter.get @@ fun x -> x.payload in

    let id_payload_pair start =
      mapping (fun id payload ->
          let payload = Option.value ~default:(PStr []) payload in
          (id, payload))
      + loc (start >> attribute_id)
      + opt (failed ng_not_empty >> payload)
    in

    let module Core : CORE with module Comb = Comb = struct
      module Comb = Comb

      let signature = getter.get @@ fun x -> x.signature

      let structure = getter.get @@ fun x -> x.structure

      let attribute = named "attribute" @@ id_payload_pair at

      let extension = named "extension" @@ id_payload_pair percent

      let attrs_ = named "attrs" & seq ~sep:ng ~trail:true attribute

      let attrs1_ = named "attrs1" @@ seq ~n:1 ~sep:ng ~trail:true attribute

      let pat_attrs p =
        mapping (fun loc_start attrs x ->
            match attrs with
            | [] -> x
            | _ ->
                {
                  x with
                  ppat_attributes = x.ppat_attributes @ attrs;
                  ppat_loc = { x.ppat_loc with loc_start };
                })
        + pos + attrs_ + p

      let exp_attrs p =
        mapping (fun loc_start attrs x ->
            match attrs with
            | [] -> x
            | _ ->
                {
                  x with
                  pexp_attributes = x.pexp_attributes @ attrs;
                  pexp_loc = { x.pexp_loc with loc_start };
                })
        + pos + attrs_ + p

      let mod_attrs p =
        mapping (fun loc_start attrs x ->
            match attrs with
            | [] -> x
            | _ ->
                {
                  x with
                  pmod_attributes = x.pmod_attributes @ attrs;
                  pmod_loc = { x.pmod_loc with loc_start };
                })
        + pos + attrs_ + p

      let mty_attrs p =
        mapping (fun loc_start attrs x ->
            match attrs with
            | [] -> x
            | _ ->
                {
                  x with
                  pmty_attributes = x.pmty_attributes @ attrs;
                  pmty_loc = { x.pmty_loc with loc_start };
                })
        + pos + attrs_ + p

      let typ_attrs p =
        mapping (fun loc_start attrs x ->
            match attrs with
            | [] -> x
            | _ ->
                {
                  x with
                  ptyp_attributes = x.ptyp_attributes @ attrs;
                  ptyp_loc = { x.ptyp_loc with loc_start };
                })
        + pos + attrs_ + p
    end in
    let module Type = Parser_type.Make (Basic) (Core) in
    let module Pattern = Parser_pattern.Make (Basic) (Core) (Type) in
    let open Core in
    let open Type in
    let open Pattern in
    let payload =
      named "payload"
      & choice
          [
            parens
            & (mapping (fun x -> PPat (x, None)) - question - ng + pattern);
            parens & (mapping (fun x -> PSig x) - colon - ng - sig' + signature);
            parens & (mapping (fun x -> PTyp x) - colon - ng + core_type);
            parens & (mapping (fun x -> PStr x) + structure);
          ]
    in

    let module Modexpr = struct
      module Comb = Comb

      let modexpr =
        getter.get @@ fun x ->
        let (module M) = x.modexpr in
        M.modexpr

      let modexpr_constrainted =
        getter.get @@ fun x ->
        let (module M) = x.modexpr in
        M.modexpr_constrainted
    end in
    let module Expression =
      Parser_expression.Make (Basic) (Core) (Type) (Pattern) (Modexpr)
    in
    let module Modtype =
      Parser_modtype.Make (Basic) (Core) (Type) (Expression) (Modexpr)
    in
    let module Modexpr =
      Parser_modexpr.Make (Basic) (Core) (Type) (Expression) (Modtype)
    in
    let open Modtype in
    let open Modexpr in
    let open Expression in
    let module_extension =
      named "module extension" @@ id_payload_pair percent_percent
    in

    let module_attribute = named "module attribute" @@ id_payload_pair at_at in

    let top_open =
      named "open" & with_loc
      & mapping (fun attrs override name loc ->
            Opn.mk ~loc ~attrs ?docs:None ?override name)
        + attrs_ - open'
        + opt (bang >>$ Override)
        - ng + loc u_longident
    in

    let top_exception =
      named "top:exception"
      & mapping (fun attrs constr ->
            { constr with pext_attributes = constr.pext_attributes @ attrs })
        + attrs_ - exception' - ng + type_extension_constructor
    in

    let top_include x =
      with_loc & (hlp_a (Incl.mk ?docs:None) + attrs_ - include' - ng + x)
    in

    let modtype_base =
      named "top:modtype" & with_loc
      & mapping (fun attrs name typ loc -> Mtd.mk ~attrs ~loc ~typ name)
        + attrs_ - module' - ng - type' - ng + loc ident - ng - eq - ng
        + modtype_with
    in

    let top_type mk =
      let first =
        mapping (fun attrs eflag rec_flag decl ->
            let decl =
              match eflag with
              | None -> decl
              | Some loc -> tdecl_add_attr "genType" ~loc decl
            in
            ( rec_flag,
              { decl with ptype_attributes = attrs @ decl.ptype_attributes } ))
        + attrs_
        + opt (loc_of @@ export << ng)
        - type' - ng
        + choice
            [
              rec' >> ng >>$ Recursive;
              nonrec' >> ng >>$ Nonrecursive;
              return Nonrecursive;
            ]
        + type_declaration
      in

      let other =
        mapping (fun attrs eflag decl ->
            let decl =
              match eflag with
              | None -> decl
              | Some loc -> tdecl_add_attr "genType" ~loc decl
            in
            { decl with ptype_attributes = attrs @ decl.ptype_attributes })
        + attrs_ - and' - ng
        + opt (loc_of @@ export << ng)
        + type_declaration
      in

      with_del
      & mapping (fun (rec_flag, hd) tail loc ->
            mk ?loc:(Some loc) rec_flag (hd :: tail))
        + first
        + seq (ng >> other)
    in

    let top_external =
      named "external"
        (with_loc
        & mapping (fun attrs name typ prim loc ->
              Val.mk ~loc ~attrs ?docs:None ~prim name typ)
          + attrs_ - external' - ng + loc l_ident - ng - colon - ng
          + core_type_poly - ng - eq
          + seq ~n:1 (ng >> string_raw))
    in

    let module_decl =
      named "module decl"
        (let module_alias = with_loc & (hlp Mty.alias + loc u_longident) in

         mapping (fun n t attrs loc -> Md.mk ~loc ~attrs n t)
         + loc u_ident - ng
         + choice [ colon >> ng >> modtype_with; eq >> ng >> module_alias ])
    in

    let top_module =
      named "module" & with_loc & attrs_ && module' >> ng >> module_decl
    in

    let top_module_rec =
      named "module rec"
        (let first =
           with_loc & attrs_ && module' >> ng >> rec' >> ng >> module_decl
         in
         let other = with_loc & attrs_ && and' >> ng >> module_decl in

         with_loc
         & mapping (fun hd tail loc -> Sig.rec_module ~loc (hd :: tail))
           + first
           + seq (ng >> other))
    in

    let top_import =
      named "import"
        (let item =
           with_loc
           & mapping (fun name alias typ loc attrs ->
                 let alias = Option.value alias ~default:name in
                 Val.mk ~loc ~attrs ~prim:[ name.txt ] alias typ)
             + loc l_ident - ng
             + opt (as' >> ng >> loc l_ident << ng)
             - colon - ng + core_type_poly
         in

         let list = braces & seq ~n:1 item ~sep in

         let scope =
           seq ~n:1
             ~sep:(ng - dot - ng)
             (with_loc
             & mapping (fun x loc -> Exp.constant ~loc (Const.string x))
               + ident)
         in

         choice
           [
             with_loc
             & mapping (fun attrs list name loc ->
                   let attr =
                     ( Location.mkloc "genType.import" name.loc,
                       PStr
                         [ Str.eval @@ Exp.constant @@ Const.string name.txt ]
                     )
                   in
                   let list = List.map ~f:(fun x -> x [ attr ]) list in
                   let str = List.map ~f:Str.primitive list in
                   let mod_ = Mod.structure str in

                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + list - ng - from - ng + loc string_raw;
             with_loc
             & mapping (fun attrs item name loc ->
                   let attr =
                     ( Location.mkloc "genType.import" name.loc,
                       PStr
                         [
                           Str.eval
                           @@ Exp.tuple
                                [
                                  Exp.constant @@ Const.string name.txt;
                                  Exp.constant @@ Const.string "default";
                                ];
                         ] )
                   in
                   let mod_ =
                     Mod.structure [ Str.primitive @@ item [ attr ] ]
                   in
                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + item - ng - from - ng + loc string_raw;
             with_loc
             & mapping (fun attrs list names loc ->
                   let attr =
                     [
                       (Location.mknoloc "val", PStr []);
                       ( Location.mkloc "scope" names.loc,
                         PStr
                           [
                             (Str.eval
                             @@
                             match names.txt with
                             | [ x ] -> x
                             | xs -> Exp.tuple xs);
                           ] );
                     ]
                   in
                   let list = List.map list ~f:(fun x -> x attr) in
                   let str = List.map ~f:Str.primitive list in
                   let mod_ = Mod.structure str in
                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + list - ng - from - ng + loc scope;
             with_loc
             & mapping (fun attrs item names loc ->
                   let attr =
                     [
                       (Location.mknoloc "val", PStr []);
                       ( Location.mkloc "scope" names.loc,
                         PStr
                           [
                             (Str.eval
                             @@
                             match names.txt with
                             | [ x ] -> x
                             | xs -> Exp.tuple xs);
                           ] );
                     ]
                   in
                   let mod_ = Mod.structure [ Str.primitive @@ item attr ] in
                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + item - ng - from - ng + loc scope;
             with_loc
             & mapping (fun attrs list loc ->
                   let attr = (Location.mknoloc "val", PStr []) in
                   let list = List.map ~f:(fun x -> x [ attr ]) list in
                   let str = List.map ~f:Str.primitive list in
                   let mod_ = Mod.structure str in
                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + list;
             with_loc
             & mapping (fun attrs item loc ->
                   let attr = (Location.mknoloc "val", PStr []) in
                   let mod_ =
                     Mod.structure [ Str.primitive @@ item [ attr ] ]
                   in
                   Incl.mk ~loc ~attrs:(Hc.attr "ns.jsFfi" :: attrs) mod_)
               + attrs_ - import - ng + item;
           ])
    in

    let signature =
      named "sig"
        (seq ~sep:ng
           (choice ~name:"sig:elem"
              [
                with_del & (na_hlp Sig.attribute + module_attribute);
                top_type Sig.type_;
                top_module_rec;
                with_del & (hlp_a Sig.extension + attrs_ + module_extension);
                named "sig:exception" & with_del
                & (na_hlp Sig.exception_ + top_exception);
                with_del & (na_hlp Sig.value + top_external);
                named "sig:include" & with_del
                & (na_hlp Sig.include_ + top_include modtype_with);
                named "sig:value" & with_del
                & na_hlp Sig.value
                  + (with_loc
                    & hlp2_a (Val.mk ?docs:None ?prim:None)
                      + attrs_ - let' - ng + loc l_ident - ng - colon - ng
                      + core_type_poly);
                named "sig:modtype" & with_del
                & na_hlp Sig.modtype
                  + choice
                      [
                        modtype_base;
                        with_loc
                        & hlp_a (Mtd.mk ?docs:None ?text:None ?typ:None)
                          + attrs_ - module' - ng - type' - ng + loc u_ident;
                      ];
                with_del & (na_hlp Sig.module_ + top_module);
                with_del & (na_hlp Sig.open_ + top_open);
                with_del & (na_hlp Sig.type_extension + type_extension);
              ]))
    in

    let value_str =
      named "str:value"
        (let lat = type' >> seq ~n:1 (ng >> loc l_ident) << ng << dot in

         let with_lat =
           named "str:value:vb:lat"
             (mapping (fun p1 attrs pat types typ expr vb_attrs vb_loc ->
                  let mapping =
                    {
                      Parsetree_mapping.default with
                      core_type =
                        (fun x ->
                          match x.ptyp_desc with
                          | Ptyp_constr (t, a) ->
                              let desc =
                                match a with
                                | [] ->
                                    let rec loop = function
                                      | [] -> Ptyp_constr (t, a)
                                      | x :: xs -> (
                                          match t.txt with
                                          | Longident.Lident t
                                            when String.equal t x.txt ->
                                              Ptyp_var x.txt
                                          | _ -> loop xs)
                                    in
                                    loop types
                                | _ -> Ptyp_constr (t, a)
                              in
                              { x with ptyp_desc = desc }
                          | _ -> x);
                    }
                  in

                  let typ_pat = Parsetree_mapping.core_type mapping typ in
                  let typ_pat = Typ.poly types typ_pat in

                  let pat =
                    Pat.constraint_
                      ~loc:{ typ.ptyp_loc with loc_start = p1 }
                      ~attrs pat typ_pat
                  in

                  let expr =
                    List.fold_right types ~f:Exp.newtype
                      ~init:(Exp.constraint_ expr typ)
                  in
                  Vb.mk ~loc:vb_loc ~attrs:vb_attrs pat expr)
             + pos + attrs_ + pattern - ng - colon - ng + lat - ng
             + core_type_atom - ng - eq - ng + expression_fun)
         in

         let helper =
           choice
             [
               with_lat;
               mapping (fun pattern expr attrs loc ->
                   Vb.mk ~loc ~attrs pattern expr)
               + pattern_poly_constrainted - ng - eq - ng + expression_fun;
             ]
         in

         let first =
           choice
             [
               with_loc
               & mapping (fun attrs eflag rec_flag decl loc ->
                     let decl =
                       match eflag with
                       | None -> decl attrs
                       | Some loc -> decl (Hc.attr "genType" ~loc :: attrs)
                     in
                     match rec_flag with
                     | None -> (Nonrecursive, decl loc)
                     | _ -> (Recursive, decl loc))
                 + attrs_
                 + opt (loc_of export - ng)
                 - let' - ng
                 + opt (rec' << ng)
                 + helper;
               with_loc
               & mapping (fun attrs loc decl decl_loc ->
                     ( Nonrecursive,
                       decl (Hc.attr "genType" ~loc :: attrs) decl_loc ))
                 + attrs_ + loc_of export - ng + helper;
             ]
         in
         let other =
           mapping (fun attrs p1 eflag x p2 ->
               let decl =
                 match eflag with
                 | None -> x attrs
                 | Some loc -> x (Hc.attr "genType" ~loc :: attrs)
               in
               decl @@ loc_mk p1 p2)
           + attrs_ - and' - ng + pos
           + opt (loc_of export - ng)
           + helper + pos
         in

         with_del
         & mapping (fun (rec_flag, first) others loc ->
               Str.value ~loc rec_flag (first :: others))
           + first
           + seq (ng >> other))
    in

    let module_binding_helper =
      mapping (fun a t b attrs loc ->
          let b =
            Option.value ~default:b
              (Option.map ~f:(fun t -> Mod.constraint_ b t) t)
          in
          Mb.mk ~loc ~attrs a b)
      + loc u_ident - ng
      + opt (colon >> ng >> modtype << ng)
      - eq - ng + modexpr
    in

    let module_binding =
      named "str:mb"
        (with_loc & attrs_ && module' >> ng >> module_binding_helper)
    in

    let rec_module_binding =
      let first =
        with_loc & attrs_
        && module' >> ng >> rec' >> ng >> module_binding_helper
      in
      let other = with_loc & attrs_ && and' >> ng >> module_binding_helper in

      named "str:mb:rec" (cons + first + seq (ng >> other))
    in

    let structure =
      named "str"
      & seq ~sep:ng
          (choice ~name:"str:elem"
             [
               with_del & (na_hlp Str.attribute + module_attribute);
               with_del & (na_hlp Str.rec_module + rec_module_binding);
               value_str;
               with_del & (hlp_a Str.extension + attrs_ + module_extension);
               named "str:exception" & with_del
               & (na_hlp Str.exception_ + top_exception);
               with_del & (na_hlp Str.primitive + top_external);
               with_del & (na_hlp Str.include_ + top_import);
               named "str:include" & with_del
               & (na_hlp Str.include_ + top_include modexpr);
               with_del & (na_hlp Str.module_ + module_binding);
               named "str:modtype" & with_del
               & (na_hlp Str.modtype + modtype_base);
               with_del & (na_hlp Str.open_ + top_open);
               with_del & (na_hlp Str.type_extension + type_extension);
               top_type Str.type_;
               with_del & (hlp_a Str.eval + attrs_ + expression_fun);
             ])
    in

    { signature; structure; payload; modexpr = (module Modexpr) }

  let parse p ~src ~filename =
    let fold_f (c, d) = function
      | LogElement.Comment x -> (x :: c, d)
      | Diagnostics x -> (c, x :: d)
    in

    let p =
      mapping (fun x (comments, diagnostics) ->
          Res_driver.
            {
              filename;
              source = src;
              parsetree = x;
              diagnostics;
              invalid = false;
              comments;
            })
      + p
      + fold_log ([], []) fold_f
    in
    parse_string p ~filename src

  let signature_parser = ng >> parsers.signature << ng

  let structure_parser = ng >> parsers.structure << ng

  let parse_interface = parse signature_parser

  let parse_implementation = parse structure_parser
end
