open Base
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic

module Make (Basic : BASIC) (Core : CORE with module Comb = Basic.Comb) (Type : TYPE with module Comb = Basic.Comb) =
struct
  open Basic
  open Core
  open Type
  open Pc
  open Comb
  module Comb = Comb

  module type THIS = PATTERN with module Comb = Basic.Comb

  let x =
    fix_poly @@ fun getter : (module THIS) ->
    (module struct
      module Comb = Comb

      let pattern = getter.get @@ fun (module M : THIS) -> M.pattern

      let pattern_constrainted =
        named "pattern:constraint"
        & choice [ with_loc & (hlp2 Pat.constraint_ + pattern - ng - colon - ng + core_type); pattern ]

      let pattern_poly_constrainted =
        named "pattern:constraint:poly"
        & choice [ with_loc & (hlp2 Pat.constraint_ + pattern - ng - colon - ng + core_type_poly); pattern ]

      let tuple =
        named "pattern:tuple" (with_loc & parens & (hlp Pat.tuple + seq ~n:2 pattern_constrainted ~sep ~trail:true))

      let array =
        named "pattern:array" (with_loc & brackets & (hlp Pat.array + seq pattern_constrainted ~sep ~trail:true))

      let record =
        let lid2str lid =
          let str =
            match[@warning "-8"] lid.Location.txt with Longident.Lident str -> str | Longident.Ldot (_, str) -> str
          in
          let loc_end = lid.Location.loc.loc_end in
          Location.mkloc str @@ loc_mk { loc_end with pos_cnum = Int.(loc_end.pos_cnum - String.length str) } loc_end
        in

        with_loc
        & hlp2 Pat.record - l_brace - ng
          + seq ~n:1 ~sep
              (mapping (fun lid pat ->
                   match pat with
                   | Some pat -> (lid, pat)
                   | None ->
                       ( lid,
                         let str = lid2str lid in
                         Pat.var ~loc:str.loc str ))
              + loc l_longident
              + opt (ng >> colon >> ng >> pattern_constrainted))
          + choice [ opt sep >> ng >> r_brace >>$ Closed; sep >> _' >> opt sep >> ng >> r_brace >>$ Open ]

      let list_helper = make_list_helper ~constr:Pat.construct ~tuple:Pat.tuple ~get_loc:(fun x -> x.ppat_loc)

      let list =
        named "pattern:list"
        & choice
            [
              with_loc & (mapping (list_helper [] None) - list - ng - r_brace);
              list >> ng >> ellipsis >> ng >> pattern_constrainted << opt sep << ng << r_brace;
              with_loc
              & mapping list_helper - list - ng + seq ~n:1 pattern_constrainted ~sep
                + choice [ sep >> ng >> ellipsis >> ng >> pattern_constrainted >>| Base.Option.some; return None ]
                - opt sep - ng - r_brace;
            ]

      let constructor_arguments =
        choice
          [
            tuple;
            l_paren >> ng >> pattern_constrainted << opt sep << ng << r_paren;
            with_loc
            & mapping (fun loc -> Pat.construct ~loc (Location.mknoloc @@ Longident.Lident "()") None)
              - l_paren - ng - r_paren;
          ]

      let construct = with_loc & (hlp2 Pat.construct + loc u_longident + opt constructor_arguments)

      let polyvariant = with_loc & (hlp2 Pat.variant + variant_tag + opt constructor_arguments)

      let typ = with_loc & (hlp Pat.type_ - hash - ng - ellipsis - ng + loc l_longident)

      let unpack =
        named "pattern:unpack"
        & choice
            [
              with_loc
              & mapping (fun m t loc ->
                    let m = Pat.unpack m in
                    Pat.constraint_ ~loc m t)
                - module' - ng - l_paren - ng + loc u_ident - ng - colon - ng + core_type_package - ng - r_paren;
              with_loc & (hlp Pat.unpack - module' - ng - l_paren - ng + loc u_ident - ng - r_paren);
            ]

      let signed_number =
        choice
          [
            mapping (function [@warning "-8"]
              | Pconst_integer (str, suf) -> Pconst_integer ("-" ^ str, suf)
              | Pconst_float (str, suf) -> Pconst_float ("-" ^ str, suf))
            - minus + number;
            plus >> number;
          ]

      let number_range =
        let number = signed_number <|> number in

        with_loc & (hlp2 Pat.interval + number - ng - dot_dot - ng + number)

      let js_string =
        named "pattern:js_string"
          (with_loc
          & (mapping (fun c loc -> Pat.constant ~loc ~attrs:[ Hc.attr "res.template" ] c) + template_no_template))

      let true_ =
        with_loc & (mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid [ "true" ]) None) + loc_of true')

      let false_ =
        with_loc & (mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid [ "false" ]) None) + loc_of false')

      let pattern_atom =
        choice ~name:"pattern:atom"
          [
            with_loc & (mapping (fun loc -> Pat.any ~loc ()) - _');
            with_loc & (hlp2 Pat.interval + character - ng - dot_dot - ng + character);
            number_range;
            with_loc
            & (mapping (fun loc -> Pat.construct ~loc (mkloc ~loc @@ Hc.lid [ "()" ]) None) - l_paren - ng - r_paren);
            unpack;
            tuple;
            with_loc & parens & (mapping pat_loc + pat_attrs pattern_constrainted);
            array;
            record;
            with_loc & (hlp Pat.constant + (signed_number <|> constant));
            js_string;
            list;
            construct;
            polyvariant;
            typ;
            true_;
            false_;
            with_loc & (hlp Pat.var + loc ident);
          ]

      let pattern_exception =
        choice
          [
            with_loc & (hlp Pat.exception_ - exception' - ng + pattern_atom);
            with_loc & (hlp Pat.extension + extension);
            pattern_atom;
          ]

      let primary = choice [ with_loc & (hlp Pat.lazy_ - lazy' - ng + pattern_exception); pattern_exception ]

      let alias =
        fold_left_cont_0_n primary
          (mapping (fun name prev -> Pat.alias ~loc:(loc_comb prev.ppat_loc name.loc) prev name)
          - ng - as' - ng + loc l_ident)

      let or_ =
        fold_left_cont_0_n alias
          (mapping (fun alias prev -> Pat.or_ ~loc:(loc_comb prev.ppat_loc alias.ppat_loc) prev alias)
          - ng - pipe - ng + alias)

      let pattern = named "pattern" or_
    end)

  let pattern =
    let (module M) = x in
    M.pattern

  let pattern_atom =
    let (module M) = x in
    M.pattern_atom

  let pattern_constrainted =
    let (module M) = x in
    M.pattern_constrainted

  let pattern_poly_constrainted =
    let (module M) = x in
    M.pattern_poly_constrainted
end
