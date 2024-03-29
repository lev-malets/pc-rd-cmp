open Base
open Basic
open Compilerlibs406
open Parsetree
open Asttypes
open Ast_helper

module Make
    (Basic : Sigs.BASIC)
    (Core : Basic.CORE)
    (Type : Basic.TYPE)
    (Pattern : Basic.PATTERN)
    (Modexpr : Basic.MODEXPR) =
struct
  open Basic
  open Core
  open Type
  open Pattern
  open Modexpr
  open Pc
  open Comb
  module Comb = Comb

  type arrow_parser_helpers =
    {args_loop : expression Comb.t; types_loop : expression Comb.t}

  let x =
    fix_gen
    @@ fun getter : (module EXPRESSION) ->
    (module struct
      module Comb = Comb

      let expression = getter.get @@ fun (module M : EXPRESSION) -> M.expression
      let expression_fun = getter.get @@ fun (module M) -> M.expression_fun

      let expression_sequence =
        getter.get @@ fun (module M) -> M.expression_sequence

      let expression_p0 = getter.get @@ fun (module M) -> M.expression_p0

      let expression_constrainted =
        named "expression:constrainted"
        & choice
            [ with_loc
              & hlp2 Exp.constraint_ <*> expression_fun <<. colon << ng
                <*> core_type
            ; expression_fun ]

      let value_binding_list =
        let value_binding =
          with_loc
          & hlp2 @@ Vb.mk ?docs:None ?text:None
            <*> pattern <<. eq <*>. expression_fun
        in
        seq ~n:1 value_binding ~sep:(ng >> and' >> ng)

      let let' =
        let rec_flag = choice [rec' >>$ Recursive; return Nonrecursive] in
        named "expression:let"
          (with_loc
          & hlp3 Exp.let_ << let' <*>. rec_flag << ng <*> value_binding_list
            << del
            <*> choice (* TODO: remove choice *)
                  [ ng >> expression_sequence
                  ; return @@ Hc.unit_expr Location.none ])

      let letmodule =
        named "expression:module"
          (with_loc
          & hlp3 Exp.letmodule << module' <*>. loc u_ident <<. eq <*>. modexpr
            << del <*>. expression_sequence)

      let pack =
        choice ~name:"expression:pack"
          [ with_loc
            & mapping (fun m t loc -> Exp.constraint_ ~loc (Exp.pack m) t)
              << module' <<. l_paren <*>. modexpr <<. colon << ng
              <*> core_type_package <<. r_paren
          ; with_loc
            & hlp Exp.pack << module' <<. l_paren <*>. modexpr << ng << r_paren
          ]

      let letopen =
        named "expression:open"
          (let f ?loc ?attrs override name cont =
             let override =
               override
               |> Option.map ~f:(fun _ -> Override)
               |> Option.value ~default:Fresh
             in
             Exp.open_ ?loc ?attrs override name cont
           in
           with_loc
           & hlp3 f << open' <*> opt bang <*>. loc u_longident << del << ng
             <*> expression_sequence)

      let letexception =
        named "expression:exception"
          (with_loc
          & hlp2 Exp.letexception << exception' << ng
            <*> type_extension_constructor << del <*>. expression_sequence)

      let expression_in_braces =
        named "expression:in_braces"
        & choice
            [ choice ~name:"expression:in_braces:let"
                [let'; letmodule; letopen; letexception]
            ; expression_fun ]

      let expression_sequence =
        fix
        & fun expression_sequence ->
        named "expression:sequence"
        & choice
            [ with_loc
              & hlp2 Exp.sequence <*> expression_in_braces << del << ng
                <*> expression_sequence
            ; exp_attrs expression_in_braces ]

      let scoped_sequence =
        l_brace >> ng >> expression_sequence << del <<. r_brace

      let fun' =
        let arrow_tail =
          let tail = arrow >> ng >> expression_fun in
          choice ~name:"expression:arrow:tail"
            [ mapping (fun typ expr ->
                  Exp.constraint_ ~loc:expr.pexp_loc expr typ)
              << colon <*>. core_type_atom <*>. tail
            ; tail ]
        in
        let label = tilda >> ng >> loc l_ident in
        let with_label =
          choice ~name:"expression:arrow:with_label"
            [ mapping (fun {txt; loc} pat ->
                  (txt, pat_add_attr "ns.namedArgLoc" ~loc pat))
              <*> label <<. as' <*>. pattern_constrainted
            ; mapping (fun ({txt; loc} as var) typ ->
                  ( txt
                  , Pat.constraint_
                      ~loc:(loc_comb loc typ.ptyp_loc)
                      ~attrs:[Hc.attr "ns.namedArgLoc" ~loc]
                      (Pat.var ~loc var) typ ))
              <*> label <<. colon <*>. core_type_atom
            ; mapping (fun ({txt; loc} as var) ->
                  (txt, Pat.var ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] var))
              <*> label ]
        in
        let with_value =
          choice ~name:"expression:arrow:with_value"
            [ mapping (fun (s, pat) -> (Optional s, pat, None))
              <*> with_label <<. eq <<. question
            ; mapping (fun (s, pat) expr -> (Optional s, pat, Some expr))
              <*> with_label <<. eq <*>. expression_constrainted
            ; mapping (fun (s, pat) -> (Labelled s, pat, None)) <*> with_label
            ]
        in
        let nolabel =
          choice ~name:"expression:arrow:nolabel"
            [ with_loc
              & hlp2 Pat.constraint_ <*> pat_attrs pattern <<. colon << ng
                <*> core_type
            ; pat_attrs pattern ]
          >>| fun p -> (Nolabel, p, None)
        in
        let constr_unit =
          named "expression:arrow:unit"
            (let unit = l_paren >> r_paren >>$ Hc.lid ["()"] in
             let pat =
               loc unit >>| fun lid -> Pat.construct ~loc:lid.loc lid None
             in
             exp_attrs & Sugar.async
             & mapping (fun p1 pat exp p2 ->
                   Exp.fun_ ~loc:(loc_mk p1 p2) Nolabel None pat exp)
               <*> pos <*> pat <*>. arrow_tail <*> pos)
        in
        let with_many_args =
          let loop =
            fix_gen
            @@ fun getter ->
            let args_loop = getter.get @@ fun x -> x.args_loop in
            let types_loop = getter.get @@ fun x -> x.types_loop in
            let tail =
              choice ~name:"expression:arrow:many_args:tail"
                [ r_paren >> ng >> arrow_tail
                ; comma >> ng >> r_paren >> ng >> arrow_tail
                ; comma >> ng >> exp_attrs (type' >> ng >> types_loop)
                ; comma >> ng >> args_loop ]
            in
            let curried =
              choice ~name:"expression:arrow:many_args:curried"
                [ exp_attrs
                  & mapping (fun p1 (label, pat, expr) tail p2 ->
                        Exp.fun_ ~loc:(loc_mk p1 p2) label expr pat tail)
                    <*> pos <*> with_value <*>. tail <*> pos
                ; mapping (fun p1 (label, pat, expr) tail p2 ->
                      Exp.fun_ ~loc:(loc_mk p1 p2) label expr pat tail)
                  <*> pos <*> nolabel <*>. tail <*> pos ]
            in
            { args_loop =
                choice ~name:"expression:arrow:many_args:args_loop"
                  [dot >> ng >> curried >>| exp_add_attr "bs"; curried]
            ; types_loop =
                choice ~name:"expression:arrow:many_args:types_loop"
                  [ with_loc & hlp2 Exp.newtype <*> loc l_ident <*>. types_loop
                  ; comma >> ng >> exp_attrs (type' >> ng >> types_loop)
                  ; comma >> ng >> args_loop ] }
          in
          choice ~name:"expression:arrow:many_args"
            [ exp_attrs & Sugar.async
              & l_paren >> ng >> exp_attrs (type' >> ng >> loop.types_loop)
            ; exp_attrs & Sugar.async & l_paren >> ng >> loop.args_loop ]
        in
        let constr_unit_uncurried =
          let unit =
            return (Longident.Lident "()") << l_paren <<. dot <<. r_paren
          in
          let pat =
            loc unit >>| fun lid -> Pat.construct ~loc:lid.loc lid None
          in
          named "expression:arrow:unit_uncur"
            (Sugar.async
            & mapping (fun p1 pat expr p2 ->
                  Exp.fun_ ~loc:(loc_mk p1 p2)
                    ~attrs:[Hc.attr "bs"]
                    Nolabel None pat expr)
              <*> pos <*> pat <*>. arrow_tail <*> pos)
        in
        let only_arg =
          named "expression:arrow:one_arg"
            (exp_attrs & Sugar.async
            & mapping (fun p1 p e p2 ->
                  Exp.fun_ ~loc:(loc_mk p1 p2) Nolabel None p e)
              <*> pos <*> pattern_atom <<. arrow <*>. expression_fun <*> pos)
        in
        choice ~name:"expression:arrow"
          [constr_unit; with_many_args; constr_unit_uncurried; only_arg]

      let tuple =
        named "expression:tuple"
        & mapping (fun p1 l p2 -> Exp.tuple ~loc:(loc_mk p1 p2) l)
          <*> pos << l_paren << ng
          <*> seq ~n:2 expression_constrainted ~sep ~trail:true
          <<. r_paren <*> pos

      let list_helper =
        make_list_helper ~constr:Exp.construct ~tuple:Exp.tuple
          ~get_loc:(fun x -> x.pexp_loc)

      let list =
        choice ~name:"expression:list"
          [ with_loc & mapping (list_helper [] None) << list <<. r_brace
          ; list >> ng >> ellipsis >> ng >> expression_constrainted << opt sep
            <<. r_brace
          ; with_loc
            & mapping list_helper << list << ng
              <*> seq ~n:1 ~sep expression_constrainted
              <*> choice
                    [ ng >> comma >> ng >> ellipsis >> ng
                      >> expression_constrainted << opt sep >>| Option.some
                    ; opt sep >>$ None ]
              <<. r_brace ]

      let case =
        mapping (fun pat guard exp -> Exp.case pat ?guard exp)
        <*> pattern << ng
        <*> opt (choice ~name:"case:guard" [when'; if'] >> ng >> expression)
        <<. arrow <*>. expression_sequence

      let case_list =
        l_brace
        >> opt (ng >> pipe)
        >> seq ~n:1 (ng >> case) ~sep:(ng >> pipe)
        <<. r_brace

      let switch =
        named "expression:switch"
          (with_loc & hlp2 Exp.match_ << switch <*>. expression <*>. case_list)

      let try_catch =
        named "expression:try"
          (with_loc
          & hlp2 Exp.try_ << try' <*>. expression <<. catch << ng <*> case_list
          )

      type if_parsers = {else_part : expression t; ifthenelse : expression t}

      let {ifthenelse; _} =
        fix_gen
        @@ fun x ->
        let else_part = x.get (fun x -> x.else_part) in
        let ifthenelse =
          named "expression:if"
            (with_loc
            & hlp3 Exp.ifthenelse << if' <*>. expression << ng
              <*> scoped_sequence <*> opt else_part)
        in
        let else_part =
          ng >> else' >> ng
          >> choice ~name:"expression:if:else" [scoped_sequence; ifthenelse]
        in
        {else_part; ifthenelse}

      let for_ =
        let direction =
          choice ~name:"expression:for:dir" [to' >>$ Upto; downto' >>$ Downto]
        in
        let mapping =
          mapping (fun p1 pat ef dir et ea p2 ->
              Exp.for_ ~loc:(loc_mk p1 p2) pat ef et dir ea)
        in
        choice ~name:"expression:for"
          [ mapping <*> pos << for' <*>. pattern <<. in' << ng <*> expression
            <*>. direction <*>. expression << ng <*> scoped_sequence <*> pos
          ; mapping <*> pos << for' <<. l_paren <*>. pattern << ng << in'
            <*>. expression <*>. direction <*>. expression <<. r_paren
            <*>. scoped_sequence <*> pos ]

      let while_ =
        named "expression:while"
          (with_loc
          & hlp2 Exp.while_ << while' <*>. expression << ng <*> scoped_sequence
          )

      let assert_ =
        named "expression:assert"
          (with_loc & hlp Exp.assert_ << assert' <*>. expression)

      let lazy_ =
        named "expression:lazy"
          (with_loc & hlp Exp.lazy_ << lazy' <*>. expression)

      let _unit_arg_ loc =
        ( Nolabel
        , Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
        )

      let _unit_arg = _unit_arg_ Location.none
      let ident = with_loc & hlp Exp.ident <*> loc l_longident

      let jsx =
        let arg =
          choice ~name:"expression:jsx:arg"
            [ mapping (fun {txt; loc} expr ->
                  (Labelled txt, exp_add_attr "ns.namedArgLoc" ~loc expr))
              <*> loc l_ident <<. eq <*>. expression_p0
            ; mapping (fun {txt; loc} expr ->
                  (Optional txt, exp_add_attr "ns.namedArgLoc" ~loc expr))
              <*> loc l_ident <<. eq <<. question << ng <*> expression_p0
            ; mapping (fun {txt; loc} ->
                  ( Labelled txt
                  , Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                  ))
              <*> loc l_ident
            ; mapping (fun {txt; loc} ->
                  ( Optional txt
                  , Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                  ))
              << question <*>. loc l_ident ]
        in
        let children_list =
          with_loc
          & mapping (fun list -> list_helper list None)
            <*> seq expression ~sep:ng
        in
        let children = choice [ellipsis >> expression; children_list] in
        choice ~name:"expression:jsx"
          [ named "jsx:leaf"
              (mapping (fun p1 tag args p2 ->
                   Exp.apply ~loc:(loc_mk p1 p2) (Exp.ident tag)
                     (args
                     @ [ ( Labelled "children"
                         , Exp.construct
                             (Location.mknoloc (Longident.Lident "[]"))
                             None )
                       ; _unit_arg ]))
              <*> pos << lt << ng
              <*> loc (l_ident >>| fun x -> Longident.Lident x)
              <*> seq (ng >> arg)
              <<. slash <<. gt <*> pos)
          ; named "jsx:tag"
              (run
              & mapping (fun p1 tag args children tag2 p2 ->
                    let open Simple in
                    if String.(tag.txt <> tag2) then fail
                    else
                      let tag = {tag with txt = Longident.Lident tag.txt} in
                      let tailargs =
                        [(Labelled "children", children); _unit_arg]
                      in
                      let e =
                        Exp.apply ~loc:(loc_mk p1 p2) (Exp.ident tag)
                          (args @ tailargs)
                      in
                      return e)
                <*> pos << lt <*>. loc l_ident
                <*> seq (ng >> arg)
                <<. gt <*>. children <<. lt <<. slash << ng <*> l_ident <<. gt
                <*> pos_end)
          ; named "jsx:ce:leaf"
              (mapping (fun p1 tag args p2 ->
                   let tag =
                     Exp.ident ~loc:tag.loc
                     @@ Location.mkloc
                          (Longident.Ldot (tag.txt, "createElement"))
                          tag.loc
                   in
                   Exp.apply ~loc:(loc_mk p1 p2) tag
                     (args
                     @ [ ( Labelled "children"
                         , Exp.construct
                             (Location.mknoloc (Longident.Lident "[]"))
                             None )
                       ; _unit_arg ]))
              <*> pos << lt <*>. loc longident
              <*> seq (ng >> arg)
              <<. slash <<. gt <*> pos_end)
          ; named "jsx:ce:tag"
              (run
              & mapping (fun p1 tag args children tag2 p2 ->
                    let open Simple in
                    if Poly.(tag.txt <> tag2) then fail
                    else
                      let tailargs =
                        [(Labelled "children", children); _unit_arg]
                      in
                      let tag =
                        Exp.ident ~loc:tag.loc
                          { tag with
                            txt = Longident.Ldot (tag.txt, "createElement") }
                      in
                      let e =
                        Exp.apply ~loc:(loc_mk p1 p2) tag (args @ tailargs)
                      in
                      return e)
                <*> pos << lt <*>. loc longident
                <*> seq (ng >> arg)
                <<. gt <*>. children <<. lt <<. slash << ng <*> longident <<. gt
                <*> pos_end)
          ; named "jsx:notag:item"
              (with_loc
              & mapping (fun expr -> list_helper [expr] None)
                << lt <<. gt <<. ellipsis <*>. expression << ng << lt <<. slash
                <<. gt)
          ; named "jsx:notag"
              (lt >> ng >> gt >> ng >> children_list <<. lt <<. slash <<. gt) ]

      let jsx = jsx >>| exp_add_attr "JSX"

      let array =
        choice ~name:"expression:array"
          [ mapping (fun p1 p2 -> Exp.array ~loc:(loc_mk p1 p2) [])
            <*> pos << l_bracket <<. r_bracket <*> pos
          ; mapping (fun p1 l p2 -> Exp.array ~loc:(loc_mk p1 p2) l)
            <*> pos << l_bracket << ng
            <*> seq ~n:1 ~sep ~trail:true expression_constrainted
            <<. r_bracket <*> pos ]

      let record =
        named "expression:record"
          (let nb =
             choice ~name:"expression:record:nb"
               [ t2 <*> loc l_longident <<. colon << ng
                 <*> Sugar.optional expression_fun
               ; Sugar.optional1
                   (loc l_longident
                   >>| fun lid ->
                   (lid, Exp.ident ~loc:lid.loc (lid_drop_path lid))) ]
           in
           with_loc
           & mapping (fun expr list loc -> Exp.record ~loc list expr)
             << l_brace << ng
             <*> opt (ng >> ellipsis >> ng >> expression_constrainted << sep)
             <*> seq ~n:1 nb ~sep ~trail:true
             <<. r_brace)

      let bs_object =
        let record =
          let row =
            mapping (fun {txt; loc} pat ->
                let lid = Location.mkloc (Longident.Lident txt) loc in
                match pat with
                | Some pat -> (lid, pat)
                | None -> (lid, Exp.ident ~loc lid))
            <*> loc string_raw
            <*> opt (ng >> colon >> ng >> expression)
          in
          mapping (fun p1 list p2 -> Exp.record ~loc:(loc_mk p1 p2) list None)
          <*> pos << l_brace << ng
          <*> seq ~n:1 row ~sep ~trail:true
          <<. r_brace <*> pos
        in
        named "bs_object"
          (record
          >>| fun x ->
          Exp.extension ~loc:x.pexp_loc
            (Location.mkloc "obj" x.pexp_loc, PStr [Str.eval x]))

      let extension = with_loc & hlp Exp.extension <*> extension

      let coerce =
        named "expression:coerce"
          (mapping (fun p1 a b p2 -> Exp.coerce ~loc:(loc_mk p1 p2) a None b)
          <*> pos << l_paren <*>. expression_constrainted <<. colon_gt
          <*>. core_type_atom <<. r_paren <*> pos)

      let string =
        named "expression:string"
          (with_loc & hlp Exp.constant <*> string_multiline)

      let js_string =
        named "expression:js_string" @@ template ~quote_tag:"js" ~expression

      let json_string =
        named "expression:json_string"
        & json_tag >> ng >> template ~quote_tag:"json" ~expression

      let constr_args =
        choice ~name:"expression:constr_args"
          [ loc_of (l_paren >> ng >> r_paren) >>| Hc.unit_expr
          ; tuple
          ; l_paren >> ng >> expression_constrainted << opt sep <<. r_paren ]

      let polyvariant =
        named "polyvariant"
          (with_loc & hlp2 Exp.variant <*> variant_tag <*> opt constr_args)

      let true_ =
        with_loc
        & mapping (fun a loc -> Exp.construct ~loc a None)
          <*> loc (true' >>$ Longident.Lident "true")

      let false_ =
        with_loc
        & mapping (fun a loc -> Exp.construct ~loc a None)
          <*> loc (false' >>$ Longident.Lident "false")

      let string_ident =
        with_loc
        & hlp Exp.ident <*> loc (string_ident >>| fun x -> Longident.Lident x)

      let atom =
        choice ~name:"expression:atom"
          [ switch
          ; assert_
          ; lazy_
          ; try_catch
          ; for_
          ; while_
          ; jsx
          ; list
          ; string_ident
          ; string
          ; js_string
          ; json_string
          ; pack
          ; true_
          ; false_
          ; ident
          ; with_loc
            & hlp2 Exp.construct <*> loc u_longident
              <*> opt (ng_no_new_line >> constr_args)
          ; polyvariant
          ; with_loc
            & mapping (fun a loc -> Exp.construct ~loc a None)
              <*> loc (l_paren >> ng >> r_paren >>$ Longident.Lident "()")
          ; tuple
          ; coerce
          ; with_loc & parens
            & mapping exp_loc <*> exp_attrs expression_constrainted
          ; array
          ; scoped_sequence >>| exp_add_attr "ns.braces"
          ; record
          ; bs_object
          ; extension
          ; with_loc & hlp Exp.constant <*> constant ]

      let res_unit loc =
        let pattern = Pat.var @@ Location.mknoloc "__res_unit" in
        let expr =
          Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        let expr2 =
          Exp.ident @@ Location.mknoloc @@ Longident.Lident "__res_unit"
        in
        let vb = Vb.mk ?docs:None ?text:None pattern expr in
        Exp.let_ Nonrecursive [vb] expr2

      let field_set_cont =
        named "expression:field:set"
          (mapping (fun lid expr prev ->
               let loc = loc_comb prev.pexp_loc expr.pexp_loc in
               Exp.setfield ~loc prev lid expr)
          <<. dot <*>. loc l_longident <<. eq << ng <*> expression_fun)

      let object_set_cont =
        named "expression:object:set"
        & mapping (fun idx p1 loc value prev ->
              let s =
                Exp.send ~loc:(loc_mk prev.pexp_loc.loc_start p1) prev idx
              in
              let lid =
                Exp.ident ~loc @@ Location.mkloc (Longident.Lident "#=") loc
              in
              Exp.apply
                ~loc:(loc_comb prev.pexp_loc value.pexp_loc)
                lid
                [(Nolabel, s); (Nolabel, value)])
          <<. l_bracket <*>. loc string_raw <<. r_bracket <*> pos <*>. loc_of eq
          <*>. expression_fun

      let array_set_cont =
        named "expression:array:set"
          (mapping (fun idx value prev ->
               let loc = loc_comb prev.pexp_loc value.pexp_loc in
               Exp.apply ~loc
                 (Hc.expr_id ["Array"; "set"])
                 [(Nolabel, prev); (Nolabel, idx); (Nolabel, value)])
          <<. l_bracket <*>. expression <<. r_bracket <<. eq <*>. expression_fun
          )

      let tail =
        let labelled special_arg =
          choice
            [ mapping (fun {txt; loc} expr ->
                  (Optional txt, exp_add_attr "ns.namedArgLoc" ~loc expr))
              << tilda <*>. loc l_ident <<. eq <<. question << ng
              <*> expression_constrainted
            ; mapping (fun tag expr -> (Optional tag, expr))
              << tilda <*>. l_ident <<. eq <<. question << ng <*> special_arg
            ; mapping (fun {txt; loc} expr ->
                  (Labelled txt, exp_add_attr "ns.namedArgLoc" ~loc expr))
              << tilda <*>. loc l_ident <<. eq << ng <*> expression_constrainted
            ; mapping (fun tag expr -> (Labelled tag, expr))
              << tilda <*>. l_ident <<. eq <*>. special_arg
            ; mapping (fun {txt; loc} typ p2 ->
                  let exp =
                    Exp.constraint_ ~loc:(loc_mk loc.loc_start p2)
                      ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                      (Hc.expr_id ~loc
                         ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                         [txt])
                      typ
                  in
                  (Labelled txt, exp))
              << tilda <*>. loc l_ident <<. colon << ng <*> core_type_atom
              <*> pos
            ; mapping (fun {txt; loc} p2 ->
                  let exp =
                    Exp.ident ~loc:(loc_mk loc.loc_start p2)
                      ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                      (Location.mkloc (Hc.lid [txt]) loc)
                  in
                  (Optional txt, exp))
              << tilda <*>. loc l_ident <<. question <*> pos
            ; mapping (fun {txt; loc} ->
                  ( Labelled txt
                  , Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]
                  ))
              << tilda <*>. loc l_ident
            ; (choice [expression_constrainted; special_arg]
              >>| fun e -> (Nolabel, e)) ]
        in
        let apply =
          named "expression:apply"
            (let flags = ref [] in
             let special_arg =
               mapping (fun loc ->
                   Exp.ident ~loc @@ Location.mkloc (Longident.Lident "__x") loc)
               <*> loc_of _'
               << exec (fun _ -> List.hd_exn !flags := true)
             in
             let params =
               choice ~name:"expression:apply:params"
                 [ l_paren >> ng
                   >> seq ~n:1 ~sep ~trail:true (labelled special_arg)
                   <<. r_paren
                 ; (l_paren >> ng >> loc_of r_paren
                   >>| fun loc -> [_unit_arg_ loc]) ]
             in
             let drop_flag =
               exec
               @@ fun _ ->
               let f = !(List.hd_exn !flags) in
               flags := List.tl_exn !flags;
               f
             in
             exec (fun _ -> flags := ref false :: !flags)
             >> choice
                  [ mapping (fun list flag p2 prev ->
                        let loc = {prev.pexp_loc with loc_end = p2} in
                        if flag then
                          let x = Exp.apply ~loc prev list in
                          Exp.fun_ ~loc Nolabel None
                            (Pat.var @@ Location.mknoloc "__x")
                            x
                        else Exp.apply ~loc prev list)
                    << ng_no_new_line <*> params <*> drop_flag <*> pos
                  ; drop_flag >> fail ])
        in
        let apply_uncurried =
          let apply_params = dot >> ng >> seq ~n:1 ~sep (labelled fail) in
          let args =
            named "expression:apply:uncurried:tail"
              (fold_left_cont_0_n
                 (mapping (fun args p2 expr ->
                      Exp.apply
                        ~loc:{expr.pexp_loc with loc_end = p2}
                        ~attrs:[Hc.attr "bs"]
                        expr args)
                 <*> apply_params <*> pos)
                 (mapping (fun args p3 cont expr ->
                      let expr = cont expr in
                      Exp.apply
                        ~loc:{expr.pexp_loc with loc_end = p3}
                        ~attrs:[Hc.attr "bs"]
                        expr args)
                 <<. comma <*>. apply_params <*> pos))
          in
          let unit_arg =
            choice ~name:"expression:apply:uncurried:unit_arg"
              [ mapping (fun loc -> [(Nolabel, res_unit loc)])
                << l_paren <<. dot << ng
                <*> loc_of (l_paren >> ng >> r_paren)
                <<. r_paren
              ; return [_unit_arg] << l_paren <<. dot <<. r_paren ]
          in
          choice ~name:"expression:apply:uncurried"
            [ mapping (fun arg loc_end prev ->
                  Exp.apply
                    ~loc:{prev.pexp_loc with loc_end}
                    ~attrs:[Hc.attr "bs"]
                    prev arg)
              <*>. unit_arg <*> pos
            ; ng >> l_paren >> ng >> args << opt sep <<. r_paren ]
        in
        let field =
          failed field_set_cont
          >> named "expression:field"
               (mapping (fun lid loc_end prev ->
                    Exp.field ~loc:{prev.pexp_loc with loc_end} prev lid)
               <<. dot <*>. loc l_longident <*> pos)
        in
        let object_get =
          failed object_set_cont
          >> named "expression:send"
               (mapping (fun str loc_end prev ->
                    Exp.send ~loc:{prev.pexp_loc with loc_end} prev str)
               <<. l_bracket <*>. loc string_raw <<. r_bracket <*> pos)
        in
        let array_get =
          failed array_set_cont
          >> named "expression:index"
               (mapping (fun idx loc_end prev ->
                    Exp.apply
                      ~loc:{prev.pexp_loc with loc_end}
                      (Hc.expr_id ["Array"; "get"])
                      [(Nolabel, prev); (Nolabel, idx)])
               <<. l_bracket <*>. expression_constrainted << ng << r_bracket
               <*> pos)
        in
        choice [apply; apply_uncurried; field; object_get; array_get]

      let primary = named "expression:primary" (fold_left_cont_0_n atom tail)

      let set =
        named "expression:set"
          (let set_cont =
             choice ~name:"expression:set:cont"
               [field_set_cont; object_set_cont; array_set_cont]
           in
           fold_left_cont_0_1 primary set_cont)

      let unops = bang >>$ "not"

      let unary =
        fix
        @@ fun unary ->
        choice ~name:"expression:unary"
          [ with_loc
            & mapping (fun op expr ->
                  let op = str2lid op in
                  match expr.pexp_desc with
                  | Pexp_constant (Pconst_integer (str, suf) as c) ->
                      if Char.equal str.[0] '-' then fun loc ->
                        Exp.constant ~loc c
                      else fun loc ->
                        Exp.constant ~loc (Pconst_integer ("-" ^ str, suf))
                  | Pexp_constant (Pconst_float (str, suf) as c) ->
                      if Char.equal str.[0] '-' then fun loc ->
                        Exp.constant ~loc c
                      else fun loc ->
                        Exp.constant ~loc (Pconst_float ("-" ^ str, suf))
                  | _ ->
                      fun loc ->
                        Exp.apply ~loc (Exp.ident ~loc:op.loc op)
                          [(Nolabel, expr)])
              <*> loc
                    (choice ~name:"expression:unary:minus"
                       [minus_dot >>$ "~-."; minus >>$ "~-"])
              <*>. unary
          ; with_loc
            & mapping (fun op expr ->
                  let op = str2lid op in
                  match expr.pexp_desc with
                  | Pexp_constant (Pconst_integer (str, suf)) ->
                      fun loc -> Exp.constant ~loc (Pconst_integer (str, suf))
                  | Pexp_constant (Pconst_float (str, suf)) ->
                      fun loc -> Exp.constant ~loc (Pconst_float (str, suf))
                  | _ ->
                      fun loc ->
                        Exp.apply ~loc (Exp.ident ~loc:op.loc op)
                          [(Nolabel, expr)])
              <*> loc
                    (choice ~name:"expression:unary:plus"
                       [plus_dot >>$ "~+."; plus >>$ "~+"])
              <*>. unary
          ; with_loc
            & mapping (fun op expr loc ->
                  let op = str2lid op in
                  Exp.apply ~loc (Exp.ident ~loc:op.loc op) [(Nolabel, expr)])
              <*> loc unops <*>. unary
          ; set ]

      let p0 =
        named "expression:p0" & exp_attrs & Sugar.await
        & choice ~name:"expression:p0:p" [ifthenelse; unary]

      let expression_p0 = p0

      let left_assoc mp ops =
        let tail =
          mapping (fun op right left ->
              let op = str2lid op in
              Exp.apply
                ~loc:(loc_comb left.pexp_loc right.pexp_loc)
                (Exp.ident ~loc:op.loc op)
                [(Nolabel, left); (Nolabel, right)])
          <*>. loc ops <*>. mp
        in
        fold_left_cont_0_n mp tail

      let left_assoc2 mp ops =
        let tail =
          mapping (fun op right left ->
              let op = str2lid op in
              Exp.apply
                ~loc:(loc_comb left.pexp_loc right.pexp_loc)
                (Exp.ident ~loc:op.loc op)
                [(Nolabel, left); (Nolabel, right)])
          <*> ops <*>. mp
        in
        fold_left_cont_0_n mp tail

      let p1 = named "expression:p1" @@ left_assoc p0 (minus_gt >>$ "|.")

      let p2 =
        named "expression:p2" @@ left_assoc p1 (asterisk_asterisk >>$ "**")

      let p3 =
        named "expression:p3"
        @@ left_assoc p2
             (choice ~name:"expression:p3:op"
                [ asterisk_dot >>$ "*."
                ; asterisk >>$ "*"
                ; slash_dot >>$ "/."
                ; slash >>$ "/" ])

      let p4 =
        let unary_on_new_line =
          ng_new_line
          >> choice ~name:"expression:p4:unl" [minus_dot; minus]
          >> failed ng_not_empty >> p0
        in
        named "expression:p4"
        @@ left_assoc2 p3
             (choice ~name:"expression:p4:op"
                [ ng
                  >> loc
                       (choice ~name:"expression:p4:op:1"
                          [plus_plus >>$ "^"; plus_dot >>$ "+."; plus >>$ "+"])
                ; failed unary_on_new_line >> ng
                  >> loc
                       (choice ~name:"expression:p4:op:2"
                          [minus_dot >>$ "-."; minus >>$ "-"]) ])

      let p5 =
        named "expression:p5"
        @@ left_assoc p4
             (choice ~name:"expression:p5:op"
                [ eq_eq_eq >>$ "=="
                ; eq_eq >>$ "="
                ; eq_op >>$ "="
                ; bang_eq_eq >>$ "!="
                ; bang_eq >>$ "<>"
                ; lt_eq >>$ "<="
                ; gt_eq >>$ ">="
                ; pipe_gt >>$ "|>"
                ; failed jsx >> failed (lt >> ng >> slash) >> lt >>$ "<"
                ; gt >>$ ">" ])

      let p6 =
        named "expression:p6" @@ left_assoc p5 (ampersand_ampersand >>$ "&&")

      let p7 = named "expression:p7" @@ left_assoc p6 (pipe_pipe >>$ "||")

      let p8 =
        named "expression:p8"
        @@ left_assoc p7
             (choice ~name:"expression:p8:op"
                [hash_eq >>$ "#="; colon_eq >>$ ":="])

      let ternary =
        named "expression:ternary"
          (with_loc
          & mapping (fun e1 e2 e3 loc ->
                Exp.ifthenelse ~loc
                  ~attrs:[Hc.attr "ns.ternary"]
                  e1 e2 (Some e3))
            <*> p8 <<. question <*>. expression_fun <<. colon
            <*>. expression_fun)

      let expression = choice ~name:"expression" [ternary; p8]
      let expression_fun = choice ~name:"expression_fun" [fun'; expression]
    end)

  let expression =
    let (module M) = x in
    M.expression

  let expression_fun =
    let (module M) = x in
    M.expression_fun

  let expression_sequence =
    let (module M) = x in
    M.expression_sequence

  let expression_p0 =
    let (module M) = x in
    M.expression_p0
end
