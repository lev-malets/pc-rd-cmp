
open Base
open Basic
open Parsetree
open Asttypes
open Ast_helper

module Make
        (Basic: Sigs.BASIC)
        (Core: Basic.CORE)
        (Type: Basic.TYPE)
        (Pattern: Basic.PATTERN)
        (Modexpr: Basic.MODEXPR)
        = struct
    open Basic
    open Core
    open Type
    open Pattern
    open Modexpr
    open Comb

    module Comb = Comb

    type arrow_parser_helpers = {
        args_loop : expression Comb.t;
        types_loop : expression Comb.t;
    }

    let x = fix_poly @@ fun getter ->
        (module struct
            module Comb = Comb

            let expression = getter.get @@ fun (module M: EXPRESSION) -> M.expression
            let expression_fun = getter.get @@ fun (module M) -> M.expression_fun
            let expression_sequence = getter.get @@ fun (module M) -> M.expression_sequence
            let expression_p0 = getter.get @@ fun (module M) -> M.expression_p0

            let expression_constrainted =
                named "expression:constrainted" begin
                        with_loc & hlp2 Exp.constraint_
                        +expression_fun -ng -colon -ng +core_type

                    ||  expression_fun
                end

            let value_binding_list =
                let value_binding =
                    with_loc & (hlp2 @@ Vb.mk ?docs:None ?text:None)
                    +pattern -ng -eq -ng +expression_fun
                in

                seq ~n:1 value_binding ~sep:(ng >> and' >> ng)

            let let' =
                let rec_flag = rec' >>$ Recursive <|> return Nonrecursive in

                named "expression:let" begin
                    with_loc & hlp3 Exp.let_
                    -let' -ng +rec_flag -ng +value_binding_list -del
                    +(ng >> expression_sequence || return @@ Hc.unit_expr Location.none)
                end

            let letmodule =
                named "expression:module" begin
                    with_loc & hlp3 Exp.letmodule
                    -module' -ng +loc u_ident -ng -eq -ng +modexpr -del -ng +expression_sequence
                end

            let pack =
                named "expression:pack" begin
                        with_loc & mapping (fun m t loc -> Exp.constraint_ ~loc (Exp.pack m) t)
                        -module' -ng -l_paren -ng +modexpr -ng -colon -ng +core_type_package -ng -r_paren

                    ||  with_loc & hlp Exp.pack
                        -module' -ng -l_paren -ng +modexpr -ng -r_paren
                end

            let letopen =
                named "expression:open" begin
                    let override = (bang >>$ Override) <|> return Fresh in

                    with_loc & hlp3 Exp.open_
                    -open' +override -ng +loc u_longident -del -ng +expression_sequence
                end

            let letexception =
                named "expression:exception" begin
                    with_loc & hlp2 Exp.letexception
                    -exception' -ng +type_extension_constructor -del -ng +expression_sequence
                end

            let expression_in_braces =
                memo & named "expression:in_braces" begin
                        peek_first
                        [ let'
                        ; letmodule
                        ; letopen
                        ; letexception
                        ]
                    ||  expression_fun
                end

            let expression_sequence =
                fix & fun expression_sequence ->

                memo & named "expression:sequence" begin
                        with_loc & hlp2 Exp.sequence
                        +expression_in_braces -del -ng +expression_sequence

                    ||  exp_attrs expression_in_braces
                end


            let scoped_sequence = l_brace >> ng >> expression_sequence << del << ng << r_brace

            let fun' =
                memo & named "expression:arrow" begin
                    let arrow_tail =
                        let tail = arrow >> ng >> expression_fun in
                            mapping begin fun typ expr -> Exp.constraint_ ~loc:expr.pexp_loc expr typ end
                            -colon -ng +core_type_atom -ng +tail

                        ||  tail
                    in

                    let label = tilda >> ng >> loc l_ident in

                    let with_label =
                            mapping begin fun { txt; loc } pat ->
                                txt, pat_add_attr "ns.namedArgLoc" ~loc pat
                            end
                            +label -ng -as' -ng +pattern_constrainted

                        ||  mapping begin fun ({ txt; loc } as var) typ ->
                                txt,
                                Pat.constraint_
                                    ~loc:(loc_comb loc typ.ptyp_loc)
                                    ~attrs:[Hc.attr "ns.namedArgLoc" ~loc]
                                    (Pat.var ~loc var) typ
                            end
                            +label -ng -colon -ng +core_type_atom

                        ||  mapping begin fun ({ txt; loc } as var) ->
                                txt,
                                Pat.var ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] var
                            end
                            +label
                    in

                    let with_value =
                            mapping begin fun (s, pat) -> Optional s, pat, None end
                            +with_label -ng -eq -ng -question

                        ||  mapping begin fun (s, pat) expr -> Optional s, pat, Some expr end
                            +with_label -ng -eq -ng +expression_constrainted

                        ||  mapping begin fun (s, pat) -> Labelled s, pat, None end
                            +with_label
                    in

                    let nolabel =
                        (
                                with_loc & hlp2 Pat.constraint_
                                +pat_attrs pattern -ng -colon -ng +core_type

                            ||  pat_attrs pattern
                        ) >>| fun p -> Nolabel, p, None
                    in

                    let constr_unit =
                        named "expression:arrow:unit" begin
                            let unit = l_paren >> r_paren >>$ Hc.lid ["()"] in
                            let pat = loc unit >>| fun lid -> Pat.construct ~loc:lid.loc lid None in

                            mapping (fun p1 pat exp p2 -> Exp.fun_ ~loc:(make_location p1 p2) Nolabel None pat exp)
                            +pos +pat -ng +arrow_tail +pos
                        end
                    in

                    let with_many_args =
                        let loop = fix_poly @@ fun getter ->
                            let args_loop = getter.get @@ fun x -> x.args_loop in
                            let types_loop = getter.get @@ fun x -> x.types_loop in

                            let tail =
                                    r_paren >> ng >> arrow_tail
                                ||  comma >> ng >> r_paren >> ng >> arrow_tail
                                ||  comma >> ng >> exp_attrs (type' >> ng >> types_loop)
                                ||  comma >> ng >> args_loop
                            in

                            let curried =
                                    exp_attrs & mapping begin fun p1 (label, pat, expr) tail p2 ->
                                        Exp.fun_ ~loc:(make_location p1 p2) label expr pat tail
                                    end
                                    +pos +with_value -ng +tail +pos

                                ||  mapping begin fun p1 (label, pat, expr) tail p2 ->
                                        Exp.fun_ ~loc:(make_location p1 p2) label expr pat tail
                                    end
                                    +pos +nolabel -ng +tail +pos
                            in

                            {
                                args_loop =
                                        dot >> ng >> curried >>| exp_add_attr "bs"
                                    ||  curried
                                ;
                                types_loop = begin
                                        with_loc & hlp2 Exp.newtype
                                        +loc l_ident -ng +types_loop

                                    ||  comma >> ng >> exp_attrs (type' >> ng >> types_loop)

                                    ||  comma >> ng >> args_loop
                                end;
                            }
                        in

                        named "expression:arrow:many_args" begin
                                exp_attrs & l_paren >> ng >> exp_attrs (type' >> ng >> loop.types_loop)
                            ||  exp_attrs & l_paren >> ng >> loop.args_loop
                        end
                    in

                    let constr_unit_uncurried =
                        let unit =
                            return (Longident.Lident "()")
                            -l_paren -ng -dot -ng -r_paren
                        in
                        let pat = loc unit >>| fun lid -> Pat.construct ~loc:lid.loc lid None in

                        named "expression:arrow:unit_uncur" begin
                            mapping begin fun p1 pat expr p2 ->
                                Exp.fun_ ~loc:(make_location p1 p2) ~attrs:[Hc.attr "bs"] Nolabel None pat expr
                            end
                            +pos +pat -ng +arrow_tail +pos
                        end
                    in

                    let only_arg =
                        named "expression:arrow:one_arg" begin
                            exp_attrs & mapping (fun p1 p e p2 -> Exp.fun_ ~loc:(make_location p1 p2) Nolabel None p e)
                            +pos +pattern_atom -ng -arrow -ng +expression_fun +pos
                        end
                    in

                    constr_unit <|> with_many_args <|> constr_unit_uncurried <|> only_arg
                end

            let tuple =
                named "expression:tuple" begin
                    mapping (fun p1 l p2 -> Exp.tuple ~loc:(make_location p1 p2) l)
                    +pos -l_paren -ng +(seq ~n:2 expression_constrainted ~sep ~trail) -ng -r_paren  +pos
                end

            let list_helper =
                make_list_helper ~constr:Exp.construct ~tuple:Exp.tuple ~get_loc:(fun x -> x.pexp_loc)

            let list =
                named "expression:list" begin
                        with_loc & mapping (list_helper [] None)
                        -list -ng -r_brace

                    ||  list >> ng >> ellipsis >> ng >> expression_constrainted << opt sep << ng << r_brace

                    ||  with_loc & mapping list_helper
                        -list -ng +(seq ~n:1 ~sep expression_constrainted)
                        +(
                                ng >> comma >> ng >> ellipsis >> ng >> expression_constrainted << opt sep >>| Option.some
                            ||  opt sep >>$ None
                        )
                        -ng -r_brace
                end

            let case =
                mapping begin fun pat guard exp -> Exp.case pat ?guard exp end
                +pattern -ng +opt(when' <|> if' >> ng >> expression) -ng -arrow -ng +expression_sequence

            let case_list =
                l_brace >> opt (ng >> pipe) >> seq ~n:1 (ng >> case) ~sep:(ng >> pipe) << ng << r_brace

            let switch =
                named "expression:switch" begin
                    with_loc & hlp2 Exp.match_
                    -switch -ng +expression -ng +case_list
                end

            let try_catch =
                named "expression:try" begin
                    with_loc & hlp2 Exp.try_
                    -try' -ng +expression -ng -catch -ng +case_list
                end

            let ifthenelse =
                fix @@ fun ifthenelse ->
                let else_part = ng >> else' >> ng >> (scoped_sequence <|> ifthenelse) in

                named "expression:if" begin
                    with_loc & hlp3 Exp.ifthenelse
                    -if' -ng +expression -ng +scoped_sequence +opt(else_part)
                end

            let for_ =
                named "expression:for" begin
                    let direction = to' >>$ Upto || downto' >>$ Downto in
                    let mapping =
                        mapping begin fun p1 pat ef dir et ea p2 ->
                            Exp.for_ ~loc:(make_location p1 p2) pat ef et dir ea
                        end
                    in

                        mapping
                        +pos -for' -ng +pattern -ng -in' -ng +expression -ng +direction
                        -ng +expression -ng +scoped_sequence +pos

                    ||  mapping
                        +pos -for' -ng -l_paren -ng +pattern -ng -in' -ng +expression
                        -ng +direction -ng +expression -ng -r_paren  -ng +scoped_sequence +pos
                end

            let while_ =
                named "expression:while" begin
                    with_loc & hlp2 Exp.while_
                    -while' -ng +expression -ng +scoped_sequence
                end

            let assert_ =
                named "expression:assert" begin
                    with_loc & hlp Exp.assert_
                    -assert' -ng +expression
                end

            let lazy_ =
                named "expression:lazy" begin
                    with_loc & hlp Exp.lazy_
                    -lazy' -ng +expression
                end

            let _unit_arg_ loc = Nolabel, Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
            let _unit_arg = _unit_arg_ Location.none

            let ident =
                with_loc & hlp Exp.ident
                +loc l_longident

            let jsx =
                named "expression:jsx" begin
                    let arg =
                            mapping begin fun { txt; loc } expr ->
                                Labelled txt, exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            +loc l_ident -ng -eq -ng +expression_p0

                        ||  mapping begin fun { txt; loc } expr ->
                                Optional txt, exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            +loc l_ident -ng -eq -ng -question -ng +expression_p0

                        ||  mapping begin fun { txt; loc } ->
                                Labelled txt, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                            end
                            +loc l_ident

                        ||  mapping begin fun { txt; loc } ->
                                Optional txt, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                            end
                            -question -ng +loc l_ident
                    in

                    let children_list =
                        with_loc & mapping (fun list -> list_helper list None)
                        +(seq expression ~sep:ng)
                    in

                    let children =
                            ellipsis >> expression
                        ||  children_list
                    in

                        named "jsx:leaf" begin
                            mapping begin fun p1 tag args p2 -> Exp.apply ~loc:(make_location p1 p2) (Exp.ident tag) (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg
                                ])
                            end
                            +pos -lt -ng +loc (l_ident >>| fun x -> Longident.Lident x) +seq (ng >> arg) -ng -slash -ng -gt +pos
                        end
                    ||  named "jsx:tag" begin
                            run & mapping begin fun p1 tag args children tag2 p2 ->
                                let open Simple in
                                if String.(tag.txt <> tag2) then
                                    fail
                                else
                                    let tag = { tag with txt = Longident.Lident tag.txt } in
                                    let tailargs =
                                        (Labelled "children", children) :: [_unit_arg]
                                    in
                                    let e = Exp.apply ~loc:(make_location p1 p2) (Exp.ident tag) (args @ tailargs) in
                                    return e
                            end
                            +pos -lt -ng +loc(l_ident) +seq (ng >> arg)
                            -ng -gt -ng +children -ng -lt -ng -slash -ng +l_ident -ng -gt +pos_end
                        end
                    ||  named "jsx:ce:leaf" begin
                            mapping begin fun p1 tag args p2 ->
                                let tag =
                                    Exp.ident ~loc:tag.loc @@ Location.mkloc (Longident.Ldot (tag.txt, "createElement")) tag.loc
                                in

                                Exp.apply ~loc:(make_location p1 p2) tag (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg])
                            end
                            +pos -lt -ng +loc longident +seq (ng >> arg) -ng -slash -ng -gt +pos_end
                        end
                    ||  named "jsx:ce:tag" begin
                            run & mapping begin fun p1 tag args children tag2 p2 ->
                                let open Simple in
                                if Poly.(tag.txt <> tag2) then
                                    fail
                                else
                                    let tailargs =
                                        (Labelled "children", children) :: [_unit_arg]
                                    in
                                    let tag =
                                        Exp.ident ~loc:tag.loc { tag with txt = Longident.Ldot (tag.txt, "createElement") }
                                    in
                                    let e =
                                        Exp.apply ~loc:(make_location p1 p2) tag (args @ tailargs)
                                    in
                                    return e
                            end
                            +pos -lt -ng +loc longident +seq (ng >> arg) -ng -gt
                            -ng +children -ng -lt -ng -slash -ng +longident -ng -gt +pos_end
                        end
                    ||  named "jsx:notag:item" begin
                            with_loc & mapping begin fun expr -> list_helper [expr] None end
                            -lt -ng -gt -ng -ellipsis -ng +expression -ng -lt -ng -slash -ng -gt
                        end
                    ||  named "jsx:notag" begin
                            lt >> ng >> gt >> ng >> children_list << ng << lt << ng << slash << ng << gt
                        end
                end

            let jsx = jsx >>| exp_add_attr "JSX"

            let array =
                named "expression:array" begin
                        mapping (fun p1 p2 -> Exp.array ~loc:(make_location p1 p2) [])
                        +pos -l_bracket -ng -r_bracket +pos

                    ||  mapping (fun p1 l p2 -> Exp.array ~loc:(make_location p1 p2) l)
                        +pos -l_bracket -ng +(seq ~n:1 ~sep ~trail expression_constrainted) -ng -r_bracket +pos
                end

            let record =
                named "expression:record" begin
                    let nb =
                            t2
                            +loc l_longident -ng -colon -ng +expression_fun

                        ||  loc l_longident >>| fun lid -> lid, Exp.ident ~loc:lid.loc lid
                    in

                    with_loc & mapping begin fun expr list loc -> Exp.record ~loc list expr end
                    -l_brace -ng +opt(ng >> ellipsis >> ng >> expression_constrainted << sep)
                    +(seq ~n:1 nb ~sep ~trail) -ng -r_brace
                end

            let bs_object =
                let record =
                    let row =
                        mapping begin fun {txt; loc} pat ->
                            let lid = Location.mkloc (Longident.Lident txt) loc in
                            match pat with
                            | Some pat -> lid, pat
                            | None -> lid, Exp.ident ~loc lid
                        end
                        +loc string_raw +opt(ng >> colon >> ng >> expression)
                    in

                    mapping begin fun p1 list p2 ->
                        Exp.record ~loc:(make_location p1 p2) list None
                    end
                    +pos -l_brace -ng +(seq ~n:1 row ~sep ~trail) -ng -r_brace +pos
                in

                named "bs_object" begin
                    record >>| fun x -> Exp.extension ~loc:x.pexp_loc (Location.mkloc "obj" x.pexp_loc, PStr [Str.eval x])
                end

            let extension =
                with_loc & hlp Exp.extension
                +extension

            let coerce =
                named "expression:coerce" begin
                    mapping (fun p1 a b p2 -> Exp.coerce ~loc:(make_location p1 p2) a None b)
                    +pos -l_paren -ng +expression_constrainted -ng -colon_gt -ng +core_type_atom -ng -r_paren  +pos
                end

            let string =
                named "expression:string" begin
                    with_loc & hlp Exp.constant
                    +string_multiline ~q:"\""
                end

            let js_string = named "expression:js_string" @@ interpolated_string ~quote_tag:"js" ~expression
            let json_string =
                named "expression:json_string" &
                json_tag >> ng >> interpolated_string ~quote_tag:"json" ~expression

            let constr_args =
                    loc_of (l_paren >> ng >> r_paren) >>| Hc.unit_expr
                ||  tuple
                ||  l_paren >> ng >> expression_constrainted << opt sep << ng << r_paren


            let polyvariant =
                named "polyvariant" begin
                    with_loc & hlp2 Exp.variant

                    +variant_tag +opt(constr_args)
                end

            let true_ =
                with_loc & mapping (fun a loc -> Exp.construct ~loc a None)
                +loc (true' >>$ Longident.Lident "true")

            let false_ =
                with_loc & mapping (fun a loc -> Exp.construct ~loc a None)
                +loc (false' >>$ Longident.Lident "false")

            let string_ident =
                with_loc & hlp Exp.ident
                +loc (string_ident >>| fun x -> Longident.Lident x)

            let atom =
                named "expression:atom" begin
                    peek_first
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
                    ;
                        with_loc & hlp2 Exp.construct
                        +loc u_longident +opt(constr_args)
                    ; polyvariant
                    ;
                        with_loc & mapping (fun a loc -> Exp.construct ~loc a None)
                        +loc (l_paren >> ng >> r_paren >>$ Longident.Lident "()")
                    ; tuple
                    ; coerce
                    ;
                        with_loc & parens & mapping exp_loc
                        +exp_attrs expression_constrainted
                    ; array
                    ; scoped_sequence >>| exp_add_attr "ns.braces"
                    ; record
                    ; bs_object
                    ; extension
                    ;
                        with_loc & hlp Exp.constant
                        +constant
                    ]
                end

            let res_unit loc =
                let pattern = Pat.var @@ Location.mknoloc "__res_unit" in
                let expr = Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None in
                let expr2 = Exp.ident @@ Location.mknoloc @@ Longident.Lident "__res_unit" in
                let vb = Vb.mk ?docs:None ?text:None pattern expr in
                Exp.let_ Nonrecursive [vb] expr2

            let field_set_cont =
                named "expression:field:set" begin
                    mapping begin fun lid expr prev ->
                        let loc = loc_comb prev.pexp_loc expr.pexp_loc in
                        Exp.setfield ~loc prev lid expr
                    end
                    -ng -dot -ng +loc l_longident -ng -eq -ng +expression_fun
                end

            let object_set_cont =
                memo & named "expression:object:set" &
                mapping begin fun idx p1 loc value prev ->
                    let s = Exp.send ~loc:(make_location prev.pexp_loc.loc_start p1) prev idx in
                    let lid = Exp.ident ~loc @@ Location.mkloc (Longident.Lident "#=") loc in
                    Exp.apply ~loc:(loc_comb prev.pexp_loc value.pexp_loc) lid [Nolabel, s; Nolabel, value]
                end
                -ng -l_bracket -ng +loc string_raw -ng -r_bracket +pos -ng +loc_of (eq) -ng +expression_fun

            let array_set_cont =
                named "expression:array:set" begin
                    mapping begin fun idx value prev ->
                        let loc = loc_comb prev.pexp_loc value.pexp_loc in
                        Exp.apply ~loc (Hc.expr_id ["Array"; "set"]) [Nolabel, prev; Nolabel, idx; Nolabel, value]
                    end
                    -ng -l_bracket -ng +expression -ng -r_bracket -ng -eq -ng +expression_fun
                end

            let tail =
                named "expression:tail" begin
                    let labelled special_arg =
                            mapping begin fun { txt; loc } expr ->
                                Optional txt,
                                exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            -tilda -ng +loc l_ident -ng -eq -ng -question -ng +expression_constrainted
                        ||  mapping begin fun tag expr -> Optional tag, expr end
                            -tilda -ng +l_ident -ng -eq -ng -question -ng +special_arg

                        ||  mapping begin fun { txt; loc } expr ->
                                Labelled txt,
                                exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            -tilda -ng +loc l_ident -ng -eq -ng +expression_constrainted

                        ||  mapping begin fun tag expr -> Labelled tag, expr end
                            -tilda -ng +l_ident -ng -eq -ng +special_arg

                        ||  mapping begin fun { txt; loc } typ p2 ->
                                let exp =
                                    Exp.constraint_
                                        ~loc:(make_location loc.loc_start p2)
                                        ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                                        (Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]) typ
                                in
                                Labelled txt, exp
                            end
                            -tilda -ng +loc l_ident -ng -colon -ng +core_type_atom +pos

                        ||  mapping begin fun { txt; loc } p2 ->
                                let exp =
                                    Exp.ident
                                        ~loc:(make_location loc.loc_start p2)
                                        ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                                        (Location.mkloc (Hc.lid [txt]) loc)
                                in
                                Optional txt, exp
                            end
                            -tilda -ng +loc l_ident -ng -question +pos

                        ||  mapping begin fun { txt; loc } ->
                                Labelled txt
                                ,
                                Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]
                            end
                            -tilda -ng +loc l_ident

                        ||  (expression_constrainted <|> special_arg) >>| fun e -> Nolabel, e
                    in

                    let apply =
                        named "expression:apply" begin
                            let flags = ref [] in

                            let special_arg =
                                mapping (fun loc -> Exp.ident ~loc @@ Location.mkloc (Longident.Lident "__x") loc)
                                +loc_of _' -exec (fun _ -> List.hd_exn !flags := true)
                            in

                            let params =
                                    l_paren >> ng >> seq ~n:1 ~sep ~trail (labelled special_arg) << ng << r_paren
                                ||  l_paren >> ng >> loc_of (r_paren) >>| fun loc -> [_unit_arg_ loc]
                            in

                            let drop_flag = exec @@ fun _ ->
                                let f = !(List.hd_exn !flags) in
                                flags := List.tl_exn !flags;
                                f
                            in

                            exec (fun _ -> flags := ref false :: !flags) >>
                            (
                                mapping begin fun list flag p2 prev ->
                                    let loc = { prev.pexp_loc with loc_end = p2 } in
                                    if flag then
                                        let x = Exp.apply ~loc prev list in
                                        Exp.fun_ ~loc Nolabel None (Pat.var @@ Location.mknoloc "__x") x
                                    else
                                        Exp.apply ~loc prev list
                                end
                                -ng_no_new_line +params +drop_flag +pos
                            ) <|> (drop_flag >> fail)
                        end
                    in

                    let apply_uncurried =
                        named "expression:apply:uncurried" begin
                            let apply_params =
                                dot >> ng >> seq ~n:1 ~sep (labelled fail)
                            in

                            let args =
                                named "expression:apply:uncurried:tail" begin
                                    fold_left_cont_0_n
                                        (
                                            mapping begin fun args p2 ->
                                                (fun expr -> Exp.apply ~loc:{expr.pexp_loc with loc_end = p2} ~attrs:[Hc.attr "bs"] expr args)
                                            end
                                            +apply_params +pos
                                        )
                                        (
                                            mapping begin fun args p3 cont ->
                                                begin fun expr ->
                                                    let expr = cont expr in
                                                    Exp.apply ~loc:{expr.pexp_loc with loc_end = p3 } ~attrs:[Hc.attr "bs"] expr args
                                                end
                                            end
                                            -ng -comma -ng +apply_params +pos
                                        )
                                end
                            in

                            let unit_arg =
                                    mapping (fun loc -> [Nolabel, res_unit loc])
                                    -l_paren -ng -dot -ng +loc_of (l_paren >> ng >> r_paren) -ng -r_paren

                                ||  return [_unit_arg]
                                    -l_paren -ng -dot -ng -r_paren
                            in

                                mapping begin fun arg loc_end prev ->
                                    Exp.apply ~loc:{prev.pexp_loc with loc_end} ~attrs:[Hc.attr "bs"] prev arg
                                end
                                -ng +unit_arg +pos
                            ||  ng >> l_paren >> ng >> args << opt sep << ng << r_paren
                        end
                    in

                    let field =
                        failed field_set_cont >>
                        named "expression:field" begin
                            mapping begin fun lid loc_end prev ->
                                Exp.field ~loc:{prev.pexp_loc with loc_end} prev lid
                            end
                            -ng -dot -ng +loc l_longident +pos
                        end
                    in

                    let object_get =
                        failed object_set_cont >>
                        named "expression:send" begin
                            mapping begin fun str loc_end prev ->
                                Exp.send ~loc:{prev.pexp_loc with loc_end} prev str
                            end
                            -ng -l_bracket -ng +loc string_raw -ng -r_bracket +pos
                        end
                    in

                    let array_get =
                        failed array_set_cont >>
                        named "expression:index" begin
                            mapping begin fun idx loc_end prev ->
                                Exp.apply ~loc:{prev.pexp_loc with loc_end} (Hc.expr_id ["Array"; "get"]) [Nolabel, prev; Nolabel, idx]
                            end
                            -ng -l_bracket -ng +expression_constrainted -ng -r_bracket +pos
                        end
                    in

                    apply <|> apply_uncurried <|> field <|> object_get <|> array_get
                end

            let primary =
                named "expression:primary" begin
                    fold_left_cont_0_n atom tail
                end

            let set =
                named "expression:set" begin
                    let set_cont =
                        named "expression:set:cont" begin
                            field_set_cont <|> object_set_cont <|> array_set_cont
                        end
                    in

                    fold_left_cont_0_1 primary set_cont
                end

            let unops = bang

            let unary = fix @@ fun unary ->
                    with_loc & mapping begin fun op expr ->
                        let op = unop2longident op in
                        match expr.pexp_desc with
                        | Pexp_constant (Pconst_integer (str, suf) as c) ->
                            if Char.equal str.[0] '-' then
                                (fun loc -> Exp.constant ~loc c)
                            else
                                (fun loc -> Exp.constant ~loc (Pconst_integer ("-" ^ str, suf)))
                        | Pexp_constant (Pconst_float (str, suf) as c) ->
                            if Char.equal str.[0] '-' then
                                (fun loc -> Exp.constant ~loc c)
                            else
                                (fun loc -> Exp.constant ~loc (Pconst_float ("-" ^ str, suf)))
                        | _ -> (fun loc -> Exp.apply ~loc (Exp.ident ~loc:op.loc op) [Nolabel, expr])
                    end
                    +loc (minus_dot <|> minus) -ng +unary

                ||  with_loc & mapping begin fun op expr ->
                        let op = unop2longident op in
                        match expr.pexp_desc with
                        | Pexp_constant (Pconst_integer (str, suf)) ->
                            (fun loc -> Exp.constant ~loc (Pconst_integer (str, suf)))
                        | Pexp_constant (Pconst_float (str, suf)) ->
                            (fun loc -> Exp.constant ~loc (Pconst_float (str, suf)))
                        | _ ->
                            (fun loc -> Exp.apply ~loc (Exp.ident ~loc:op.loc op) [Nolabel, expr])
                    end
                    +loc (plus_dot <|> plus) -ng +unary

                ||  with_loc & mapping begin fun op expr loc ->
                        let op = unop2longident op in
                        Exp.apply ~loc (Exp.ident ~loc:op.loc op) [Nolabel, expr]
                    end
                    +loc unops -ng +unary

                ||  set

            let p0 =
                memo & named "expression:p0" @@
                exp_attrs (ifthenelse <|> unary)

            let expression_p0 = p0

            let left_assoc mp ops =
                let tail =
                    mapping begin fun op right left ->
                        let op = binop2longident op in
                        Exp.apply
                            ~loc:(loc_comb left.pexp_loc right.pexp_loc)
                            (Exp.ident ~loc:op.loc op)
                            [Nolabel, left; Nolabel, right]
                    end
                    -ng +loc ops -ng +mp
                in

                fold_left_cont_0_n mp tail

            let left_assoc2 mp ops =
                let tail =
                    mapping begin fun op right left ->
                        let op = binop2longident op in
                        Exp.apply
                            ~loc:(loc_comb left.pexp_loc right.pexp_loc)
                            (Exp.ident ~loc:op.loc op)
                            [Nolabel, left; Nolabel, right]
                    end
                    +ops -ng +mp
                in

                fold_left_cont_0_n mp tail

            let p1 =
                named "expression:p1" @@
                left_assoc p0 minus_gt
            let p2 =
                named "expression:p2" @@
                left_assoc p1 asterisk_asterisk
            let p3 =
                named "expression:p3" @@
                left_assoc p2 (asterisk_dot <|> asterisk <|> slash_dot <|> slash)
            let p4 =
                named "expression:p4" @@
                left_assoc2 p3
                (
                        (ng >> loc (plus_plus <|> plus_dot <|> plus))
                    <|> (failed (ng_new_line >> (minus_dot <|> minus) >> p0) >> ng >> loc (minus_dot <|> minus))
                )
            let p5 =
                named "expression:p5" @@
                left_assoc p4
                (
                    peek_first
                    [ eq_eq_eq; eq_eq; eq_op
                    ; bang_eq_eq; bang_eq; lt_eq; gt_eq; pipe_gt
                    ; failed jsx >> failed (lt >> ng >> slash) >> lt; gt
                    ]
                )
            let p6 =
                named "expression:p6" @@
                left_assoc p5 ampersand_ampersand
            let p7 =
                named "expression:p7" @@
                left_assoc p6 pipe_pipe
            let p8 =
                memo & named "expression:p8" @@
                left_assoc p7 (hash_eq <|> colon_eq)

            let ternary =
                named "expression:ternary" begin
                    with_loc & mapping begin fun e1 e2 e3 loc ->
                        Exp.ifthenelse ~loc ~attrs:[Hc.attr "ns.ternary"] e1 e2 (Some e3)
                    end
                    +p8 -ng -question -ng +expression_fun -ng -colon -ng +expression_fun
                end

            let expression =
                memo & named "expression" begin
                        ternary
                    <|> p8
                end

            let expression_fun = fun' <|> expression
        end: EXPRESSION)

    let expression = let (module M) = x in M.expression
    let expression_fun = let (module M) = x in M.expression_fun
    let expression_sequence = let (module M) = x in M.expression_sequence
    let expression_p0 = let (module M) = x in M.expression_p0
end
