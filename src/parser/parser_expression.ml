
open Base
open Basic
open Parsetree
open Asttypes
open Ast_helper
open Sigs

module Make
        (APos: APOS)
        (Utils: UTILS) (Constant: CONSTANT)
        (Core: CORE) (Type: TYPE) (Pattern: PATTERN) (Modexpr: MODEXPR)
        = struct
    open APos
    open Utils
    open Constant
    open Core
    open Type
    open Pattern
    open Modexpr

    module Parser = APos.Parser

    type arrow_parser_helpers = {
        args_loop : expression Parser.t;
        types_loop : expression Parser.t;
    }

    let x = fix_poly @@ fun getter ->
        (module struct
            let expression = getter.get @@ fun (module M: EXPRESSION) -> M.expression
            let expression_arrow = getter.get @@ fun (module M) -> M.expression_arrow
            let expression_sequence = getter.get @@ fun (module M) -> M.expression_sequence
            let expression_p0 = getter.get @@ fun (module M) -> M.expression_p0

            let expression_constrainted =
                named "expression:constrainted" begin
                        with_loc & hlp2 Exp.constraint_
                        +expression_arrow -ng -s":" -ng +core_type

                    ||  expression_arrow
                end

            let value_binding_list =
                let value_binding =
                    with_loc & (hlp2 @@ Vb.mk ?docs:None ?text:None)
                    +pattern -ng -s"=" -ng +expression_arrow
                in

                seq ~n:1 value_binding ~sep:(ng >> k"and" >> ng)

            let let_ =
                let rec_flag = k"rec" >>$ Recursive <|> return Nonrecursive in

                named "expression:let" begin
                    with_loc & hlp3 Exp.let_
                    -k"let" -ng +rec_flag -ng +value_binding_list -del
                    +(ng >> expression_sequence || return @@ Hc.unit_expr Location.none)
                end

            let letmodule =
                named "expression:module" begin
                    with_loc & hlp3 Exp.letmodule
                    -k"module" -ng +loc u_ident -ng -s"=" -ng +modexpr -del -ng +expression_sequence
                end

            let pack =
                named "expression:pack" begin
                        with_loc & mapping (fun m t loc -> Exp.constraint_ ~loc (Exp.pack m) t)
                        -k"module" -ng -s"(" -ng +modexpr -ng -s":" -ng +core_type_package -ng -s")"

                    ||  with_loc & hlp Exp.pack
                        -k"module" -ng -s"(" -ng +modexpr -ng -s")"
                end

            let letopen =
                named "expression:open" begin
                    let override = (s"!" >>$ Override) <|> return Fresh in

                    with_loc & hlp3 Exp.open_
                    -k"open" +override -ng +loc u_longident -del -ng +expression_sequence
                end

            let letexception =
                named "expression:exception" begin
                    with_loc & hlp2 Exp.letexception
                    -k"exception" -ng +type_extension_constructor -del -ng +expression_sequence
                end

            let expression_in_braces =
                memo & named "expression:in_braces" begin
                        peek_first
                        [ let_
                        ; letmodule
                        ; letopen
                        ; letexception
                        ]
                    ||  expression_arrow
                end

            let expression_sequence =
                fix & fun expression_sequence ->

                memo & named "expression:sequence" begin
                        with_loc & hlp2 Exp.sequence
                        +expression_in_braces -del -ng +expression_sequence

                    ||  exp_attrs expression_in_braces
                end


            let scoped_sequence = s"{" >> ng >> expression_sequence << del << ng << s"}"

            let arrow =
                memo & named "expression:arrow" begin
                    let arrow_tail =
                        let tail = s"=>" >> ng >> expression_arrow in
                            mapping begin fun typ expr -> Exp.constraint_ ~loc:expr.pexp_loc expr typ end
                            -s":" -ng +core_type_atom -ng +tail

                        ||  tail
                    in

                    let label = s"~" >> ng >> loc l_ident in

                    let with_label =
                            mapping begin fun { txt; loc } pat ->
                                txt, pat_add_attr "ns.namedArgLoc" ~loc pat
                            end
                            +label -ng -k"as" -ng +pattern_constrainted

                        ||  mapping begin fun ({ txt; loc } as var) typ ->
                                txt,
                                Pat.constraint_
                                    ~loc:(loc_comb loc typ.ptyp_loc)
                                    ~attrs:[Hc.attr "ns.namedArgLoc" ~loc]
                                    (Pat.var ~loc var) typ
                            end
                            +label -ng -s":" -ng +core_type_atom

                        ||  mapping begin fun ({ txt; loc } as var) ->
                                txt,
                                Pat.var ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] var
                            end
                            +label
                    in

                    let with_value =
                            mapping begin fun (s, pat) -> Optional s, pat, None end
                            +with_label -ng -s"=" -ng -s"?"

                        ||  mapping begin fun (s, pat) expr -> Optional s, pat, Some expr end
                            +with_label -ng -s"=" -ng +expression_constrainted

                        ||  mapping begin fun (s, pat) -> Labelled s, pat, None end
                            +with_label
                    in

                    let nolabel =
                        (
                                with_loc & hlp2 Pat.constraint_
                                +pat_attrs pattern -ng -s":" -ng +core_type

                            ||  pat_attrs pattern
                        ) >>| fun p -> Nolabel, p, None
                    in

                    let constr_unit =
                        named "expression:arrow:unit" begin
                            let unit = s"()" >>$ Hc.lid ["()"] in
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
                                    s")" >> ng >> arrow_tail
                                ||  s"," >> ng >> s")" >> ng >> arrow_tail
                                ||  s"," >> ng >> exp_attrs (k"type" >> ng >> types_loop)
                                ||  s"," >> ng >> args_loop
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
                                        s"." >> ng >> curried >>| exp_add_attr "bs"
                                    ||  curried
                                ;
                                types_loop = begin
                                        with_loc & hlp2 Exp.newtype
                                        +loc l_ident -ng +types_loop

                                    ||  s"," >> ng >> exp_attrs (k"type" >> ng >> types_loop)

                                    ||  s"," >> ng >> args_loop
                                end;
                            }
                        in

                        named "expression:arrow:many_args" begin
                                exp_attrs & s"(" >> ng >> exp_attrs (k"type" >> ng >> loop.types_loop)
                            ||  exp_attrs & s"(" >> ng >> loop.args_loop
                        end
                    in

                    let constr_unit_uncurried =
                        let unit =
                            return (Longident.Lident "()")
                            -s"(" -ng -s"." -ng -s")"
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
                            +pos +pattern_atom -ng -s"=>" -ng +expression_arrow +pos
                        end
                    in

                    constr_unit <|> with_many_args <|> constr_unit_uncurried <|> only_arg
                end

            let tuple =
                named "expression:tuple" begin
                    mapping (fun p1 l p2 -> Exp.tuple ~loc:(make_location p1 p2) l)
                    +pos -s"(" -ng +(seq ~n:2 expression_constrainted ~sep ~trail) -ng -s")" +pos
                end

            let list_helper =
                make_list_helper ~constr:Exp.construct ~tuple:Exp.tuple ~get_loc:(fun x -> x.pexp_loc)

            let list =
                named "expression:list" begin
                        with_loc & mapping (list_helper [] None)
                        -s"list{" -ng -s"}"

                    ||  s"list{" >> ng >> s"..." >> ng >> expression_constrainted << opt sep << ng << s"}"

                    ||  with_loc & mapping list_helper
                        -s"list{" -ng +(seq ~n:1 ~sep expression_constrainted)
                        +(
                                ng >> s"," >> ng >> s"..." >> ng >> expression_constrainted << opt sep >>| Option.some
                            ||  opt sep >>$ None
                        )
                        -ng -s"}"
                end

            let case =
                mapping begin fun pat guard exp -> Exp.case pat ?guard exp end
                +pattern -ng +opt(k"when" <|> k"if" >> ng >> expression) -ng -s"=>" -ng +expression_sequence

            let case_list =
                s"{" >> opt (ng >> s"|") >> seq ~n:1 (ng >> case) ~sep:(ng >> s"|") << ng << s"}"

            let switch =
                named "expression:switch" begin
                    with_loc & hlp2 Exp.match_
                    -k"switch" -ng +expression -ng +case_list
                end

            let try_catch =
                named "expression:try" begin
                    with_loc & hlp2 Exp.try_
                    -k"try" -ng +expression -ng -k"catch" -ng +case_list
                end

            let ifthenelse =
                fix @@ fun ifthenelse ->
                let else_ = ng >> k"else" >> ng >> (scoped_sequence <|> ifthenelse) in

                named "expression:if" begin
                    with_loc & hlp3 Exp.ifthenelse
                    -k"if" -ng +expression -ng +scoped_sequence +opt(else_)
                end

            let for_ =
                named "expression:for" begin
                    let direction = k"to" >>$ Upto || k"downto" >>$ Downto in
                    let mapping =
                        mapping begin fun p1 pat ef dir et ea p2 ->
                            Exp.for_ ~loc:(make_location p1 p2) pat ef et dir ea
                        end
                    in

                        mapping
                        +pos -k"for" -ng +pattern -ng -k"in" -ng +expression -ng +direction
                        -ng +expression -ng +scoped_sequence +pos

                    ||  mapping
                        +pos -k"for" -ng -s"(" -ng +pattern -ng -k"in" -ng +expression
                        -ng +direction -ng +expression -ng -s")" -ng +scoped_sequence +pos
                end

            let while_ =
                named "expression:while" begin
                    with_loc & hlp2 Exp.while_
                    -k"while" -ng +expression -ng +scoped_sequence
                end

            let assert_ =
                named "expression:assert" begin
                    with_loc & hlp Exp.assert_
                    -k"assert" -ng +expression
                end

            let lazy_ =
                named "expression:lazy" begin
                    with_loc & hlp Exp.lazy_
                    -k"lazy" -ng +expression
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
                            +loc l_ident -ng -s"=" -ng +expression_p0

                        ||  mapping begin fun { txt; loc } expr ->
                                Optional txt, exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            +loc l_ident -ng -s"=" -ng -s"?" -ng +expression_p0

                        ||  mapping begin fun { txt; loc } ->
                                Labelled txt, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                            end
                            +loc l_ident

                        ||  mapping begin fun { txt; loc } ->
                                Optional txt, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [txt]
                            end
                            -s"?" -ng +loc l_ident
                    in

                    let children_list =
                        with_loc & mapping (fun list -> list_helper list None)
                        +(seq expression ~sep:ng)
                    in

                    let children =
                            s"..." >> expression
                        ||  children_list
                    in

                        named "jsx:leaf" begin
                            mapping begin fun p1 tag args p2 -> Exp.apply ~loc:(make_location p1 p2) (Exp.ident tag) (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg
                                ])
                            end
                            +pos -s"<" -ng +loc (l_ident >>| fun x -> Longident.Lident x) +seq (ng >> arg) -ng -s"/" -ng -s">" +pos
                        end
                    ||  named "jsx:tag" begin
                            t4
                            +pos -s"<" -ng +loc (l_ident >>| fun x -> Longident.Lident x) +seq (ng >> arg)
                            -ng -s">" -ng +children -ng
                            >>= fun (p1, tag, args, children) ->
                                let open Angstrom in
                                string "<" >> ng.p >> string "/" >> ng.p >> (exact_longident tag.txt).p << ng.p << string ">"
                                >> APos.pos.p
                                >>| fun p2 ->
                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in
                                Exp.apply ~loc:(make_location p1 p2) (Exp.ident tag) (args @ tailargs)
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
                            +pos -s"<" -ng +loc longident +seq (ng >> arg) -ng -s"/" -ng -s">" +pos
                        end
                    ||  named "jsx:ce:tag" begin
                            t4
                            +pos -s"<" -ng +loc longident +seq (ng >> arg) -ng -s">"
                            -ng +children -ng
                            >>= fun (p1, { txt = tag; loc = tag_loc }, args, children) ->
                                let open Angstrom in
                                string "<" >> ng.p >> string "/" >> ng.p >> (exact_longident tag).p << ng.p << string ">"
                                >> APos.pos.p
                                >>| fun p2 ->

                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in

                                let tag =
                                    Exp.ident ~loc:tag_loc @@ Location.mkloc (Longident.Ldot (tag, "createElement")) tag_loc
                                in

                                Exp.apply ~loc:(make_location p1 p2) tag (args @ tailargs)
                        end
                    ||  named "jsx:notag:item" begin
                            with_loc & mapping begin fun expr -> list_helper [expr] None end
                            -s"<" -ng -s">" -ng -s"..." -ng +expression -ng -s"<" -ng -s"/" -ng -s">"
                        end
                    ||  named "jsx:notag" begin
                            s"<" >> ng >> s">" >> ng >> children_list << ng << s"<" << ng << s"/" << ng << s">"
                        end
                end

            let jsx = jsx >>| exp_add_attr "JSX"

            let array =
                named "expression:array" begin
                        mapping (fun p1 p2 -> Exp.array ~loc:(make_location p1 p2) [])
                        +pos -s"[" -ng -s"]" +pos

                    ||  mapping (fun p1 l p2 -> Exp.array ~loc:(make_location p1 p2) l)
                        +pos -s"[" -ng +(seq ~n:1 ~sep ~trail expression_constrainted) -ng -s"]" +pos
                end

            let record =
                named "expression:record" begin
                    let nb =
                            t2
                            +loc l_longident -ng -s":" -ng +expression_arrow

                        ||  loc l_longident >>| fun lid -> lid, Exp.ident ~loc:lid.loc lid
                    in

                    with_loc & mapping begin fun expr list loc -> Exp.record ~loc list expr end
                    -s"{" -ng +opt(ng >> s"..." >> ng >> expression_constrainted << sep)
                    +(seq ~n:1 nb ~sep ~trail) -ng -s"}"
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
                        +loc string_raw +opt(ng >> s":" >> ng >> expression)
                    in

                    mapping begin fun p1 list p2 ->
                        Exp.record ~loc:(make_location p1 p2) list None
                    end
                    +pos -s"{" -ng +(seq ~n:1 row ~sep ~trail) -ng -s"}" +pos
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
                    +pos -s"(" -ng +expression_constrainted -ng -s":>" -ng +core_type_atom -ng -s")" +pos
                end

            let interpolated_string qtag =
                let op = Hc.expr_id ["^"] in

                let cons = cons in

                let parts = fix @@ fun parts ->
                    cons
                    +take_while (function '\n' | '$' | '`' | '\\' | '\r' -> false | _ -> true)
                    +(
                            cons +new_line +parts
                        ||  cons +(s"\\`" >>$ "`") +parts
                        ||  cons +(s"\\$" >>$ "$") +parts
                        ||  cons +(s"\\\\" >>$ "\\") +parts
                        ||  cons +(s"$" << failed (s"{")) +parts
                        ||  return []
                    )
                in
                let string =
                    mapping begin fun l p1 p2 ->
                        let str = String.concat l in
                        Exp.constant ~loc:(make_location p1 p2) ~attrs:[Hc.attr "res.template"] @@
                        Const.string ~quotation_delimiter:qtag str
                    end
                    +parts
                in

                let string_part =
                    fix @@ fun string_part ->

                    let tail =
                        mapping begin fun expr pos tail prev ->
                            tail (Exp.apply ~loc:(make_location prev.pexp_loc.loc_start pos) op [Nolabel, prev; Nolabel, expr])
                        end
                        -s"${" -ng +expression -ng -s"}" +pos +string_part
                    in

                    pos && string &&
                    (
                            mapping begin fun p2 str p1 prev ->
                                Exp.apply
                                    ~loc:{prev.pexp_loc with loc_end = p2}
                                    ~attrs:[Hc.attr "res.template"] op
                                    [
                                            Nolabel, prev;
                                            Nolabel, str p1 p2;
                                    ]
                            end
                            -s"`" +pos
                        ||  mapping begin fun p2 tail str p1 prev ->
                                tail @@ Exp.apply
                                    ~loc:{prev.pexp_loc with loc_end = p2}
                                    ~attrs:[Hc.attr "res.template"] op
                                    [
                                            Nolabel, prev;
                                            Nolabel, str p1 p2;
                                    ]
                            end
                            +pos +tail
                    )
                in

                pos &&
                (
                    s"`" >>
                    (
                        string &&
                        (
                                mapping (fun p2 str p1 -> str p1 p2)
                                -s"`" +pos

                            ||  mapping begin fun pos1 expr pos2 tail str p1 ->
                                    let e0 = str p1 pos1 in
                                    let e1 = Exp.apply
                                        ~loc:(make_location p1 pos2)
                                        ~attrs:[Hc.attr "res.template"]
                                        op
                                        [Nolabel, e0; Nolabel, expr]
                                    in
                                    tail e1
                                end
                                +pos -s"${" -ng +expression -ng -s"}" +pos +string_part
                        )
                    )
                )

            let string =
                named "expression:string" begin
                    with_loc & hlp Exp.constant
                    +string_multiline ~q:"\""
                end

            let js_string = named "expression:js_string" @@ interpolated_string "js"
            let json_string =
                named "expression:json_string" @@
                (s"json" >> interpolated_string "json")

            let constr_args =
                    loc_of (s"(" >> ng >> s")") >>| Hc.unit_expr
                ||  tuple
                ||  s"(" >> ng >> expression_constrainted << opt sep << ng << s")"


            let polyvariant =
                let tag =
                        u_ident
                    ||  string_raw
                    ||  take_while (function '0'..'9' -> true | _ -> false)
                in

                named "polyvariant" begin
                    with_loc & hlp2 Exp.variant
                    -s"#" +tag +opt(constr_args)
                end

            let true_ =
                with_loc & mapping (fun a loc -> Exp.construct ~loc a None)
                +loc (k"true" >>$ Longident.Lident "true")

            let false_ =
                with_loc & mapping (fun a loc -> Exp.construct ~loc a None)
                +loc (k"false" >>$ Longident.Lident "false")

            let string_ident =
                with_loc & hlp Exp.ident
                -s"\\" +loc (string_raw >>| fun x -> Longident.Lident x)

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
                        +loc (s"()" >>$ Longident.Lident "()")
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
                    -ng -s"." -ng +loc l_longident -ng -s"=" -ng +expression_arrow
                end

            let object_set_cont =
                memo & named "expression:object:set" &
                mapping begin fun idx p1 loc value prev ->
                    let s = Exp.send ~loc:(make_location prev.pexp_loc.loc_start p1) prev idx in
                    let lid = Exp.ident ~loc @@ Location.mkloc (Longident.Lident "#=") loc in
                    Exp.apply ~loc:(loc_comb prev.pexp_loc value.pexp_loc) lid [Nolabel, s; Nolabel, value]
                end
                -ng -s"[" -ng +loc string_raw -ng -s"]" +pos -ng +loc_of (s"=") -ng +expression_arrow

            let array_set_cont =
                named "expression:array:set" begin
                    mapping begin fun idx value prev ->
                        let loc = loc_comb prev.pexp_loc value.pexp_loc in
                        Exp.apply ~loc (Hc.expr_id ["Array"; "set"]) [Nolabel, prev; Nolabel, idx; Nolabel, value]
                    end
                    -ng -s"[" -ng +expression -ng -s"]" -ng -s"=" -ng +expression_arrow
                end

            let tail =
                named "expression:tail" begin
                    let labelled special_arg =
                            mapping begin fun { txt; loc } expr ->
                                Optional txt,
                                exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            -s"~" -ng +loc l_ident -ng -s"=" -ng -s"?" -ng +expression_constrainted
                        ||  mapping begin fun tag expr -> Optional tag, expr end
                            -s"~" -ng +l_ident -ng -s"=" -ng -s"?" -ng +special_arg

                        ||  mapping begin fun { txt; loc } expr ->
                                Labelled txt,
                                exp_add_attr "ns.namedArgLoc" ~loc expr
                            end
                            -s"~" -ng +loc l_ident -ng -s"=" -ng +expression_constrainted

                        ||  mapping begin fun tag expr -> Labelled tag, expr end
                            -s"~" -ng +l_ident -ng -s"=" -ng +special_arg

                        ||  mapping begin fun { txt; loc } typ p2 ->
                                let exp =
                                    Exp.constraint_
                                        ~loc:(make_location loc.loc_start p2)
                                        ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                                        (Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]) typ
                                in
                                Labelled txt, exp
                            end
                            -s"~" -ng +loc l_ident -ng -s":" -ng +core_type_atom +pos

                        ||  mapping begin fun { txt; loc } p2 ->
                                let exp =
                                    Exp.ident
                                        ~loc:(make_location loc.loc_start p2)
                                        ~attrs:[Hc.attr ~loc "ns.namedArgLoc"]
                                        (Location.mkloc (Hc.lid [txt]) loc)
                                in
                                Optional txt, exp
                            end
                            -s"~" -ng +loc l_ident -ng -s"?" +pos

                        ||  mapping begin fun { txt; loc } ->
                                Labelled txt
                                ,
                                Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]
                            end
                            -s"~" -ng +loc l_ident

                        ||  (expression_constrainted <|> special_arg) >>| fun e -> Nolabel, e
                    in

                    let apply =
                        named "expression:apply" begin
                            let flags = ref [] in

                            let special_arg =
                                mapping (fun loc -> Exp.ident ~loc @@ Location.mkloc (Longident.Lident "__x") loc)
                                +loc_of (k"_") -exec (fun _ -> List.hd_exn !flags := true)
                            in

                            let params =
                                    s"(" >> ng >> seq ~n:1 ~sep ~trail (labelled special_arg) << ng << s")"
                                ||  s"(" >> ng >> loc_of (s")") >>| fun loc -> [_unit_arg_ loc]
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
                                s"." >> ng >> seq ~n:1 ~sep (labelled fail)
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
                                            -ng -s"," -ng +apply_params +pos
                                        )
                                end
                            in

                            let unit_arg =
                                    mapping (fun loc -> [Nolabel, res_unit loc])
                                    -s"(" -ng -s"." -ng +loc_of (s"()") -ng -s")"

                                ||  return [_unit_arg]
                                    -s"(" -ng -s"." -ng -s")"
                            in

                                mapping begin fun arg loc_end prev ->
                                    Exp.apply ~loc:{prev.pexp_loc with loc_end} ~attrs:[Hc.attr "bs"] prev arg
                                end
                                -ng +unit_arg +pos
                            ||  ng >> s"(" >> ng >> args << opt sep << ng << s")"
                        end
                    in

                    let field =
                        failed field_set_cont >>
                        named "expression:field" begin
                            mapping begin fun lid loc_end prev ->
                                Exp.field ~loc:{prev.pexp_loc with loc_end} prev lid
                            end
                            -ng -s"." -ng +loc l_longident +pos
                        end
                    in

                    let object_get =
                        failed object_set_cont >>
                        named "expression:send" begin
                            mapping begin fun str loc_end prev ->
                                Exp.send ~loc:{prev.pexp_loc with loc_end} prev str
                            end
                            -ng -s"[" -ng +loc string_raw -ng -s"]" +pos
                        end
                    in

                    let array_get =
                        failed array_set_cont >>
                        named "expression:index" begin
                            mapping begin fun idx loc_end prev ->
                                Exp.apply ~loc:{prev.pexp_loc with loc_end} (Hc.expr_id ["Array"; "get"]) [Nolabel, prev; Nolabel, idx]
                            end
                            -ng -s"[" -ng +expression_constrainted -ng -s"]" +pos
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

            let unops = op_alias "!" "not"

            let unary = fix @@ fun unary ->
                    with_loc & mapping begin fun op expr ->
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
                    +loc (op_alias "-." "~-." <|> op_alias "-" "~-") -ng +unary

                ||  with_loc & mapping begin fun op expr ->
                        match expr.pexp_desc with
                        | Pexp_constant (Pconst_integer (str, suf)) ->
                            (fun loc -> Exp.constant ~loc (Pconst_integer (str, suf)))
                        | Pexp_constant (Pconst_float (str, suf)) ->
                            (fun loc -> Exp.constant ~loc (Pconst_float (str, suf)))
                        | _ ->
                            (fun loc -> Exp.apply ~loc (Exp.ident ~loc:op.loc op) [Nolabel, expr])
                    end
                    +loc (op_alias "+." "~+." <|> op_alias "+" "~+") -ng +unary

                ||  with_loc & mapping begin fun op expr loc ->
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
                left_assoc p0 (op_alias "->" "|.")
            let p2 =
                named "expression:p2" @@
                left_assoc p1 (o"**")
            let p3 =
                named "expression:p3" @@
                left_assoc p2 (o"*." <|> o"*" <|> o"/." <|> o"/")
            let p4 =
                named "expression:p4" @@
                left_assoc2 p3
                (
                        (ng >> loc (op_alias "++" "^" <|> o"+." <|> o"+"))
                    <|> (failed (ng_new_line >> (o"-." <|> o"-") >> p0) >> ng >> loc (o"-." <|> o"-"))
                )
            let p5 =
                named "expression:p5" @@
                left_assoc p4
                (
                    peek_first
                    [ op_alias "===" "=="; op_alias "==" "="; failed (o"=>") >> o"="
                    ; op_alias "!==" "!="; op_alias "!=" "<>"; o"<="; o">="; o"|>"
                    ; failed jsx >> failed (s"<" >> ng >> s"/") >> o"<"; o">"
                    ]
                )
            let p6 =
                named "expression:p6" @@
                left_assoc p5 (o"&&")
            let p7 =
                named "expression:p7" @@
                left_assoc p6 (o"||")
            let p8 =
                memo & named "expression:p8" @@
                left_assoc p7 (o"#=" <|> o":=")

            let ternary =
                named "expression:ternary" begin
                    with_loc & mapping begin fun e1 e2 e3 loc ->
                        Exp.ifthenelse ~loc ~attrs:[Hc.attr "ns.ternary"] e1 e2 (Some e3)
                    end
                    +p8 -ng -s"?" -ng +expression_arrow -ng -s":" -ng +expression_arrow
                end

            let expression =
                memo & named "expression" begin
                        ternary
                    <|> p8
                end

            let expression_arrow = arrow <|> expression
        end: EXPRESSION)

    let expression = let (module M) = x in M.expression
    let expression_arrow = let (module M) = x in M.expression_arrow
    let expression_sequence = let (module M) = x in M.expression_sequence
    let expression_p0 = let (module M) = x in M.expression_p0
end
