
open Basic
open Parsetree
open Asttypes
open Ast_helper
open Basic.Angstrom
open Parser_utils
open Sigs

type arrow_parser_helpers = {
    args_loop : expression helper parser;
    types_loop : expression helper parser;
}

module Make
        (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser)
        (Utils: UTILS) (Constant: CONSTANT)
        (Core: CORE) (Type: TYPE) (Pattern: PATTERN) (Modexpr: MODEXPR)
        : EXPRESSION = struct
    open Utils
    open Core
    open Type
    open Pattern
    open Modexpr

    let rec x () = fix_poly @@ fun getter ->
        (module struct
            let expression = getter.get @@ fun (module M: EXPRESSION) -> M.expression
            let expression_arrow = getter.get @@ fun (module M: EXPRESSION) -> M.expression_arrow
            let expression_sequence = getter.get @@ fun (module M: EXPRESSION) -> M.expression_sequence
            let expression_p0 = getter.get @@ fun (module M: EXPRESSION) -> M.expression_p0

            let expression_constrainted =
                (
                    mapping (mk_helper2 Exp.constraint_)
                    <*> use expression_arrow <* _s":" <*> _use core_type
                )
                <|>
                expression_arrow

            let value_binding_list =
                let value_binding =
                    mapping (mk_helper2 @@ Vb.mk ?docs:None ?text:None)
                    <*> use pattern <* _s"=" <*> _use expression_arrow
                in

                seq 1 (use value_binding) ~sep:(_k_"and")

            let let_ =
                Named.p "expression:let" begin
                    mapping (mk_helper3 Exp.let_)
                    <* k"let" <*> option Nonrecursive (_k"rec" >>$ Recursive)
                    <* ng <*> value_binding_list <* del <*> (_use expression_sequence <|> return @@ Hc.unit_expr Location.none)
                end

            let letmodule =
                Named.p "expression:module" begin
                    mapping (mk_helper3 Exp.letmodule)
                    <* k"module" <*> _loc u_ident <* _s"=" <*> _use modexpr <* del <*> _use expression_sequence
                end

            let pack =
                Named.p "expression:pack" begin
                    (
                        mapping begin fun m t ->
                            let m = Exp.pack m in
                            mk_helper2 Exp.constraint_ m t
                        end
                        <* k"module" <* _s"(" <*> _use modexpr <* _s":" <*> _use core_type_package <* _s")"
                    )
                    <|>
                    (
                        mapping (mk_helper ~f:Exp.pack)
                        <* k"module" <* _s"(" <*> _use modexpr <* _s")"
                    )
                end

            let letopen =
                Named.p "expression:open" begin
                    let override = option Fresh (s"!" >>$ Override) in

                    mapping (mk_helper3 Exp.open_)
                    <* k"open" <*> override <*> _loc u_longident <* del <*> _use expression_sequence
                end

            let letexception =
                Named.p "expression:exception" begin
                    mapping @@ mk_helper2 Exp.letexception
                    <* k"exception" <*> _use type_extension_constructor <* del <*> _use expression_sequence
                end

            let expression_in_braces =
                let_ <|> letmodule <|> letopen <|> letexception
                <|>
                expression_arrow

            let expression_sequence =
                fix @@ fun expression_sequence ->
                    Named.p "expression:sequence" begin
                        (
                            mapping @@ mk_helper2 Exp.sequence
                            <*> use expression_in_braces <* del <*> _use expression_sequence
                        )
                        <|>
                        add_attrs expression_in_braces
                    end


            let scoped_sequence =
                s"{" >> ng >> (expression_sequence << del) << _s"}"

            let _unit_pat_ = Pat.construct (Location.mknoloc @@ Longident.Lident "()") None

            let arrow =
                Named.p "expression:arrow" begin
                    let arrow_tail =
                        let tail = s"=>" >> _use expression_arrow in
                        map2 (s":" >> _use core_type_atom) (ng >> tail)
                        ~f:begin fun typ expr -> Exp.constraint_ ~loc:expr.pexp_loc expr typ end
                        <|>
                        tail
                    in

                    let label = s"~" >> ng >> loc l_ident in

                    let without_pattern =
                        (
                            label >>| fun { txt = s; loc = s_loc } ->
                                helper_add_attr_loc "ns.namedArgLoc" s_loc @@
                                mk_helper () ~f:begin fun ?loc ?attrs () ->
                                    s,
                                    let loc = Base.Option.value ~default:Location.none loc in
                                    Pat.var ~loc ?attrs (Location.mkloc s loc)
                                end
                        )
                    in

                    let with_label =
                        (
                            mapping begin fun { txt = s; loc } p1 pat p2 ->
                                s, apply (helper_add_attr_loc "ns.namedArgLoc" loc pat) p1 p2
                            end
                            <*> label <* _k"as" <* ng <*> pos <*> pattern_constrainted <*> pos
                        )
                        <|>
                        use (
                            mapping begin fun hlp typ ->
                                ahelper_map hlp
                                    ~f:begin fun hlp ->
                                        mk_helper_fn begin fun ?loc ?attrs () ->
                                            let s, pat = hlp ~attrs:[] in
                                            s, Pat.constraint_ ?loc ?attrs pat typ
                                        end ()
                                    end
                            end
                            <*> set_loc without_pattern <* _s":" <*> _use core_type_atom
                        )
                        <|>
                        use without_pattern
                    in

                    let with_value =
                        (
                            with_label << ng << s"=" << ng << s"?"
                            >>|
                            fun (s, pat) -> Optional s, pat, None
                        )
                        <|>
                        (
                            map2
                            with_label
                            (ng >> s"=" >> _use expression_constrainted)
                            ~f:(fun (s, pat) expr -> Optional s, pat, Some expr)
                        )
                        <|>
                        (
                            with_label
                            >>|
                            fun (s, pat) -> Labelled s, pat, None
                        )
                    in

                    let nolabel =
                        (
                            use (mapping (mk_helper2 Pat.constraint_) <*> use @@ add_attrs pattern <* _s":" <*> _use core_type)
                            <|>
                            use @@ add_attrs pattern
                            >>|
                            fun p -> Nolabel, p, None
                        )
                    in

                    let constr_unit =
                        let unit = s"()" >>$ Longident.Lident "()" in
                        let pat = loc unit >>| fun lid -> mk_helper2 Pat.construct lid None in

                        map2 (use pat) (ng >> arrow_tail)
                        ~f:(mk_helper4 Exp.fun_ Nolabel None)
                    in

                    let with_many_args =
                        let loop = fix_poly @@ fun getter ->
                            let args_loop = getter.get @@ fun x -> x.args_loop in
                            let types_loop = getter.get @@ fun x -> x.types_loop in

                            let tail =
                                (s"," >> _s")" >> ng >> arrow_tail)
                                <|>
                                (s"," >> _use @@ add_attrs (k"type" >> ng >> types_loop))
                                <|>
                                (s"," >> _use args_loop)
                                <|>
                                (s")" >> ng >> arrow_tail)
                            in

                            let curried =
                                add_attrs @@ map2 with_value (ng >> tail)
                                ~f:(fun (label, pat, expr) tail ->
                                    mk_helper4 Exp.fun_ label expr pat tail
                                )
                                <|>
                                map2 nolabel (ng >> tail)
                                ~f:(fun (label, pat, expr) tail ->
                                    mk_helper4 Exp.fun_ label expr pat tail
                                )
                            in

                            {
                                args_loop =
                                    (s_"." >> curried >>| helper_add_attr "bs")
                                    <|>
                                    curried
                                ;
                                types_loop = begin
                                    (
                                        mapping @@ mk_helper2 Exp.newtype
                                        <*> loc l_ident <*> _use types_loop
                                    )
                                    <|>
                                    (s"," >> ng >> add_attrs (k"type" >> ng >> types_loop))
                                    <|>
                                    (s"," >> ng >> args_loop)
                                end;
                            }
                        in

                        add_attrs (s_"(" >> add_attrs (k"type" >> ng >> loop.types_loop))
                        <|>
                        add_attrs (s_"(" >> loop.args_loop)
                    in

                    let constr_unit_uncurried =
                        let unit = s"(" >> _s"." >> _s")" >>$ Longident.Lident "()" in
                        let pat = loc unit >>| fun lid -> mk_helper2 Pat.construct lid None in

                        map2 (use pat) (ng >> arrow_tail)
                        ~f:(fun pat expr ->
                            helper_add_attr "bs"
                            (mk_helper4 Exp.fun_ Nolabel None pat expr)
                        )
                    in

                    let only_arg =
                        add_attrs (
                            mapping (mk_helper4 Exp.fun_ Nolabel None)
                            <*> use pattern_atom <* _s"=>" <*> _use expression_arrow
                        )
                    in

                    constr_unit <|> with_many_args <|> constr_unit_uncurried <|> only_arg
                end

            let tuple =
                Named.p "expression:tuple" begin
                    mapping (mk_helper ~f:Exp.tuple)
                    <* s"(" <*> seq 2 (_use expression_constrainted) ~sep:(_s",") ~trail <* _s")"
                end

            let list_helper =
                make_list_helper ~constr:Exp.construct ~tuple:Exp.tuple ~get_loc:(fun x -> x.pexp_loc)

            let list =
                Named.p "expression:list" begin
                        begin
                            mapping (list_helper [] None)
                            <* s"list{" <* _s"}"
                        end
                    <|> (s"list{" >> _s_"..." >> expression_constrainted << opt @@ _s"," << _s"}")
                    <|> begin
                            mapping list_helper
                            <* s"list{" <*> seq 1 ~sep:(_s",") (_use expression_constrainted)
                            <*> (
                                    (_s"," >> _s"..." >> _use expression_constrainted << opt @@ _s"," >>| Base.Option.some)
                                    <|>
                                    (opt @@ _s"," >>$ None)
                                )
                            <* _s"}"
                        end
                end

            let case =
                map3 ~f:(fun pat guard exp -> Exp.case pat ?guard exp)
                (use pattern) (opt ((_k"when" >> _use expression) <|> (_k"if" >> _use expression))) (_s"=>" >> _use expression_sequence)

            let case_list =
                s"{" >> opt (_s_"|") >> seq 1 case ~sep:(_s_"|") << _s"}"

            let switch =
                Named.p "expression:switch" begin
                    mapping @@ mk_helper2 Exp.match_
                    <* k"switch" <*> _use expression <* ng <*> case_list
                end

            let try_catch =
                Named.p "expression:try" begin
                    mapping @@ mk_helper2 Exp.try_
                    <* k"try" <*> _use expression <* _k"catch" <* ng <*> case_list
                end

            let ifthenelse =
                fix @@ fun ifthenelse ->
                Named.p "expression:if" begin
                    mapping (mk_helper3 Exp.ifthenelse)
                    <* k"if" <*> _use expression <*> _use scoped_sequence
                    <*>? (_k"else" >> _use (scoped_sequence <|> ifthenelse))
                end

            let for_ =
                Named.p "expression:for" begin
                    let direction =
                            (k"to" >>$ Upto)
                        <|> (k"downto" >>$ Downto)
                    in

                    (
                        mapping begin fun pat ef dir et ea -> mk_helper5 Exp.for_ pat ef et dir ea end
                        <* k"for" <*> _use pattern <* _k"in" <*> _use expression <* ng <*> direction
                        <*> _use expression <*> _use scoped_sequence
                    )
                    <|>
                    (
                        mapping begin fun pat ef dir et ea -> mk_helper5 Exp.for_ pat ef et dir ea end
                        <* k"for" <* _s"(" <*> _use pattern <* _k"in" <*> _use expression
                        <* ng <*> direction <*> _use expression <* _s")" <*> _use scoped_sequence
                    )
                end

            let while_ =
                Named.p "expression:while" begin
                    mapping @@ mk_helper2 Exp.while_
                    <* k"while" <*> _use expression <*> _use scoped_sequence
                end

            let assert_ =
                Named.p "expression:assert" begin
                    mapping (mk_helper ~f:Exp.assert_)
                    <* k"assert" <*> _use expression
                end

            let lazy_ =
                Named.p "expression:lazy" begin
                    mapping (mk_helper ~f:Exp.lazy_)
                    <* k"lazy" <*> _use expression
                end

            let _unit_arg_ loc = Nolabel, Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
            let _unit_arg = _unit_arg_ Location.none

            let ident = mapping @@ mk_helper ~f:Exp.ident <*> loc l_longident

            let jsx =
                Named.p "expression:jsx" begin
                    let arg =
                        map2
                        (loc (l_ident >>| fun x -> Labelled x)) (_s"=" >> _set_loc expression_p0)
                        ~f:begin fun { txt = label; loc } expr ->
                            label, apply_ah @@ helper_add_attr_loc "ns.namedArgLoc" loc expr
                        end
                        <|>
                        map2
                        (loc (l_ident >>| fun x -> Optional x)) (_s"=" >> _s"?" >> _set_loc expression_p0)
                        ~f:begin fun { txt = label; loc } expr ->
                            label, apply_ah @@ helper_add_attr_loc "ns.namedArgLoc" loc expr
                        end
                        <|>
                        map
                        (loc (l_ident >>| fun x -> x, Labelled x))
                        ~f:begin fun { txt = s, label; loc } ->
                            let expr = Exp.ident ~loc ~attrs:[Attrs.ns_namedArgLoc loc] @@ Location.mkloc (Longident.Lident s) loc in
                            label, expr
                        end
                        <|>
                        map
                        (s_"?" >> loc (l_ident >>| fun x -> x, Optional x))
                        ~f:begin fun { txt = s, label; loc } ->
                            let expr = Exp.ident ~loc ~attrs:[Attrs.ns_namedArgLoc loc] @@ Location.mkloc (Longident.Lident s) loc in
                            label, expr
                        end
                    in

                    let children_list =
                        map (many (_use expression))
                        ~f:begin fun list -> list_helper list None end
                    in

                    let children =
                        (s"..." >> expression)
                        <|>
                        children_list
                    in

                        (
                            mapping begin fun tag args -> mk_helper2 Exp.apply (Exp.ident tag) (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg
                                ])
                            end
                            <* s"<" <*> _loc (l_ident >>| fun x -> Longident.Lident x) <*>* (ng >> arg) <* _s"/" <* _s">"
                        )
                    <|> (
                            mapping t3
                            <* s"<" <*> _loc (l_ident >>| fun x -> Longident.Lident x) <*>* (ng >> arg) <* _s">"
                            <*> _use children <* ng
                            >>= fun (tag, args, children) ->
                                s"<" >> ng >> s"/" >> ng >> exact_longident tag.txt << ng << s">"
                                >>$
                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in
                                mk_helper2 Exp.apply (Exp.ident tag) (args @ tailargs)
                        )
                    <|> (
                            map2
                            (s"<" >> _loc (l_longident <|> u_longident)) (many (ng >> arg) << _s"/" << _s">")
                            ~f:(fun tag args ->
                                let tag =
                                    Exp.ident ~loc:tag.loc @@ Location.mkloc (Longident.Ldot (tag.txt, "createElement")) tag.loc
                                in

                                mk_helper2 Exp.apply tag (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg]))
                        )
                    <|> (
                            mapping t3
                            <* s"<" <*> _loc (l_longident <|> u_longident) <*>* (ng >> arg) <* _s">"
                            <*> _use children <* ng
                            >>= fun ({ txt = tag; loc = tag_loc }, args, children) ->
                                s"<" >> _s"/" >> ng >> exact_longident tag << _s">"
                                >>$
                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in

                                let tag =
                                    Exp.ident ~loc:tag_loc @@ Location.mkloc (Longident.Ldot (tag, "createElement")) tag_loc
                                in

                                mk_helper2 Exp.apply tag (args @ tailargs)
                        )
                    <|> (
                            map
                            (s"<" >> _s">" >> _s"..." >> _use expression << _s"<" << _s"/" << _s">")
                            ~f:begin fun expr -> list_helper [expr] None end
                        )
                    <|> (s"<" >> _s">" >> children_list << _s"<" << _s"/" << _s">")
                end

            let jsx = jsx >>| helper_add_attr "JSX"
            let jsx = jsx

            let array =
                Named.p "expression:array" begin
                        (s"[" >> _s"]" >>$ mk_helper ~f:Exp.array [])
                    <|> (
                            mapping (mk_helper ~f:Exp.array)
                            <* s_"[" <*> seq 1 ~sep:(_s",") ~trail (_use expression_constrainted) <* _s"]"
                        )
                end

            let record =
                Named.p "expression:record" begin
                    let _nb =
                            (
                                mapping t2
                                <*> _loc l_longident <* _s":" <*> _use expression_arrow
                            )
                        <|> (_loc l_longident >>| fun lid -> lid, Exp.ident ~loc:lid.loc lid)
                    in

                    mapping begin fun expr list -> mk_helper2 Exp.record list expr end
                    <* s"{" <*>? (_s"..." *> _use expression_constrainted <* _s",")
                    <*> seq 1 _nb ~sep:(_s",") ~trail <* _s"}"
                end

            let bs_object =
                let record =
                    map
                    (s_"{" >> seq 1 ~sep:(ng << s",") ~trail
                        (map2 (ng >> loc (Constant.String.string >>| fun x -> Longident.Lident x)) (opt (_s":" >> _use expression))
                            ~f:begin fun lid pat ->
                            match pat with
                            | Some pat -> lid, pat
                            | None -> lid, Exp.ident ~loc:lid.loc lid
                            end
                        )
                        << _s"}"
                    )
                    ~f:begin fun list ->
                        mk_helper2 Exp.record list None
                    end
                in
                use record >>| fun x -> mk_helper ~f:Exp.extension (Location.mkloc "obj" x.pexp_loc, PStr [Str.eval x])

            let extension = mapping @@ mk_helper ~f:Exp.extension <*> extension

            let coerce =
                mapping (fun a b -> mk_helper3 Exp.coerce a None b)
                <* s"(" <*> _use expression_constrainted <* _s":>" <*> _use core_type_atom <* _s")"

            let interpolated_string qtag =
                let op = Exp.ident @@ Location.mknoloc @@ Longident.Lident "^" in

                let mk_const buf =
                    helper_add_attr "res.template" @@
                    mk_helper ~f:Exp.constant @@
                    Const.string ~quotation_delimiter:qtag @@ Buffer.contents buf
                in


                let variants buf loop =
                        ((new_line >>| Buffer.add_string buf) >> loop)
                    <|> ((s"\\`" >>| fun _ -> Buffer.add_char buf '`') >> loop)
                    <|> ((s"\\$" >>| fun _ -> Buffer.add_char buf '$') >> loop)
                    <|> ((s"\\\\" >>| fun _ -> Buffer.add_char buf '\\') >> loop)
                    <|> ((s"$" >>| fun _ -> Buffer.add_char buf '$') >> loop)
                in

                let string_part =
                    fix @@ fun (string_part: (expression -> expression helper) parser) ->

                    let tail =
                        map3 (s"${" >> _use expression << _s"}") position string_part
                        ~f:begin fun expr pos tail prev ->
                            tail @@ apply
                                (mk_helper2 Exp.apply op [Nolabel, prev; Nolabel, expr])
                                prev.pexp_loc.loc_start pos
                        end
                    in

                    return () >>= fun _ ->
                    let buf = Buffer.create 16 in
                    (
                        position >>= fun pstart ->
                        fix @@ fun loop ->
                            (take_while (function '\n' | '$' | '`' | '\\' | '\r' -> false | _ -> true) >>| Buffer.add_string buf)
                            >>
                            (
                                    (
                                        s"`"
                                        >>$ fun prev ->
                                            helper_add_attr "res.template" @@
                                            helper @@ fun ~p1 ~p2 ~attrs ->
                                                apply
                                                    (
                                                        helper_add_attrs attrs @@
                                                        mk_helper2 Exp.apply op [
                                                            Nolabel, prev;
                                                            Nolabel, apply (mk_const buf) p1 p2
                                                        ]
                                                    )
                                                    p1 p2
                                    )
                                <|> map2 position tail ~f:begin fun pos tail prev ->
                                        let e = apply (mk_const buf) pstart pos in
                                        tail (
                                            apply (
                                                helper_add_attr "res.template" @@
                                                mk_helper2 Exp.apply op [Nolabel, prev; Nolabel, e]
                                            ) prev.pexp_loc.loc_start pos
                                        )
                                    end
                                <|> variants buf loop
                            )
                    )
                in

                let tail =
                    map3 (s"${" >> _use expression << _s"}") position string_part
                    ~f:begin fun expr pos tail prev ->
                        tail @@ apply
                            (
                                helper_add_attr "res.template" @@
                                mk_helper2 Exp.apply op [Nolabel, prev; Nolabel, expr]
                            )
                            prev.pexp_loc.loc_start pos
                    end
                in

                s"`" >>= fun _ ->
                let buf = Buffer.create 16 in
                fix @@ fun loop ->
                (take_while (function '\n' | '$' | '`' | '\\' | '\r' -> false | _ -> true) >>| Buffer.add_string buf)
                >>
                (
                        (s"`" >>| fun _ -> mk_const buf)
                    <|> (both position tail >>| fun (pos, tail) ->
                            begin fun ~p1 ~p2 ~attrs ->
                                let e0 =
                                    apply (mk_const buf) p1 pos
                                in
                                apply (helper_add_attrs attrs @@ tail e0) p1 p2
                            end,
                            []
                        )
                    <|> variants buf loop
                )

            let no_ip_string =
                s"\"" >>= fun _ ->
                let buf = Buffer.create 16 in
                let mk_const () =
                    Const.string ~quotation_delimiter:"js" @@ Buffer.contents buf
                in
                fix @@ fun loop ->
                (take_while (function '\n' | '\\' | '\"' | '\r' -> false | _ -> true) >>| Buffer.add_string buf)
                >>
                (
                    (s"\"" >>| fun _ -> mk_helper ~f:Exp.constant (mk_const ()))
                    <|>
                    ((new_line >>| Buffer.add_string buf) >> loop)
                    <|>
                    ((s"\\\"" >>| fun _ -> Buffer.add_string buf "\\\"") >> loop)
                    <|>
                    ((s"\\\\" >>| fun _ -> Buffer.add_string buf "\\\\") >> loop)
                    <|>
                    ((s"\\" >>| fun _ -> Buffer.add_char buf '\\') >> loop)
                )

            let js_string = Named.p "expression:js_string" @@ interpolated_string "js"
            let json_string =
                Named.p "expression:json_string" @@
                (s"json" >> interpolated_string "json")

            let constr_args =
                    (loc_of (s"(" >> _s")") >>| Hc.unit_expr)
                <|> use tuple
                <|> (s"(" >> _use expression_constrainted << opt @@ _s"," << _s")")


            let polyvariant =
                let tag =
                        u_ident
                    <|> Constant.String.string
                    <|> take_while (function '0'..'9' -> true | _ -> false)
                in
                mapping @@ mk_helper2 Exp.variant
                <* s"#" <*> tag <*>? constr_args

            let true_false =
                mapping (fun a -> mk_helper2 Exp.construct a None)
                <*> loc ((k"true" >>$ Longident.Lident "true") <|> (k"false" >>$ Longident.Lident "false"))

            let string_ident =
                mapping @@ mk_helper ~f:Exp.ident
                <* s"\\" <*> loc (Constant.String.string >>| fun x -> Longident.Lident x)

            let atom =
                Named.p "expression:atom" begin
                        switch <|> assert_ <|> lazy_ <|> try_catch <|> for_ <|> while_ <|> jsx
                    <|> list
                    <|> string_ident
                    <|> no_ip_string
                    <|> (mapping @@ mk_helper ~f:Exp.constant <*> Constant.p)
                    <|> js_string <|> json_string
                    <|> pack
                    <|> true_false
                    <|> ident
                    <|> (mapping @@ mk_helper2 Exp.construct <*> loc u_longident <*>? constr_args)
                    <|> polyvariant
                    <|> (mapping (fun a -> mk_helper2 Exp.construct a None) <*> loc (s"()" >>$ Longident.Lident "()"))
                    <|> tuple
                    <|> coerce
                    <|> (s_"(" >> add_attrs expression_constrainted << _s")")
                    <|> array
                    <|> (scoped_sequence >>| helper_add_attr "ns.braces")
                    <|> record <|> bs_object
                    <|> extension
                end

            let res_unit loc =
                let pattern = Pat.var @@ Location.mknoloc "__res_unit" in
                let expr = Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None in
                let expr2 = Exp.ident @@ Location.mknoloc @@ Longident.Lident "__res_unit" in
                let vb = Vb.mk ?docs:None ?text:None pattern expr in
                Exp.let_ Nonrecursive [vb] expr2
(*
            let with_continuation tail x (helper: ?loc:_ -> ?attrs:_ -> _) =
                map3 x position (opt tail)
                ~f:begin fun x pend tail ->
                    fun expr ->
                        match tail with
                        | Some tail ->
                            tail @@ helper expr x ~loc:(make_location expr.pexp_loc.loc_start pend)
                        | None -> mk_helper2 helper expr x
                end
*)
            let field_set_cont =
                Named.p "expression:field:set" begin
                    map2 (_s"." >> _loc l_longident) (_s "=" >> _use expression_arrow)
                    ~f:begin fun lid expr prev ->
                        mk_helper3 Exp.setfield prev lid expr
                    end
                end

            let object_set_cont =
                Named.p "expression:object:set" begin
                    mapping begin fun idx p1 loc value expr ->
                        let s = Exp.send ~loc:(make_location expr.pexp_loc.loc_start p1) expr idx in
                        let lid = Exp.ident ~loc @@ Location.mkloc (Longident.Lident "#=") loc in
                        mk_helper2 Exp.apply lid [Nolabel, s; Nolabel, value]
                    end
                    <* _s"[" <*> _loc Constant.String.string <* _s"]" <*> position <*> _loc_of @@ s"=" <*> _use expression_arrow
                end

            let array_set_cont =
                Named.p "expression:array:set" begin
                    mapping begin fun idx value expr ->
                        mk_helper2 Exp.apply (Hc.id "Array" "set") [Nolabel, expr; Nolabel, idx; Nolabel, value]
                    end
                    <* _s"[" <*> _use expression <* _s"]" <* _s"=" <*> _use expression_arrow
                end

            let tail =
                Named.p "expression:tail" begin
                    let labelled _arg =
                            (
                                mapping begin fun tag expr ->
                                    Optional tag.txt,
                                    apply_ah @@ helper_add_attr_loc "ns.namedArgLoc" tag.loc expr
                                end
                                <* s"~" <*> _loc l_ident <* _s"=" <* _s"?" <*> _set_loc expression_constrainted
                            )
                        <|> (
                                mapping begin fun tag expr -> Optional tag, expr end
                                <* s_"~" <*> l_ident <* _s"=" <* _s"?" <*> _use _arg
                            )
                        <|> (
                                mapping begin fun tag expr ->
                                    Labelled tag.txt,
                                    apply_ah @@ helper_add_attr_loc "ns.namedArgLoc" tag.loc expr
                                end
                                <* s"~" <*> _loc l_ident <* _s"=" <*> _set_loc expression_constrainted
                            )
                        <|> (
                                mapping begin fun tag expr -> Labelled tag, expr end
                                <* s_"~" <*> l_ident <* _s"=" <*> _use _arg
                            )
                        <|> (
                                mapping begin fun { txt = (label, lident); loc } typ p2 ->
                                    let exp =
                                        apply
                                        (
                                            helper_add_attr_loc "ns.namedArgLoc" loc @@
                                            mk_helper2 Exp.constraint_ (Exp.ident ~loc:loc ~attrs:[Attrs.ns_namedArgLoc loc] (Location.mkloc lident loc)) typ
                                        )
                                        loc.loc_start p2
                                    in
                                    Labelled label, exp
                                end
                                <* s"~" <*> _loc (l_ident >>| fun x -> x, Longident.Lident x)
                                <* _s":" <*> _use core_type_atom <*> pos
                            )
                        <|> (
                                mapping begin fun { txt = (label, lident); loc } p2 ->
                                    let exp =
                                        apply
                                        (
                                            helper_add_attr_loc "ns.namedArgLoc" loc @@
                                            mk_helper ~f:Exp.ident (Location.mkloc lident loc)
                                        )
                                        loc.loc_start p2
                                    in
                                    Optional label, exp
                                end
                                <* s"~" <*> _loc (l_ident >>| fun x -> x, Longident.Lident x) <* _s"?" <*> pos
                            )
                        <|> (
                                mapping begin fun tag p2 ->
                                    let exp =
                                        apply
                                        (
                                            helper_add_attr_loc "ns.namedArgLoc" tag.loc @@
                                            mk_helper ~f:Exp.ident (Location.mkloc (Longident.Lident tag.txt) tag.loc)
                                        )
                                        tag.loc.loc_start p2
                                    in
                                    Labelled tag.txt, exp
                                end
                                <* s"~" <*> _loc l_ident <*> pos
                            )
                        <|> (use (expression_constrainted <|> _arg) >>| fun e -> Nolabel, e)
                    in

                    let apply =
                        Named.p "expression:apply" begin
                            return () >>= fun _ ->
                            let _arg_f = ref false in
                            let _arg =
                                mapping @@ mk_helper ~f:Exp.ident
                                <*> loc (k"_" >>| fun _ -> _arg_f := true; Longident.Lident "__x")
                            in

                            let params =
                                    (s_"(" >> (seq 1 ~sep:(ng << s",") ~trail (ng >> labelled _arg)) << _s")")
                                <|> (s_"(" >> loc_of (s")") >>| fun loc -> [_unit_arg_ loc])
                            in

                            ng_no_new_line >> params >>| fun list expr ->
                                mk_helper () ~f:begin fun ?loc ?attrs () ->
                                    if !_arg_f then
                                        let x = Exp.apply ?loc ?attrs expr list in
                                        Exp.fun_ ?loc ?attrs Nolabel None (Pat.var @@ Location.mknoloc "__x") x
                                    else
                                        Exp.apply ?loc ?attrs expr list
                                end
                        end
                    in

                    let apply_uncurried =
                        Named.p "expression:apply:uncurried" begin
                            let helper expr args =
                                helper_add_attr "bs" @@
                                mk_helper2 Exp.apply expr args
                            in

                            let apply_params =
                                s"." >> seq 1 ~sep:(_s",") (ng >> labelled (fail ""))
                            in

                            let args =
                                Named.p "expression:apply:uncurried:tail" begin
                                    fold_left_cont_0_n
                                        (
                                            mapping begin fun args p2 -> (fun expr -> helper expr args), p2 end
                                            <*> apply_params <*> pos
                                        )
                                        (
                                            mapping begin fun args p3 (cont, p2) ->
                                                begin fun expr ->
                                                    let expr = Basic.apply (cont expr) expr.pexp_loc.loc_start p2 in
                                                    helper expr args
                                                end, p3
                                            end
                                            <* _s"," <* ng <*> apply_params <*> pos
                                        )
                                end
                                >>| fun (hlp, _) -> hlp
                            in

                            let unit_arg =
                                    (
                                        s"(" >> _s"." >> _loc_of @@ s"()" << _s")"
                                        >>| fun loc -> [Nolabel, res_unit loc]
                                    )
                                <|> (s"(" >> _s"." >> _s")" >>$ [_unit_arg])
                            in

                                (ng >> unit_arg >>| fun arg expr -> helper expr arg)
                            <|> (_s_"(" >> args << opt (_s",") << _s")")
                        end
                    in

                    let field =
                        failed field_set_cont >>
                        Named.p "expression:field" begin
                            _s"." >> _loc l_longident >>| fun lid expr ->
                                mk_helper2 Exp.field expr lid
                        end
                    in

                    let object_get =
                        failed object_set_cont >>
                        Named.p "expression:send" begin
                            _s"[" >> _loc Constant.String.string << _s"]"
                            >>| fun str expr -> mk_helper2 Exp.send expr str
                        end
                    in

                    let array_get =
                        failed array_set_cont >>
                        Named.p "expression:index" begin
                            _s"[" >> _use expression_constrainted << _s"]"
                            >>| fun idx expr ->
                                mk_helper2 Exp.apply (Hc.id "Array" "get") [Nolabel, expr; Nolabel, idx]
                        end
                    in

                    apply <|> apply_uncurried <|> field <|> object_get <|> array_get
                end

            let primary =
                Named.p "expression:primary" begin
                    fold_left_cont_0_n
                        (
                            mapping t3
                            <*> pos <*> atom <*> pos
                        )
                        (
                            mapping begin fun cont p3 (p1, expr, p2) ->
                                p1, cont @@ apply expr p1 p2, p3
                            end
                            <*> tail <*> pos
                        )
                end
                >>| fun (_, x, _) -> x

            let set =
                Named.p "expression:set" begin
                    let set_cont =
                        Named.p "expression:set:cont" begin
                            field_set_cont <|> object_set_cont <|> array_set_cont
                        end
                    in

                    fold_left_cont_0_1
                        (
                            mapping t3
                            <*> pos <*> primary <*> pos
                        )
                        (
                            mapping begin fun cont p3 (p1, expr, p2) ->
                                p1, cont @@ apply expr p1 p2, p3
                            end
                            <*> set_cont <*> pos
                        )
                end
                >>| fun (_, x, _) -> x

            let unops = op_alias "!" "not"

            let unary = fix @@ fun unary ->
                    (
                        mapping map_unary_minus
                        <*> loc (op_alias "-." "~-." <|> op_alias "-" "~-") <*> _use unary
                    )
                <|> (
                        mapping map_unary_plus
                        <*> loc (op_alias "+." "~+." <|> op_alias "+" "~+") <*> _use unary
                    )
                <|> (
                        mapping map_unary_op
                        <*> loc @@ unops <*> _use unary
                    )
                <|> set

            let p0 =
                Named.p "expression:p0" @@
                add_attrs (ifthenelse <|> unary)

            let expression_p0 = p0

            let left_assoc mp ops =
                fold_hlp_left_0_n ~f:map_binary_op
                mp (both (_loc ops) (_use mp))

            let left_assoc2 mp ops =
                fold_hlp_left_0_n ~f:map_binary_op
                mp (both ops (_use mp))

            let p1 =
                Named.p "expression:p1" @@
                left_assoc p0 (op_alias "->" "|.")
            let p2 =
                Named.p "expression:p2" @@
                left_assoc p1 (o"**")
            let p3 =
                Named.p "expression:p3" @@
                left_assoc p2 (o"*." <|> o"*" <|> o"/." <|> o"/")
            let p4 =
                Named.p "expression:p4" @@
                left_assoc2 p3
                (
                        (ng >> loc (op_alias "++" "^" <|> o"+." <|> o"+"))
                    <|> (failed (ng_new_line >> (o"-." <|> o"-") >> p0) >> ng >> loc (o"-." <|> o"-"))
                )
            let p5 =
                Named.p "expression:p5" @@
                left_assoc p4
                (
                    op_alias "===" "==" <|> op_alias "==" "=" <|> (failed (o"=>") >> o"=")
                    <|> op_alias "!==" "!=" <|> op_alias "!=" "<>" <|> o"<=" <|> o">=" <|> o"|>"
                    <|> (failed jsx >> failed (s"<" >> _s"/") >> o"<") <|> o">"
                )
            let p6 =
                Named.p "expression:p6" @@
                left_assoc p5 (o"&&")
            let p7 =
                Named.p "expression:p7" @@
                left_assoc p6 (o"||")
            let p8 =
                Named.p "expression:p8" @@
                left_assoc p7 (o"#=" <|> o":=")

            let _right_assoc mp ops =
                fix @@ fun tail ->

                map3 (use mp) (_loc ops) (_use tail)
                ~f:begin fun e1 op e2 ->
                    mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, e1; Nolabel, e2]
                end
                <|>
                mp

            let ternary =
                Named.p "expression:ternary" begin
                    (
                        mapping (mk_helper3 Exp.ifthenelse)
                        <*> use p8 <* _s"?" <*> _use expression_arrow <* _s":" <*> (_use expression_arrow >>| some)
                    )
                    >>| (helper_add_attr "ns.ternary")
                end

            let expression =
                Named.p "expression" begin
                        ternary
                    <|> p8
                end

            let expression_arrow = arrow <|> expression
        end: EXPRESSION)
    and map_unary_minus op expr =
        match expr.pexp_desc with
        | Pexp_constant (Pconst_integer (str, suf) as c) ->
            if str.[0] = '-' then
                mk_helper ~f:Exp.constant c
            else
                mk_helper ~f:Exp.constant (Pconst_integer ("-" ^ str, suf))
        | Pexp_constant (Pconst_float (str, suf) as c) ->
            if str.[0] = '-' then
                mk_helper ~f:Exp.constant c
            else
                mk_helper ~f:Exp.constant (Pconst_float ("-" ^ str, suf))
        | _ -> mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
    and map_unary_plus op expr =
        match expr.pexp_desc with
        | Pexp_constant (Pconst_integer (str, suf)) ->
            mk_helper ~f:Exp.constant (Pconst_integer (str, suf))
        | Pexp_constant (Pconst_float (str, suf)) ->
            mk_helper ~f:Exp.constant (Pconst_float (str, suf))
        | _ -> mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
    and map_unary_op op expr =
        mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
    and map_binary_op left (op, right) =
        mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, left; Nolabel, right]

    let x = x ()
    let expression = let (module M) = x in M.expression
    let expression_arrow = let (module M) = x in M.expression_arrow
    let expression_sequence = let (module M) = x in M.expression_sequence
    let expression_p0 = let (module M) = x in M.expression_p0
end
