
open Base
open Basic
open Parsetree
open Asttypes
open Ast_helper
open APos
open Sigs

type arrow_parser_helpers = {
    args_loop : expression helper parser;
    types_loop : expression helper parser;
}

module Mapping = struct
    let unary_minus  =
        mapping begin fun op expr ->
            match expr.pexp_desc with
            | Pexp_constant (Pconst_integer (str, suf) as c) ->
                if Char.equal str.[0] '-' then
                    mk_helper ~f:Exp.constant c
                else
                    mk_helper ~f:Exp.constant (Pconst_integer ("-" ^ str, suf))
            | Pexp_constant (Pconst_float (str, suf) as c) ->
                if Char.equal str.[0] '-' then
                    mk_helper ~f:Exp.constant c
                else
                    mk_helper ~f:Exp.constant (Pconst_float ("-" ^ str, suf))
            | _ -> mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
        end

    let unary_plus =
        mapping begin fun op expr ->
            match expr.pexp_desc with
            | Pexp_constant (Pconst_integer (str, suf)) ->
                mk_helper ~f:Exp.constant (Pconst_integer (str, suf))
            | Pexp_constant (Pconst_float (str, suf)) ->
                mk_helper ~f:Exp.constant (Pconst_float (str, suf))
            | _ -> mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
        end

    let unary_op =
        mapping begin fun op expr ->
            mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, expr]
        end

    let binary_op =
        fun left (op, right) ->
            mk_helper2 Exp.apply (Exp.ident ~loc:op.loc op) [Nolabel, left; Nolabel, right]
end

module Make
        (Ext: EXT)
        (Utils: UTILS) (Constant: CONSTANT)
        (Core: CORE) (Type: TYPE) (Pattern: PATTERN) (Modexpr: MODEXPR)
        : EXPRESSION = struct
    open Ext
    open Utils
    open Core
    open Type
    open Pattern
    open Modexpr

    let x = fix_poly @@ fun getter ->
        (module struct
            let expression = getter.get @@ fun (module M: EXPRESSION) -> M.expression
            let expression_arrow = getter.get @@ fun (module M: EXPRESSION) -> M.expression_arrow
            let expression_sequence = getter.get @@ fun (module M: EXPRESSION) -> M.expression_sequence
            let expression_p0 = getter.get @@ fun (module M: EXPRESSION) -> M.expression_p0

            let expression_constrainted =
                Named.p "expression:constrainted" begin
                        (
                            mapping (mk_helper2 Exp.constraint_)
                            <*> use expression_arrow << -s":" <*> -use core_type
                        )
                    <|> expression_arrow
                end

            let value_binding_list =
                let value_binding =
                    mapping (mk_helper2 @@ Vb.mk ?docs:None ?text:None)
                    <*> use pattern << -s"=" <*> -use expression_arrow
                in

                seq 1 (use value_binding) ~sep:(-k"and" >> ng)

            let let_ =
                let rec_flag = k"rec" >>$ Recursive <|> return Nonrecursive in

                Named.p "expression:let" begin
                    mapping (mk_helper3 Exp.let_)
                    << k"let" <*> -rec_flag
                    <*> -value_binding_list << del
                    <*> (-use expression_sequence <|> return @@ Hc.unit_expr Location.none)
                end

            let letmodule =
                Named.p "expression:module" begin
                    mapping (mk_helper3 Exp.letmodule)
                    << k"module" <*> -loc u_ident << -s"=" <*> -use modexpr << del <*> -use expression_sequence
                end

            let pack =
                Named.p "expression:pack" begin
                        (
                            mapping (fun m t -> mk_helper2 Exp.constraint_ (Exp.pack m) t)
                            << k"module" << -s"(" <*> -use modexpr << -s":" <*> -use core_type_package << -s")"
                        )
                    <|> (
                            mapping (mk_helper ~f:Exp.pack)
                            << k"module" << -s"(" <*> -use modexpr << -s")"
                        )
                end

            let letopen =
                Named.p "expression:open" begin
                    let override = (s"!" >>$ Override) <|> return Fresh in

                    mapping (mk_helper3 Exp.open_)
                    << k"open" <*> override <*> -loc u_longident << del <*> -use expression_sequence
                end

            let letexception =
                Named.p "expression:exception" begin
                    mapping (mk_helper2 Exp.letexception)
                    << k"exception" <*> -use type_extension_constructor << del <*> -use expression_sequence
                end

            let expression_in_braces =
                Named.p "expression:in_braces" begin
                        Peek.first
                        [ let_
                        ; letmodule
                        ; letopen
                        ; letexception
                        ]
                    <|> expression_arrow
                end

            let expression_sequence =
                fix @@ fun expression_sequence ->
                    Named.p "expression:sequence" begin
                            (
                                mapping (mk_helper2 Exp.sequence)
                                <*> use expression_in_braces << del <*> -use expression_sequence
                            )
                        <|> add_attrs expression_in_braces
                    end


            let scoped_sequence = s"{" >> -expression_sequence << del << -s"}"

            let arrow =
                Named.p "expression:arrow" begin
                    let arrow_tail =
                        let tail = s"=>" >> -use expression_arrow in
                            (
                                mapping begin fun typ expr -> Exp.constraint_ ~loc:expr.pexp_loc expr typ end
                                << s":" <*> -use core_type_atom <*> -tail
                            )
                        <|> tail
                    in

                    let label = s"~" >> -loc l_ident in

                    let without_pattern =
                        (
                            label >>| fun { txt = s; loc } ->
                                helper_add_attr "ns.namedArgLoc" ~loc @@
                                mk_helper () ~f:begin fun ?loc ?attrs () ->
                                    s,
                                    let loc = Option.value ~default:Location.none loc in
                                    Pat.var ~loc ?attrs (Location.mkloc s loc)
                                end
                        )
                    in

                    let with_label =
                            (
                                mapping begin fun { txt = s; loc } p1 pat p2 ->
                                    s, apply (helper_add_attr "ns.namedArgLoc" ~loc pat) p1 p2
                                end
                                <*> label << -k"as" <*> -pos <*> pattern_constrainted <*> pos
                            )
                        <|> use (
                                mapping begin fun hlp typ ->
                                    ahelper_map hlp
                                        ~f:begin fun hlp ->
                                            mk_helper_fn begin fun ?loc ?attrs () ->
                                                let s, pat = hlp ~attrs:[] in
                                                s, Pat.constraint_ ?loc ?attrs pat typ
                                            end ()
                                        end
                                end
                                <*> set_loc without_pattern << -s":" <*> -use core_type_atom
                            )
                        <|> use without_pattern
                    in

                    let with_value =
                            (
                                with_label << -s"=" << -s"?"
                                >>|
                                fun (s, pat) -> Optional s, pat, None
                            )
                        <|> (
                                mapping begin fun (s, pat) expr -> Optional s, pat, Some expr end
                                <*> with_label << -s"=" <*> -use expression_constrainted
                            )
                        <|> (
                                with_label
                                >>|
                                fun (s, pat) -> Labelled s, pat, None
                            )
                    in

                    let nolabel =
                        (
                            use (mapping (mk_helper2 Pat.constraint_) <*> use @@ add_attrs pattern << -s":" <*> -use core_type)
                            <|>
                            use @@ add_attrs pattern
                        ) >>| fun p -> Nolabel, p, None
                    in

                    let constr_unit =
                        Named.p "expression:arrow:unit" begin
                            let unit = s"()" >>$ Longident.Lident "()" in
                            let pat = loc unit >>| fun lid -> mk_helper2 Pat.construct lid None in

                            mapping (mk_helper4 Exp.fun_ Nolabel None)
                            <*> use pat <*> -arrow_tail
                        end
                    in

                    let with_many_args =
                        let loop = fix_poly @@ fun getter ->
                            let args_loop = getter.get @@ fun x -> x.args_loop in
                            let types_loop = getter.get @@ fun x -> x.types_loop in

                            let tail =
                                    (s")" >> -arrow_tail)
                                <|> (s"," >> -s")" >> -arrow_tail)
                                <|> (s"," >> -use (add_attrs (k"type" >> -types_loop)))
                                <|> (s"," >> -use args_loop)
                            in

                            let curried =
                                    add_attrs (
                                        mapping begin fun (label, pat, expr) tail ->
                                            mk_helper4 Exp.fun_ label expr pat tail
                                        end
                                        <*> with_value <*> -tail
                                    )
                                <|> (
                                        mapping begin fun (label, pat, expr) tail ->
                                            mk_helper4 Exp.fun_ label expr pat tail
                                        end
                                        <*> nolabel <*> -tail
                                    )
                            in

                            {
                                args_loop =
                                        (s"." >> -curried >>| helper_add_attr "bs")
                                    <|> curried
                                ;
                                types_loop = begin
                                        (
                                            mapping @@ mk_helper2 Exp.newtype
                                            <*> loc l_ident <*> -use types_loop
                                        )
                                    <|> (s"," >> -add_attrs (k"type" >> -types_loop))
                                    <|> (s"," >> -args_loop)
                                end;
                            }
                        in

                        Named.p "expression:arrow:many_args" begin
                                add_attrs (s"(" >> -add_attrs (k"type" >> -loop.types_loop))
                            <|> add_attrs (s"(" >> -loop.args_loop)
                        end
                    in

                    let constr_unit_uncurried =
                        let unit = s"(" >> -s"." >> -s")" >>$ Longident.Lident "()" in
                        let pat = loc unit >>| fun lid -> mk_helper2 Pat.construct lid None in

                        Named.p "expression:arrow:unit_uncur" begin
                            mapping begin fun pat expr ->
                                helper_add_attr "bs"
                                (mk_helper4 Exp.fun_ Nolabel None pat expr)
                            end
                            <*> use pat <*> -arrow_tail
                        end
                    in

                    let only_arg =
                        Named.p "expression:arrow:one_arg" begin
                            add_attrs (
                                mapping (mk_helper4 Exp.fun_ Nolabel None)
                                <*> use pattern_atom << -s"=>" <*> -use expression_arrow
                            )
                        end
                    in

                    constr_unit <|> with_many_args <|> constr_unit_uncurried <|> only_arg
                end

            let tuple =
                Named.p "expression:tuple" begin
                    mapping (mk_helper ~f:Exp.tuple)
                    << s"(" <*> seq 2 (-use expression_constrainted) ~sep:(-s",") ~trail << -s")"
                end

            let list_helper =
                make_list_helper ~constr:Exp.construct ~tuple:Exp.tuple ~get_loc:(fun x -> x.pexp_loc)

            let list =
                Named.p "expression:list" begin
                        begin
                            mapping (list_helper [] None)
                            << s"list{" << -s"}"
                        end
                    <|> (s"list{" >> -s"..." >> -expression_constrainted << opt @@ -s"," << -s"}")
                    <|> begin
                            mapping list_helper
                            << s"list{" <*> seq 1 ~sep:(-s",") (-use expression_constrainted)
                            <*> (
                                        (-s"," >> -s"..." >> -use expression_constrainted << opt @@ -s"," >>| Option.some)
                                    <|> (opt @@ -s"," >>$ None)
                                )
                            << -s"}"
                        end
                end

            let case =
                mapping begin fun pat guard exp -> Exp.case pat ?guard exp end
                <*> use pattern <*> opt (-k"when" <|> -k"if" >> -use expression) << -s"=>" <*> -use expression_sequence

            let case_list =
                s"{" >> opt (-s"|") >> seq 1 (-case) ~sep:(-s"|") << -s"}"

            let switch =
                Named.p "expression:switch" begin
                    mapping @@ mk_helper2 Exp.match_
                    << k"switch" <*> -use expression << ng <*> case_list
                end

            let try_catch =
                Named.p "expression:try" begin
                    mapping @@ mk_helper2 Exp.try_
                    << k"try" <*> -use expression << -k"catch" <*> -case_list
                end

            let ifthenelse =
                fix @@ fun ifthenelse ->
                Named.p "expression:if" begin
                    mapping (mk_helper3 Exp.ifthenelse)
                    << k"if" <*> -use expression <*> -use scoped_sequence
                    <*>? (-k"else" >> -use (scoped_sequence <|> ifthenelse))
                end

            let for_ =
                Named.p "expression:for" begin
                    let direction =
                            (k"to" >>$ Upto)
                        <|> (k"downto" >>$ Downto)
                    in

                    (
                        mapping begin fun pat ef dir et ea -> mk_helper5 Exp.for_ pat ef et dir ea end
                        << k"for" <*> -use pattern << -k"in" <*> -use expression <*> -direction
                        <*> -use expression <*> -use scoped_sequence
                    )
                    <|>
                    (
                        mapping begin fun pat ef dir et ea -> mk_helper5 Exp.for_ pat ef et dir ea end
                        << k"for" << -s"(" <*> -use pattern << -k"in" <*> -use expression
                        <*> -direction <*> -use expression << -s")" <*> -use scoped_sequence
                    )
                end

            let while_ =
                Named.p "expression:while" begin
                    mapping @@ mk_helper2 Exp.while_
                    << k"while" <*> -use expression <*> -use scoped_sequence
                end

            let assert_ =
                Named.p "expression:assert" begin
                    mapping (mk_helper ~f:Exp.assert_)
                    << k"assert" <*> -use expression
                end

            let lazy_ =
                Named.p "expression:lazy" begin
                    mapping (mk_helper ~f:Exp.lazy_)
                    << k"lazy" <*> -use expression
                end

            let _unit_arg_ loc = Nolabel, Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
            let _unit_arg = _unit_arg_ Location.none

            let ident = mapping @@ mk_helper ~f:Exp.ident <*> loc l_longident

            let jsx =
                Named.p "expression:jsx" begin
                    let arg =
                            (
                                mapping begin fun { txt = label; loc } expr ->
                                    Labelled label, apply_ah @@ helper_add_attr "ns.namedArgLoc" ~loc expr
                                end
                                <*> loc l_ident << -s"=" <*> -set_loc expression_p0
                            )
                        <|> (
                                mapping begin fun { txt = label; loc } expr ->
                                    Optional label, apply_ah @@ helper_add_attr "ns.namedArgLoc" ~loc expr
                                end
                                <*> loc l_ident << -s"=" << -s"?" <*> -set_loc expression_p0
                            )
                        <|> (
                                mapping begin fun { txt = label; loc } ->
                                    Labelled label, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [label]
                                end
                                <*> loc l_ident
                            )
                        <|> (
                                mapping begin fun { txt = label; loc } ->
                                    Optional label, Hc.expr_id ~loc ~attrs:[Hc.attr "ns.namedArgLoc" ~loc] [label]
                                end
                                << s"?" <*> -loc l_ident
                            )
                    in

                    let children_list =
                        seq 0 (-use expression) >>| fun list -> list_helper list None
                    in

                    let children =
                        (s"..." >> expression)
                        <|>
                        children_list
                    in

                        Named.p "jsx:leaf" begin
                            mapping begin fun tag args -> mk_helper2 Exp.apply (Exp.ident tag) (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg
                                ])
                            end
                            << s"<" <*> -loc (l_ident >>| fun x -> Longident.Lident x) <*>* -arg << -s"/" << -s">"
                        end
                    <|> Named.p "jsx:tag" begin
                            mapping t3
                            << s"<" <*> -loc (l_ident >>| fun x -> Longident.Lident x) <*>* -arg << -s">"
                            <*> -use children << ng
                            >>= fun (tag, args, children) ->
                                let open Angstrom in
                                string "<" >> ng.p >> string "/" >> ng.p >> (exact_longident tag.txt).p << ng.p << string ">"
                                >>| fun _ ->
                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in
                                mk_helper2 Exp.apply (Exp.ident tag) (args @ tailargs)
                        end
                    <|> Named.p "jsx:ce:leaf" begin
                            mapping begin fun tag args ->
                                let tag =
                                    Exp.ident ~loc:tag.loc @@ Location.mkloc (Longident.Ldot (tag.txt, "createElement")) tag.loc
                                in

                                mk_helper2 Exp.apply tag (args @ [
                                    Labelled "children", Exp.construct (Location.mknoloc (Longident.Lident "[]")) None;
                                    _unit_arg])
                            end
                            << s"<" <*> -loc (l_longident <|> u_longident) <*>* -arg << -s"/" << -s">"
                        end
                    <|> Named.p "jsx:ce:tag" begin
                            mapping t3
                            << s"<" <*> -loc (l_longident <|> u_longident) <*>* -arg << -s">"
                            <*> -use children << ng
                            >>= fun ({ txt = tag; loc = tag_loc }, args, children) ->
                                let open Angstrom in
                                string "<" >> ng.p >> string "/" >> ng.p >> (exact_longident tag).p << ng.p << string ">"
                                >>| fun _ ->

                                let tailargs =
                                    (Labelled "children", children) :: [_unit_arg]
                                in

                                let tag =
                                    Exp.ident ~loc:tag_loc @@ Location.mkloc (Longident.Ldot (tag, "createElement")) tag_loc
                                in

                                mk_helper2 Exp.apply tag (args @ tailargs)
                        end
                    <|> Named.p "jsx:notag:item" begin
                            mapping begin fun expr -> list_helper [expr] None end
                            << s"<" << -s">" << -s"..." <*> -use expression << -s"<" << -s"/" << -s">"
                        end
                    <|> Named.p "jsx:notag" begin
                            s"<" >> -s">" >> children_list << -s"<" << -s"/" << -s">"
                        end
                end

            let jsx = jsx >>| helper_add_attr "JSX"

            let array =
                Named.p "expression:array" begin
                        (s"[" >> -s"]" >>$ mk_helper ~f:Exp.array [])
                    <|> (
                            mapping (mk_helper ~f:Exp.array)
                            << s"[" <*> seq 1 ~sep:(-s",") ~trail (-use expression_constrainted) << -s"]"
                        )
                end

            let record =
                Named.p "expression:record" begin
                    let _nb =
                            (
                                mapping t2
                                <*> -loc l_longident << -s":" <*> -use expression_arrow
                            )
                        <|> (-loc l_longident >>| fun lid -> lid, Exp.ident ~loc:lid.loc lid)
                    in

                    mapping begin fun expr list -> mk_helper2 Exp.record list expr end
                    << s"{" <*>? (-s"..." >> -use expression_constrainted << -s",")
                    <*> seq 1 _nb ~sep:(-s",") ~trail << -s"}"
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
                        <*> loc Constant.String.string <*>? (-s":" >> -use expression)
                    in

                    mapping begin fun list ->
                        mk_helper2 Exp.record list None
                    end
                    << s"{" <*> seq 1 (-row) ~sep:(-s",") ~trail << -s"}"
                in

                Named.p "bs_object" begin
                    use record >>| fun x -> mk_helper ~f:Exp.extension (Location.mkloc "obj" x.pexp_loc, PStr [Str.eval x])
                end

            let extension = mapping @@ mk_helper ~f:Exp.extension <*> extension

            let coerce =
                Named.p "expression:coerce" begin
                    mapping (fun a b -> mk_helper3 Exp.coerce a None b)
                    << s"(" <*> -use expression_constrainted << -s":>" <*> -use core_type_atom << -s")"
                end

            let interpolated_string qtag =
                let op = Exp.ident @@ Location.mknoloc @@ Longident.Lident "^" in

                let cons = mapping cons in

                let parts = fix @@ fun parts ->
                    cons
                    <*> take_while (function '\n' | '$' | '`' | '\\' | '\r' -> false | _ -> true)
                    <*> (
                                (cons <*> new_line <*> parts)
                            <|> (cons <*> (s"\\`" >>$ "`") <*> parts)
                            <|> (cons <*> (s"\\$" >>$ "$") <*> parts)
                            <|> (cons <*> (s"\\\\" >>$ "\\") <*> parts)
                            <|> (cons <*> (s"$" << failed (s"{")) <*> parts)
                            <|> return []
                        )
                in
                let string =
                    parts >>| fun l ->
                        let str = String.concat l in
                        helper_add_attr "res.template" @@
                        mk_helper ~f:Exp.constant @@
                        Const.string ~quotation_delimiter:qtag str
                in

                let string_part =
                    fix @@ fun string_part ->

                    let tail =
                        mapping begin fun expr pos tail prev ->
                            tail @@ apply
                                (mk_helper2 Exp.apply op [Nolabel, prev; Nolabel, expr])
                                prev.pexp_loc.loc_start pos
                        end
                        << s"${" <*> -use expression << -s"}" <*> pos <*> string_part
                    in

                    rapply pos @@ rapply string
                    (
                            (
                                s"`" >>$ fun str _p1 prev ->
                                helper_add_attr "res.template" @@
                                helper @@ fun ~p1 ~p2 ~attrs ->
                                    apply
                                        (
                                            helper_add_attrs attrs @@
                                            mk_helper2 Exp.apply op [
                                                Nolabel, prev;
                                                Nolabel, apply str p1 p2;
                                            ]
                                        )
                                        p1 p2
                            )
                        <|> (
                                mapping begin fun p2 tail str p1 prev ->
                                    let e = apply str p1 p2 in
                                    tail (
                                        apply (
                                            helper_add_attr "res.template" @@
                                            mk_helper2 Exp.apply op [Nolabel, prev; Nolabel, e]
                                        ) prev.pexp_loc.loc_start p2
                                    )
                                end
                                <*> pos <*> tail
                            )
                    )
                in

                s"`" >>
                (
                    rapply string
                    (
                            (s"`" >>$ fun str -> str)
                        <|> (
                                mapping begin fun pos1 expr pos2 tail str ->
                                    helper begin fun ~p1 ~p2 ~attrs ->
                                        let e0 =
                                            apply str p1 pos1
                                        in
                                        let e1 =
                                            apply
                                                (
                                                    helper_add_attr "res.template" @@
                                                    mk_helper2 Exp.apply op [Nolabel, e0; Nolabel, expr]
                                                )
                                                p1 pos2
                                        in
                                        apply (helper_add_attrs attrs @@ tail e1) p1 p2
                                    end
                                end
                                <*> pos << s"${" <*> -use expression << -s"}" <*> pos <*> string_part
                            )
                    )
                )

            let string =
                Named.p "expression:string" begin
                    Constant.String.multiline ~q:'"' >>| mk_helper ~f:Exp.constant
                end

            let js_string = Named.p "expression:js_string" @@ interpolated_string "js"
            let json_string =
                Named.p "expression:json_string" @@
                (s"json" >> interpolated_string "json")

            let constr_args =
                    (loc_of (s"(" >> -s")") >>| Hc.unit_expr)
                <|> use tuple
                <|> (s"(" >> -use expression_constrainted << opt @@ -s"," << -s")")


            let polyvariant =
                let tag =
                        u_ident
                    <|> Constant.String.string
                    <|> take_while (function '0'..'9' -> true | _ -> false)
                in

                Named.p "polyvariant" begin
                    mapping @@ mk_helper2 Exp.variant
                    << s"#" <*> tag <*>? constr_args
                end

            let true_ =
                mapping (fun a -> mk_helper2 Exp.construct a None)
                <*> loc (k"true" >>$ Longident.Lident "true")

            let false_ =
                mapping (fun a -> mk_helper2 Exp.construct a None)
                <*> loc (k"false" >>$ Longident.Lident "false")

            let string_ident =
                mapping @@ mk_helper ~f:Exp.ident
                << s"\\" <*> loc (Constant.String.string >>| fun x -> Longident.Lident x)

            let atom =
                Named.p "expression:atom" begin
                    Peek.first
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
                    ; mapping @@ mk_helper2 Exp.construct <*> loc u_longident <*>? constr_args
                    ; polyvariant
                    ; mapping (fun a -> mk_helper2 Exp.construct a None) <*> loc (s"()" >>$ Longident.Lident "()")
                    ; tuple
                    ; coerce
                    ; s"(" >> -add_attrs expression_constrainted << -s")"
                    ; array
                    ; scoped_sequence >>| helper_add_attr "ns.braces"
                    ; record
                    ; bs_object
                    ; extension
                    ; Constant.p >>| mk_helper ~f:Exp.constant
                    ]
                end

            let res_unit loc =
                let pattern = Pat.var @@ Location.mknoloc "__res_unit" in
                let expr = Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None in
                let expr2 = Exp.ident @@ Location.mknoloc @@ Longident.Lident "__res_unit" in
                let vb = Vb.mk ?docs:None ?text:None pattern expr in
                Exp.let_ Nonrecursive [vb] expr2

            let field_set_cont =
                Named.p "expression:field:set" begin
                    mapping begin fun lid expr prev ->
                        mk_helper3 Exp.setfield prev lid expr
                    end
                    << -s"." <*> -loc l_longident << -s "=" <*> -use expression_arrow
                end

            let object_set_cont =
                Named.p "expression:object:set" begin
                    mapping begin fun idx p1 loc value expr ->
                        let s = Exp.send ~loc:(make_location expr.pexp_loc.loc_start p1) expr idx in
                        let lid = Exp.ident ~loc @@ Location.mkloc (Longident.Lident "#=") loc in
                        mk_helper2 Exp.apply lid [Nolabel, s; Nolabel, value]
                    end
                    << -s"[" <*> -loc Constant.String.string << -s"]" <*> pos <*> -loc_of (s"=") <*> -use expression_arrow
                end

            let array_set_cont =
                Named.p "expression:array:set" begin
                    mapping begin fun idx value expr ->
                        mk_helper2 Exp.apply (Hc.expr_id ["Array"; "set"]) [Nolabel, expr; Nolabel, idx; Nolabel, value]
                    end
                    << -s"[" <*> -use expression << -s"]" << -s"=" <*> -use expression_arrow
                end

            let tail =
                Named.p "expression:tail" begin
                    let labelled _arg =
                            (
                                mapping begin fun { txt; loc } expr ->
                                    Optional txt,
                                    apply_ah @@ helper_add_attr "ns.namedArgLoc" ~loc expr
                                end
                                << s"~" <*> -loc l_ident << -s"=" << -s"?" <*> -set_loc expression_constrainted
                            )
                        <|> (
                                mapping begin fun tag expr -> Optional tag, expr end
                                << s"~" <*> -l_ident << -s"=" << -s"?" <*> -use _arg
                            )
                        <|> (
                                mapping begin fun { txt; loc } expr ->
                                    Labelled txt,
                                    apply_ah @@ helper_add_attr "ns.namedArgLoc" ~loc expr
                                end
                                << s"~" <*> -loc l_ident << -s"=" <*> -set_loc expression_constrainted
                            )
                        <|> (
                                mapping begin fun tag expr -> Labelled tag, expr end
                                << s"~" <*> -l_ident << -s"=" <*> -use _arg
                            )
                        <|> (
                                mapping begin fun { txt; loc } typ p2 ->
                                    let exp =
                                        apply
                                        (
                                            helper_add_attr "ns.namedArgLoc" ~loc @@
                                            mk_helper2 Exp.constraint_ (Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]) typ
                                        )
                                        loc.loc_start p2
                                    in
                                    Labelled txt, exp
                                end
                                << s"~" <*> -loc l_ident
                                << -s":" <*> -use core_type_atom <*> pos
                            )
                        <|> (
                                mapping begin fun { txt; loc } p2 ->
                                    let exp =
                                        apply
                                        (
                                            helper_add_attr "ns.namedArgLoc" ~loc @@
                                            mk_helper ~f:Exp.ident (Location.mkloc (Hc.lid [txt]) loc)
                                        )
                                        loc.loc_start p2
                                    in
                                    Optional txt, exp
                                end
                                << s"~" <*> -loc l_ident << -s"?" <*> pos
                            )
                        <|> (
                                mapping begin fun { txt; loc } ->
                                    Labelled txt
                                    ,
                                    Hc.expr_id ~loc ~attrs:[Hc.attr ~loc "ns.namedArgLoc"] [txt]
                                end
                                << s"~" <*> -loc l_ident
                            )
                        <|> (use (expression_constrainted <|> _arg) >>| fun e -> Nolabel, e)
                    in

                    let apply =
                        Named.p "expression:apply" begin
                            let flags = ref [] in

                            let _arg =
                                mapping @@ mk_helper ~f:Exp.ident
                                <*> loc (k"_" >> exec @@ fun _ -> List.hd_exn !flags := true; Longident.Lident "__x")
                            in

                            let params =
                                    (s"(" >> seq 1 ~sep:(-s",") ~trail (-labelled _arg) << -s")")
                                <|> (s"(" >> -loc_of (s")") >>| fun loc -> [_unit_arg_ loc])
                            in

                            let drop_flag = exec @@ fun _ ->
                                let f = !(List.hd_exn !flags) in
                                flags := List.tl_exn !flags;
                                f
                            in

                            exec (fun _ -> flags := ref false :: !flags) >>
                            (
                                mapping begin fun list flag expr ->
                                    mk_helper () ~f:begin fun ?loc ?attrs () ->
                                        if flag then
                                            let x = Exp.apply ?loc ?attrs expr list in
                                            Exp.fun_ ?loc ?attrs Nolabel None (Pat.var @@ Location.mknoloc "__x") x
                                        else
                                            Exp.apply ?loc ?attrs expr list
                                    end
                                end
                                << ng_no_new_line <*> params <*> drop_flag
                            ) <|> (drop_flag >> fail)
                        end
                    in

                    let apply_uncurried =
                        Named.p "expression:apply:uncurried" begin
                            let helper expr args =
                                helper_add_attr "bs" @@
                                mk_helper2 Exp.apply expr args
                            in

                            let apply_params =
                                s"." >> seq 1 ~sep:(-s",") (-labelled fail)
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
                                            << -s"," << ng <*> apply_params <*> pos
                                        )
                                end
                                >>| fun (hlp, _) -> hlp
                            in

                            let unit_arg =
                                    (
                                        s"(" >> -s"." >> -loc_of (s"()") << -s")"
                                        >>| fun loc -> [Nolabel, res_unit loc]
                                    )
                                <|> (s"(" >> -s"." >> -s")" >>$ [_unit_arg])
                            in

                                (ng >> unit_arg >>| fun arg expr -> helper expr arg)
                            <|> (-s"(" >> -args << opt (-s",") << -s")")
                        end
                    in

                    let field =
                        failed field_set_cont >>
                        Named.p "expression:field" begin
                            -s"." >> -loc l_longident >>| fun lid expr ->
                                mk_helper2 Exp.field expr lid
                        end
                    in

                    let object_get =
                        failed object_set_cont >>
                        Named.p "expression:send" begin
                            -s"[" >> -loc Constant.String.string << -s"]"
                            >>| fun str expr -> mk_helper2 Exp.send expr str
                        end
                    in

                    let array_get =
                        failed array_set_cont >>
                        Named.p "expression:index" begin
                            -s"[" >> -use expression_constrainted << -s"]"
                            >>| fun idx expr ->
                                mk_helper2 Exp.apply (Hc.expr_id ["Array"; "get"]) [Nolabel, expr; Nolabel, idx]
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
                        Mapping.unary_minus
                        <*> loc (op_alias "-." "~-." <|> op_alias "-" "~-") <*> -use unary
                    )
                <|> (
                        Mapping.unary_plus
                        <*> loc (op_alias "+." "~+." <|> op_alias "+" "~+") <*> -use unary
                    )
                <|> (
                        Mapping.unary_op
                        <*> loc @@ unops <*> -use unary
                    )
                <|> set

            let p0 =
                Named.p "expression:p0" @@
                add_attrs (ifthenelse <|> unary)

            let expression_p0 = p0

            let left_assoc mp ops =
                fold_hlp_left_0_n ~f:Mapping.binary_op
                mp (mapping t2 <*> -loc ops <*> -use mp)

            let left_assoc2 mp ops =
                fold_hlp_left_0_n ~f:Mapping.binary_op
                mp (mapping t2 <*> ops <*> -use mp)

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
                        (-loc (op_alias "++" "^" <|> o"+." <|> o"+"))
                    <|> (failed (ng_new_line >> (o"-." <|> o"-") >> p0) >> -loc (o"-." <|> o"-"))
                )
            let p5 =
                Named.p "expression:p5" @@
                left_assoc p4
                (
                    Peek.first
                    [ op_alias "===" "=="; op_alias "==" "="; failed (o"=>") >> o"="
                    ; op_alias "!==" "!="; op_alias "!=" "<>"; o"<="; o">="; o"|>"
                    ; failed jsx >> failed (s"<" >> -s"/") >> o"<"; o">"
                    ]
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

            let ternary =
                Named.p "expression:ternary" begin
                    (
                        mapping (mk_helper3 Exp.ifthenelse)
                        <*> use p8 << -s"?" <*> -use expression_arrow << -s":" <*> (-use expression_arrow >>| some)
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

    let expression = let (module M) = x in M.expression
    let expression_arrow = let (module M) = x in M.expression_arrow
    let expression_sequence = let (module M) = x in M.expression_sequence
    let expression_p0 = let (module M) = x in M.expression_p0
end
