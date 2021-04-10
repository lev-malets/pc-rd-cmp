open Core_kernel
open Basic
open Angstrom
open Angstrom.Let_syntax
open Angstrom.Parser

(* TODO errors position *)
(* TODO memoization *)
(* TODO mutable state ? check impact
    variants:
        remove alteration
        create state copy on alteration
*)

(* points:
    number of calls
    number of state changes
*)

type 'a helper = loc:Warnings.loc -> attrs:Parsetree.attributes -> 'a

module Make (Trace : Sigs.TRACE) = struct
    let sline_comment = Trace.point "sline comment" @@ fun _ ->
        let p = string "//" >> take_while (fun c -> c <> '\n' && c <> '\r') in
        let%map (str, loc) = with_location (consumed p) in
        Res_comment.makeSingleLineComment ~loc str
    let comments =
        let push_comment c =
            let%bind comment = c << whitespace in
            map_state (fun s -> {s with comments = comment :: s.comments})
        in
        many (push_comment sline_comment)

    let nongrammar = Trace.point "nongrammar" @@ fun _ ->
        nongrammar (whitespace << comments)
    let p_ p = p << nongrammar

    module Token = struct
        let token_ p = p >> nongrammar
        let char_ c = Trace.point ("token char '" ^ String.make 1 c ^ "'") @@ fun _ -> token_ @@ char c
        let string_ str = Trace.point ("token string '" ^ str ^ "'") @@ fun _ -> token_ @@ string str

        let dot_ = char_ '.'

        let bang_ = char_ '!'
        let at = char_ '@'
        let percent = char_ '%'

        let semicolon_ = char_ ';'
        let colon_ = char_ ':'
        let comma_ = char_ ','

        let end_of_scope = Trace.point "end of scope" @@ fun _ ->
            match%bind peek_char with
            | None -> return ()
            | Some '}' -> return ()
            | Some ')' -> return ()
            | _ -> fail ""

        let delimiter_ = Trace.point "delim" @@ fun _ ->
            semicolon_ <|> newline_skipped <|> end_of_scope
    end

    open Token

    module Brackets = struct
        let parentheses_open_ = char_ '('
        let parentheses_close_ = char_ ')'
        let in_parentheses_ p_ = parentheses_open_ >> p_ << parentheses_close_

        let braces_open_ = char_ '{'
        let braces_close_ = char_ '}'
        let in_braces_ p_ = braces_open_ >> p_ << braces_close_

        let brackets_open_ = char_ '['
        let brackets_close_ = char_ ']'
        let in_brackets_ p_ = brackets_open_ >> p_ << brackets_close_

        let chevrons_open_ = char_ '<'
        let chevrons_close_ = char_ '>'
        let in_chevrons_ p_ = chevrons_open_ >> p_ << chevrons_close_
    end

    module Constant = struct

        let with_literal p =
            let res = ref None in
            let%map str = consumed (p >>| fun v -> res := Some v) in
            let [@warning "-8"] Some v = !res in
            (v, str)

        module Number = struct
            let funs_2 =
                (function '0'..'1' -> true | _ -> false),
                None
            let funs_8 =
                (function '0'..'7' -> true | _ -> false),
                None
            let funs_10 =
                (function '0'..'9' -> true | _ -> false),
                Some (function 'e' | 'E' -> true | _ -> false)
            let funs_16 =
                (function '0'..'9' | 'A'..'F' | 'a'..'f' -> true | _ -> false),
                Some (function 'p' | 'P' -> true | _ -> false)

            let funs = peek_char_fail >>= function
                | 'b' | 'B' -> advance 1 >>$ funs_2
                | 'o' | 'O' -> advance 1 >>$ funs_8
                | 'x' | 'X' -> advance 1 >>$ funs_16
                | _ -> return funs_8
            let funs = (char '0' >> funs) <|> return funs_10

            let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

            let value_part =
                let%bind (digit, exp) = funs in
                let skip_digits = skip (fun c -> digit c || c = '_') in
                let%bind _ = skip digit >> skip_digits in
                let%bind is_float = option false (char '.' >> skip_digits >>$ true) in
                match exp with
                | None -> return is_float
                | Some exp -> option is_float (skip exp >> exp_sign >> skip digit >> skip_digits >>$ true)

            let suffix_part literal = peek_char_fail >>= function
                | 'n' ->
                    let msg =
                        "Unsupported number type (nativeint). Did you mean `"
                        ^ literal
                        ^ "`?"
                    in
                    let _category = Res_diagnostics.message msg in
                    let%bind _pos = position in
                    return (Some 'n') (*Diagnostics.diagnostic pos pos category >> advance 1 >>$ Some 'n'*)
                | 'g'..'z' | 'G'..'Z' as ch -> advance 1 >>$ Some ch
                | _ -> return None

            let p : Parsetree.constant Parser.t =
                let%bind (is_float, value) = with_literal value_part in
                let%map suffix = suffix_part value in
                match is_float with
                | true -> Parsetree.Pconst_float (value, suffix)
                | false -> Parsetree.Pconst_integer (value, suffix)
        end

        module Character = struct
            let code offset basic c = Char.to_int c - Char.to_int basic + offset
            let octal_code = satisfy (function '0'..'7' -> true | _ -> false) >>| code 0 '0'
            let decimal_code = satisfy (function '0'..'9' -> true | _ -> false) >>| code 0 '0'
            let hexadecimal_code =
                decimal_code
                <|>
                (satisfy (function 'a'..'f' -> true | _ -> false) >>| code 10 'a')
                <|>
                (satisfy (function 'A'..'F' -> true | _ -> false) >>| code 10 'A')

            let only_escaped = function
                | '\\' | '\"' | '\'' | '\n' | '\t' | '\b' | '\r' -> true
                | _ -> false

            let escaped = match%bind any_char with
                | '\\' | '\"' | '\'' | ' ' as c -> return c
                | 'n' -> return '\n'
                | 't' -> return '\t'
                | 'b' -> return '\b'
                | 'r' -> return '\r'
                | 'x' ->
                    let%map _1 = hexadecimal_code
                    and     _2 = hexadecimal_code in
                    Char.of_int_exn @@ _1 * 16 + _2
                | '0' .. '2' as ch ->
                    let _1 = code 0 '0' ch in
                    let%bind _2 = decimal_code in
                    let%map _3 = decimal_code in
                    Char.of_int_exn @@ _1 * 100 + _2 * 10 + _3
                | 'o' ->
                    let%bind _1 = satisfy (function '0'..'3' -> true | _ -> false) >>| code 0 '0' in
                    let%bind _2 = octal_code in
                    let%map _3 = octal_code in
                    Char.of_int_exn @@ _1 * 64 + _2 * 8 + _3
                | _ -> fail "TODO"

            let p = char '\'' >> (escaped <|> satisfy (Fn.compose not only_escaped)) << char '\''
            let p : Parsetree.constant Parser.t = Trace.point "character" @@ fun _ ->
                p >>| Ast_helper.Const.char
        end

        module String = struct
            let p = Trace.point "string skip" @@ fun _ -> fix @@ fun p ->
                skip_while (Fn.compose not Character.only_escaped) >> peek_char >>= function
                | Some '\\' -> advance 1 >> Character.escaped >> p
                | Some '\"' -> return ()
                | None -> fail "TODO: unclosed str"
                | _ -> fail "TODO"

            let string = char '\"' >> consumed p << char '\"'

            let p : Parsetree.constant Parser.t = Trace.point "string" @@ fun _ ->
                let%map literal = string in
                Ast_helper.Const.string literal
        end

        let p = Trace.point "constant" @@ fun _ ->
            choice [Number.p; Character.p; String.p]
    end

    module Keyword = struct
        let identifier's_character = function
            | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
            | _ -> false

        let keyword str = string str >> peek_char >>= function
            | None -> return ()
            | Some x ->
                match identifier's_character x with
                | true -> fail ""
                | false -> return ()
        let keyword_ str = p_ @@ keyword str

        let true_ = keyword_ "true"
        let false_ = keyword_ "false"
        let open_ = keyword_ "open"
        let let_ = keyword_ "let"
        let rec_ = keyword_ "rec"
        let and_ = keyword_ "and"
        let as_ = keyword_ "as"
        let exception_ = keyword_ "exception"
        let assert_ = keyword_ "assert"
        let lazy_ = keyword_ "lazy"
        let if_ = keyword_ "if"
        let else_ = keyword_ "else"
        let for_ = keyword_ "for"
        let in_ = keyword_ "in"
        let to_ = keyword_ "to"
        let downto_ = keyword_ "downto"
        let while_ = keyword_ "while"
        let switch_ = keyword_ "switch"
        let when_ = keyword_ "when"
        let external_ = keyword_ "external"
        let type_ = keyword_ "type"
        let private_ = keyword_ "private"
        let mutable_ = keyword_ "mutable"
        let constraint_ = keyword_ "constraint"
        let include_ = keyword_ "include"
        let module_ = keyword_ "module"
        let of_ = keyword_ "of"
        let with_ = keyword_ "with"
        let try_ = keyword_ "try"
        let import_ = keyword_ "import"
        let export_ = keyword_ "export"
    end

    module Name = struct
        let ident = Trace.point "ident" @@ fun _ ->
            (take_while1 Keyword.identifier's_character << nongrammar)
        let c_ident first = consumed (skip first >> skip_while Keyword.identifier's_character) << nongrammar

        let l_ident = c_ident lower
        let l_ident_ = p_ l_ident
        let u_ident = c_ident upper
        let u_ident_ = p_ u_ident

        let longident firsts last =
            let p p =
                let rec loop res =
                    (
                        dot_ >> match%bind peek_char_fail with
                        | c when firsts c -> ident >>= fun str -> loop @@ Longident.Ldot (res, str)
                        | c when last c -> ident >>| fun str -> Longident.Ldot (res, str)
                        | _ -> fail "TODO"
                    )
                    <|>
                    (Brackets.in_parentheses_ p >>= fun t -> loop @@ Lapply (res, t))
                    <|>
                    return res
                in
                match%bind peek_char_fail with
                | c when firsts c -> ident >>= fun str -> loop @@ Longident.Lident str
                | c when last c -> ident >>| fun str -> Longident.Lident str
                | _ -> fail "TODO"
            in
            fix p

        let module_name = Trace.point "module name" @@ fun _ ->
            longident upper upper
        let type_name = Trace.point "type name" @@ fun _ -> longident upper lower
        let vartype_name = c_ident ((=) '\'')
    end

    module Tree = struct
        open Parsetree

        type mutually_recursive =
            {
                signature : Parsetree.signature t;
                structure : Parsetree.structure t;
                attribute : Parsetree.attribute t;
            }

        let
            {
                signature;
                structure;
                _
            }
        = fix_poly @@ fun generator ->
            let signature = generator.gen @@ fun x -> x.signature in
            let structure = generator.gen @@ fun x -> x.structure in
            let attribute = generator.gen @@ fun x -> x.attribute in

            let attrs = many attribute in

            let with_loc_attrs p =
                let%map pos = position
                and attrs = attrs
                and res = p
                and end_pos = end_position
                in
                (res, make_location pos end_pos, attrs)
            in

            let mk_loc_attrs : 'a.
                (?loc:Warnings.loc -> ?attrs:attributes -> 'a) ->
                'a helper
                =
                fun f ->
                    fun ~loc ~attrs -> f ~loc ~attrs
            in

            let use : 'a. 'a helper t -> 'a t =
                fun mk ->
                    let%map (mk, loc, attrs) = with_loc_attrs mk in
                    mk ~loc ~attrs
            in

            let use_no_attrs : 'a. 'a helper t -> 'a t =
                fun mk ->
                    let%map (mk, loc) = with_location mk in
                    mk ~loc ~attrs:[]
            in

            let attributed: 'a 'b.
                (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) ->
                'a t -> 'b helper t
                =
                fun f p ->
                    let%map res = p in
                    mk_loc_attrs f res
            in

            let _attributed2: 'a 'b 'c.
                (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) ->
                'a t -> 'b t -> 'c helper t
                =
                fun f p1 p2 ->
                    let%map res1 = p1 and res2 = p2 in
                    mk_loc_attrs f res1 res2
            in

            let typexpr = Trace.point "typexr" @@ fun _ -> fix @@ fun typexpr ->
                let module Help = Ast_helper.Typ in

                let var = attributed Help.var Name.vartype_name in
                let any = attributed Help.any (char_ '_') in

                let constr =
                    let args = Trace.point __LOC__ @@ fun _ ->
                        Brackets.in_chevrons_ (sequence1 typexpr ~sep:comma_ << opt comma_) <|> return []
                    in
                    let%map name = loc Name.type_name and args = args in
                    mk_loc_attrs Help.constr name args
                in

                let tuple =
                    let p = Brackets.in_parentheses_ (sequence1 typexpr ~sep:comma_) in
                    attributed Help.tuple p
                in

                let unit =
                    let%map name = loc (string_ "()" >>$ Longident.Lident "unit") in
                    mk_loc_attrs Help.constr name []
                in

                let atom = Trace.point "typexpr atom" @@ fun _ ->
                    var
                    <|>
                    any
                    <|>
                    constr
                    <|>
                    tuple
                    <|>
                    unit
                in

                let arrow_tail label arg =
                    string_ "=>" >>
                    let%map typ = typexpr in
                    mk_loc_attrs Help.arrow label arg typ
                in

                let arrow = Trace.point "typexr arrow" @@ fun _ ->
                    let label =
                        let help = with_location Name.l_ident_ >>| fun (x, loc) -> Asttypes.Labelled x, Some loc in
                        option (Asttypes.Nolabel, None) (char_ '~' >> help << char_ ':')
                    in

                    let arg =
                        let%map (label, loc) = label
                        and x = use atom
                        in
                        let x =
                            match loc with
                            | None -> x
                            | Some loc -> Ast_helper.Typ.attr x (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr [])
                        in
                        (label, x)
                    in

                    let with_args = fix @@ fun with_args ->
                        let%bind (label, arg) = arg in
                        (
                            comma_ >>
                            let%map typ = use with_args in
                            mk_loc_attrs Help.arrow label arg typ
                        )
                        <|>
                        (Brackets.parentheses_close_ >> arrow_tail label arg)
                    in

                    (Brackets.parentheses_open_ >> with_args)
                    <|>
                    (
                        let%bind (label, arg) = arg in
                        arrow_tail label arg
                    )
                in

                let poly = Trace.point ("typexpr poly" ^ __LOC__) @@ fun _ ->
                    let%bind vars = many1 @@ loc Name.vartype_name in
                    match vars with
                    | [] -> failwith "unreachable"
                    | x::_ ->
                        match%bind peek_char_fail with
                        | '.' ->
                            let%map typ = dot_ >> typexpr in
                            mk_loc_attrs Help.poly vars typ
                        | _ ->
                            let arg = Ast_helper.Typ.var ~loc:x.loc x.txt in
                            let%map tail = arrow_tail Asttypes.Nolabel arg in
                            tail
                in

                use (poly <|> arrow <|> atom)
            in

            let pattern = Trace.point ("pattern: " ^ __LOC__) @@ fun _ -> fix @@ fun pattern ->
                let with_constr =
                    let%map pattern = pattern
                    and typ = opt (colon_ >> typexpr)
                    in
                    match typ with
                    | Some typ ->
                        Ast_helper.Pat.constraint_ ~loc:{pattern.ppat_loc with loc_end = typ.ptyp_loc.loc_end} pattern typ
                    | None -> pattern
                in
                let tuple = Trace.point "pattern tuple" @@ fun _ ->
                    let%map list = Brackets.in_parentheses_ @@
                        sequence1 ~sep:comma_ with_constr
                    in
                    match list with
                    | [] -> failwith "unreachable"
                    | [x] ->
                        fun ~loc ~attrs ->
                            {x with ppat_loc = loc; ppat_attributes = attrs @ x.ppat_attributes}
                    | _ ->
                        mk_loc_attrs Ast_helper.Pat.tuple list
                in

                let array =
                    Brackets.in_brackets_ @@ sequence1 ~sep:comma_ with_constr
                in

                let atom = Trace.point "pattern atom" @@ fun _ ->
                    (attributed Ast_helper.Pat.any @@ char_ '_')
                    <|>
                    (attributed Ast_helper.Pat.var (loc Name.ident))
                    <|>
                    (
                        let%map c1 = Constant.Character.p << string_ ".." and c2 = Constant.Character.p in
                        mk_loc_attrs Ast_helper.Pat.interval c1 c2
                    )
                    <|>
                    (attributed Ast_helper.Pat.constant Constant.p)
                    <|>
                    tuple
                    (* <|>
                        TODO list *)
                    <|>
                    (attributed Ast_helper.Pat.array array)
                in

                use atom
            in

            let attrubute_id = take_while (fun x -> Keyword.identifier's_character x || x = '.') in
            let payload = Trace.point "payload" @@ fun _ ->
                match%bind check Brackets.parentheses_open_ with
                | false -> return @@ PStr []
                | true ->
                    match%bind peek_char_fail with
                    | '?' ->
                        let%map pattern = advance 1 >> pattern << Brackets.parentheses_close_ in
                        PPat (pattern, None)
                    | ':' ->
                        (
                            let%map signature = advance 1 >> string_ "sig" >> signature << Brackets.parentheses_close_ in
                            PSig signature
                        )
                        <|>
                        (
                            let%map typ = advance 1 >> typexpr in
                            PTyp typ
                        )
                    | _ ->
                        let%map structure = structure << Brackets.parentheses_close_ in
                        PStr structure
            in

            let id_payload_pair start =
                both
                    (loc (start >> attrubute_id))
                    payload
                << nongrammar
            in

            let attribute = Trace.point ("attribute: " ^ __LOC__) @@ fun _ ->
                id_payload_pair at
            in

            let extension = Trace.point ("extension: " ^ __LOC__) @@ fun _ ->
                id_payload_pair percent
            in

            let module_extension = Trace.point ("module extension @ " ^ __LOC__) @@ fun _ ->
                id_payload_pair @@ string_ "%%"
            in

            let open_ = Trace.point "open" @@ fun _ ->
                let override = option Asttypes.Fresh (bang_ >>$ Asttypes.Override) in

                let p =
                    let%map override = Keyword.open_ >> override
                    and name = loc Name.module_name
                    in
                    mk_loc_attrs Ast_helper.Opn.mk ?docs:None ?override:(Some override) name
                in

                p
            in

            let type_desc =
                Trace.point "mk_type_desc" @@ fun _ ->
                char_ '=' >>
                (
                    let%map typ = typexpr in
                    mk_loc_attrs Ast_helper.Type.mk
                        ?docs:None ?text:None ?params:None ?cstrs:None ?kind:None ?priv:None ?manifest:(Some typ)
                )
            in

            let module_expr = Trace.point "module expr" @@ fun _ -> fix @@ fun _module_expr ->
                let module Help = Ast_helper.Mod in

                let atom =
                    attributed Help.ident (loc Name.module_name)
                in

                use atom
            in

            let module_type = Trace.point "module type" @@ fun _ -> fix @@ fun module_type ->
                let module Help = Ast_helper.Mty in

                let atom =
                    attributed Help.ident (loc Name.module_name)
                    <|>
                    Brackets.in_parentheses_ module_type
                    <|>
                    attributed Help.signature (Brackets.in_braces_ signature)
                    <|>
                    attributed Help.extension extension
                    <|>
                    (
                        let p = Keyword.module_ >> Keyword.type_ >> Keyword.of_ >> module_expr in
                        attributed Help.typeof_ p
                    )
                in

                let with_constraint = Trace.point "with constraint" @@ fun _ ->
                    Keyword.type_ >>
                    let%bind name = loc Name.type_name in

                    let help str =
                        let%map mk = type_desc in
                        let decl = mk {txt = str; loc = name.loc} in
                        fun ~loc:_ ~attrs ->
                            Parsetree.Pwith_type (name, decl ~loc:name.loc ~attrs)
                    in

                    match name.txt with
                    | Lident str -> help str
                    | Ldot (_, str) -> help str
                    | _ -> fail "TODO"
                in

                let atom_with =
                    let%map pos = position
                    and typ = use atom
                    and constrs = Keyword.with_ >> sequence1 ~sep:Keyword.and_ (use with_constraint)
                    and end_pos = end_position
                    in
                    fun ~loc:_ ~attrs ->
                        mk_loc_attrs Help.with_ ~loc:(make_location pos end_pos) ~attrs:[]
                            {typ with pmty_attributes = attrs} constrs
                in

                let atom_with = atom_with <|> atom in

                let functor_arg =
                    let%map par_name = loc (Name.u_ident <|> p_ (string "_"))
                    and par_type = match%bind check (char_ ':') with
                        | true -> use module_type >>| Option.some
                        | false -> return None
                    in
                    (par_name, par_type)
                in

                let tail = string_ "=>" >> use module_type in

                let with_functor_args = fix @@ fun with_functor_args ->
                    let mt =
                        match%bind p_ any_char with
                        | ',' -> use with_functor_args
                        | ')' -> tail
                        | _ -> fail "TODO"
                    in
                    let%map (par_name, par_type) = functor_arg
                    and mt = mt
                    in
                    mk_loc_attrs Help.functor_ par_name par_type mt
                in

                (Brackets.parentheses_open_ >> with_functor_args)
                <|>
                (
                    let%map arg = use atom_with
                    and tail = tail
                    in
                    mk_loc_attrs Help.functor_ (Location.mknoloc "_") (Some arg) tail
                )
                <|>
                atom_with
            in

            let label_declaration =
                let%map mut = option Asttypes.Immutable (Keyword.mutable_ >>$ Asttypes.Mutable)
                and name = loc Name.l_ident_
                and typ = colon_ >> typexpr
                in
                mk_loc_attrs Ast_helper.Type.field ?info:None ?mut:(Some mut) name typ
            in

            let constr_args =
                Brackets.in_parentheses_
                (
                    (
                        sequence1 ~sep:comma_ (typexpr) << opt comma_
                        >>|
                        fun x -> Parsetree.Pcstr_tuple x
                    )
                    <|>
                    (
                        Brackets.in_braces_ (sequence1 ~sep:comma_ (use label_declaration) << opt comma_)
                        >>|
                        (fun x -> Parsetree.Pcstr_record x)
                        <<
                        opt comma_
                    )
                )
            in

            let exception_sig = Trace.point "exception" @@ fun _ ->
                let kind =
                    match%bind peek_char with
                    | Some '(' ->
                        let%map args = constr_args in
                        Parsetree.Pext_decl (args, None)
                    | Some '=' ->
                        p_ (advance 1)
                        >>
                        let%map name = loc Name.module_name in Parsetree.Pext_rebind name
                    | Some ':' ->
                        p_ (advance 1)
                        >>
                        let%map typ = typexpr in
                        Parsetree.Pext_decl (Pcstr_tuple [], Some typ)
                    | _ -> return @@ Parsetree.Pext_decl (Pcstr_tuple [], None)
                in
                Keyword.exception_ >> begin
                    let%map name = loc Name.u_ident
                    and kind = kind
                    in
                    mk_loc_attrs Ast_helper.Te.constructor ?docs:None ?info:None name kind
                end
            in

            let include_sig =
                Keyword.include_ >>
                let p =
                    let%map (typ, loc) = with_location module_type in
                    fun ~loc:_ ~attrs -> typ ~loc ~attrs
                in
                let%map typ = use p in
                mk_loc_attrs Ast_helper.Incl.mk ?docs:None typ
            in

            let value_sig = Trace.point "sig value" @@ fun _ ->
                let%map _ = Keyword.let_
                and name = loc Name.l_ident
                and _ = colon_
                and typ = typexpr
                in
                mk_loc_attrs Ast_helper.Val.mk ?docs:None ?prim:None name typ
            in

            let modtype = Trace.point "modtype" @@ fun _ ->
                let%map name = Keyword.module_ >> Keyword.type_ >> loc Name.ident
                and mt =
                    match%bind opt @@ char_ '=' with
                    | Some _ -> use module_type >>| Option.some
                    | None -> return None
                in
                let helper = mk_loc_attrs Ast_helper.Mtd.mk in

                let helper = match mt with
                    | None -> helper ?typ:None
                    | Some typ -> helper ~typ
                in
                helper name
            in

            let type_desc =
                type_desc
                <|>
                return
                (
                    mk_loc_attrs Ast_helper.Type.mk
                        ?docs:None ?text:None ?params:None ?cstrs:None ?kind:None ?priv:None ?manifest:None
                )
            in

            let type_declaration_first =
                Keyword.type_ >>
                let%map rec_flag = option Asttypes.Nonrecursive (Keyword.rec_ >>$ Asttypes.Recursive)
                and name = loc Name.l_ident_
                and desc = type_desc
                in
                (rec_flag, desc name)
            in

            let type_sig = Trace.point "sig type" @@ fun _ ->
                let%map ((rec_flag, first), l) = with_location @@ type_declaration_first in
                fun ~loc ~attrs -> Ast_helper.Sig.type_ ~loc rec_flag [first ~loc:l ~attrs]
            in

            let external_ = Trace.point "external" @@ fun _ ->
                Keyword.external_
                >>
                let%map name = loc Name.l_ident_
                and typ = colon_ >> typexpr
                and prim = char_ '=' >> many1 (p_ Constant.String.string)
                in
                mk_loc_attrs Ast_helper.Val.mk ?docs:None ?prim:(Some prim) name typ
            in

            let item_helper : 'a 'b.
                (?loc:Warnings.loc -> 'a -> 'b) ->
                'a helper t ->
                'b helper t
                =
                fun h p ->
                    let%map res = p
                    and end_pos = end_position << delimiter_
                    in

                    fun ~loc ~attrs ->
                        let res = res ~loc:{loc with loc_end = end_pos} ~attrs in
                        h ~loc res
            in

            let signature = Trace.point "signature" @@ fun _ ->
                let open_ = item_helper Ast_helper.Sig.open_ open_ in
                let include_ = item_helper Ast_helper.Sig.include_ include_sig in
                let let_ = item_helper Ast_helper.Sig.value value_sig in
                let extension = attributed Ast_helper.Sig.extension module_extension in
                let exception_ = item_helper Ast_helper.Sig.exception_ exception_sig in
                let external_ = item_helper Ast_helper.Sig.value external_ in
                let modtype = item_helper Ast_helper.Sig.modtype modtype in

                let sig_item =
                    Trace.point ("signature item @ " ^ __LOC__) @@ fun _ ->
                    match%bind peek_char_fail with
                    | 'm' -> modtype
                    | 'o' -> open_
                    | 'i' -> include_
                    | 'l' -> let_
                    | '%' -> extension
                    | 'e' -> exception_ <|> external_
                    | 't' -> type_sig
                    | _ -> fail "TODO"
                in
                many @@ use (sig_item << delimiter_)
            in

            let expression = Trace.point "expression" @@ fun _ -> fix @@ fun expression ->
                let fn = Trace.point "expression fn" @@ fun _ ->
                    let arrow_tail = string_ "=>" >> use expression in

                    let arrow = Trace.point "expression arrow" @@ fun _ ->
                        let%map args =
                            Brackets.in_parentheses_ (sequence1 (both (pattern) position) ~sep:comma_)
                            <|>
                            (let%map x = both (pattern) position in [x])
                        and next = arrow_tail
                        and end_pos = end_position
                        in

                        let res = List.fold_right
                            ~f:(fun (arg, _pos) acc -> Ast_helper.Exp.fun_ ~loc:(make_location arg.ppat_loc.loc_start end_pos) Nolabel None arg acc)
                            ~init:next
                            args
                        in
                        fun ~loc ~attrs ->
                            {res with pexp_loc = loc; pexp_attributes = attrs}
                    in

                    let tuple = Trace.point "expr tuple" @@ fun _ ->
                        let%map list = Brackets.in_parentheses_ (sequence (use expression) ~sep:comma_) in
                        fun ~loc ~attrs ->
                            match list with
                            | [] -> failwith "unimplemented"
                            | [x] -> {x with pexp_loc = loc; pexp_attributes = attrs @ x.pexp_attributes}
                            | list -> Ast_helper.Exp.tuple ~loc ~attrs list
                    in

                    let atom = Trace.point "expr atom" @@ fun _ ->
                        (
                            let%map name = loc Name.type_name in
                            mk_loc_attrs Ast_helper.Exp.ident name
                        )
                        <|>
                        (
                            let%map c = Constant.p in
                            mk_loc_attrs Ast_helper.Exp.constant c
                        )
                        <|>
                        tuple
                        (* <|>
                            TODO list *)
                    in

                    arrow <|> atom
                in
                (* TODO
                fun P -> E1                          (Simple, None)
                fun ~l:P -> E1                       (Labelled l, None)
                fun ?l:P -> E1                       (Optional l, None)
                fun ?l:(P = E0) -> E1                (Optional l, Some E0)

                Notes:
                - If E0 is provided, only Optional is allowed.
                - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
                - "let f P = E" is represented using Pexp_fun.
                *)
                fn
            in

            let structure = Trace.point "structure" @@ fun _ ->
                let expression =
                    let%map exp = use_no_attrs expression in
                    mk_loc_attrs Ast_helper.Str.eval exp
                in

                let structure_item = Trace.point "structure_item" @@ fun _ ->
                    expression
                in
                many @@ use (structure_item << delimiter_)
            in

            {
                signature;
                structure;
                attribute;
            }
    end

    let with_print p =
        let%map res = p
        and _state = get_state
        in
        (*Res_diagnostics.printReport state.diagnostics "TODO";*)
        res

    let parse p state filename =
        parse_string p state ~filename (Res_io.readFile ~filename)

    let parse_interface = parse (with_print Tree.signature) State.default
    let parse_implementation = parse (with_print Tree.structure) State.default
end
