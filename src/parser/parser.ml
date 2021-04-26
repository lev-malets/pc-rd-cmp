open Basic
open Angstrom
open Angstrom.Let_syntax
open Angstrom.Parser
open Parsetree

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

module Helper = struct
    type 'a loc = loc:Warnings.loc -> 'a
    type 'a loc_start = loc_start:Lexing.position -> 'a
    type 'a loc_end = loc_end:Lexing.position -> 'a
    type 'a attrs = attrs:attributes -> 'a
    type 'a t = 'a attrs loc_end loc_start

    let mk : 'a.
        (?loc:Warnings.loc -> ?attrs:attributes -> 'a) ->
        'a t
        =
        fun f ->
            fun ~loc_start ~loc_end ~attrs -> f ~loc:(make_location loc_start loc_end) ~attrs

    let from_loc : 'a. 'a loc -> 'a loc_end loc_start
        =
        fun f ->
            fun ~loc_start ~loc_end -> f ~loc:(make_location loc_start loc_end)

    let from_attrs : 'a. 'a attrs -> 'a t
        =
        fun f ->
            fun ~loc_start:_ ~loc_end:_ -> f
end

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
    let nongrammar =
        whitespace << comments
    let nongrammar = Trace.point "nongrammar" @@ fun _ ->
        memo_by_pos nongrammar

    let ng = nongrammar

    let with_location p = nongrammar >> with_location p
    let loc p = with_location p >>| fun (x, loc) -> Location.mkloc x loc

    let position_delimiter =
        position >>= fun pos ->
        ng >> peek_char >>= function
        | Some '}' -> return pos
        | Some ')' -> return pos
        | Some ';' -> advance 1 >> position
        | None -> return pos
        | _ ->
            position >>= fun pos1 ->
            match pos1.pos_lnum = pos.pos_lnum with
            | true -> fail ""
            | false -> return pos

    module Token = struct
        let char c = Trace.point ("char '" ^ String.make 1 c ^ "'") @@ fun _ ->
            char c

        let tilda = char '~'
        let dot = char '.'
        let dot_ng = nongrammar << dot
        let eq = char '='
        let eq_ng = nongrammar << eq
        let bang = char '!'
        let at = char '@'
        let percent = char '%'

        let semicolon = char ';'
        let colon = char ':'
        let colon_ng = nongrammar << colon
        let comma = char ','
        let comma_ng = nongrammar << comma

        let underscore = char '_'

        let opn_round = char '('
        let cls_round = char ')'
        let cls_round_ng = nongrammar << cls_round

        let opn_curly = char '{'
        let cls_curly = char '}'
        let cls_curly_ng = nongrammar << cls_curly

        let opn_square = char '['
        let cls_square = char ']'
        let cls_square_ng = nongrammar << cls_square

        let opn_angle = char '<'
        let cls_angle = char '>'
        let cls_angle_ng = nongrammar << cls_angle

        let string str = Trace.point ("token string '" ^ str ^ "'") @@ fun _ ->
            string str

        let unit = string "()"
        let arrow = string "=>"
    end

    open Token

    module Brackets = struct

        let in_ opn cls p = opn >> nongrammar >> p << nongrammar << cls
        
        let in_round p = in_ opn_round cls_round_ng p
        let in_curly p = in_ opn_curly cls_curly_ng p
        let in_square p = in_ opn_square cls_square_ng p
        let in_angle p = in_ opn_angle cls_angle_ng p
    end

    module Constant = struct

        let with_literal p =
            let res = ref None in
            let%map str = consumed (p >>| fun v -> res := Some v) in
            match [@warning "-8"] !res with
            | Some v -> (v, str)

        module Number = struct

            let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

            let value_part digit exp =
                let skip_digits = skip (fun c -> digit c || c = '_') in
                skip digit >> skip_digits >>
                let%bind is_float = option false (char '.' >> skip_digits >>$ true) in
                match exp with
                | None -> return is_float
                | Some exp -> option is_float (skip exp >> exp_sign >> skip digit >> skip_digits >>$ true)

            let value_part_2 = value_part
                (function '0'..'1' -> true | _ -> false)
                None
            let value_part_8 = value_part
                (function '0'..'7' -> true | _ -> false)
                None
            let value_part_10 = value_part
                (function '0'..'9' -> true | _ -> false)
                (Some (function 'e' | 'E' -> true | _ -> false))
            let value_part_16 = value_part
                (function '0'..'9' | 'A'..'F' | 'a'..'f' -> true | _ -> false)
                (Some (function 'p' | 'P' -> true | _ -> false))

            let value_part =
                peek_char_fail >>= function
                | 'b' | 'B' -> advance 1 >> value_part_2
                | 'o' | 'O' -> advance 1 >> value_part_8
                | 'x' | 'X' -> advance 1 >> value_part_16
                | _ -> value_part_8
            let value_part = (char '0' >> value_part) <|> value_part_10

            let suffix_part = opt @@ satisfy (function 'g'..'z' | 'G'..'Z' -> true | _ -> false)

            let p =
                map2
                (with_literal value_part)
                suffix_part
                ~f:(fun (is_float, value) suffix ->
                    match is_float with
                    | true -> Pconst_float (value, suffix)
                    | false -> Pconst_integer (value, suffix)
                )
        end

        module Character = struct
            let code offset basic c = Char.code c - Char.code basic + offset
            let octal_code = satisfy (function '0'..'7' -> true | _ -> false) >>| code 0 '0'
            let decimal_code = satisfy (function '0'..'9' -> true | _ -> false) >>| code 0 '0'
            let hexadecimal_code =
                decimal_code
                <|>
                (satisfy (function 'a'..'f' -> true | _ -> false) >>| code 10 'a')
                <|>
                (satisfy (function 'A'..'F' -> true | _ -> false) >>| code 10 'A')

            let not_escaped = function
                | '\\' | '\"' | '\'' | '\n' | '\t' | '\b' | '\r' -> false
                | _ -> true

            let escaped =
                any_char >>= function
                | '\\' | '\"' | '\'' | ' ' as c -> return c
                | 'n' -> return '\n'
                | 't' -> return '\t'
                | 'b' -> return '\b'
                | 'r' -> return '\r'
                | 'x' ->
                    map2
                    hexadecimal_code
                    hexadecimal_code
                    ~f:(fun _1 _2 -> Char.chr @@ _1 * 16 + _2)
                | '0' .. '2' as ch ->
                    let _1 = code 0 '0' ch in
                    map2 decimal_code decimal_code
                    ~f:(fun _2 _3 -> Char.chr @@ _1 * 100 + _2 * 10 + _3)
                | 'o' ->
                    map3
                    (satisfy (function '0'..'3' -> true | _ -> false) >>| code 0 '0')
                    octal_code
                    octal_code
                    ~f:(fun _1 _2 _3 -> Char.chr @@ _1 * 64 + _2 * 8 + _3)
                | _ -> fail "TODO"

            let p = char '\'' >> (escaped <|> satisfy not_escaped) << char '\''
            let p = Trace.point "character" @@ fun _ ->
                p >>| Ast_helper.Const.char
        end

        module String = struct
            let p = Trace.point "string skip" @@ fun _ -> fix @@ fun p ->
                skip_while Character.not_escaped >>
                match%bind peek_char with
                | Some '\\' -> advance 1 >> Character.escaped >> p
                | Some '\"' -> return ()
                | None -> fail "TODO: unclosed str"
                | _ -> fail "TODO"

            let string = char '\"' >> consumed p << char '\"'

            let p = Trace.point "string" @@ fun _ ->
                let%map literal = string in
                Ast_helper.Const.string literal
        end

        let is_begin_symbol = function
            | '0'..'9' | '-' | '.'
            | '\''
            | '\"' -> true
            | _ -> false

        let p = Trace.point "constant" @@ fun _ ->
            match%bind peek_char_fail with
            | '0'..'9' | '-' | '.' -> Number.p
            | '\'' -> Character.p
            | '\"' -> String.p
            | _ -> fail "TODO"
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

        let true_ = keyword "true"
        let false_ = keyword "false"
        let open_ = keyword "open"
        let let_ = keyword "let"
        let rec_ = keyword "rec"
        let and_ = keyword "and"
        let as_ = keyword "as"
        let exception_ = keyword "exception"
        let assert_ = keyword "assert"
        let lazy_ = keyword "lazy"
        let if_ = keyword "if"
        let else_ = keyword "else"
        let for_ = keyword "for"
        let in_ = keyword "in"
        let to_ = keyword "to"
        let downto_ = keyword "downto"
        let while_ = keyword "while"
        let switch_ = keyword "switch"
        let when_ = keyword "when"
        let external_ = keyword "external"
        let type_ = keyword "type"
        let type_ng = nongrammar << type_
        let private_ = keyword "private"
        let private_ng = nongrammar << private_
        let mutable_ = keyword "mutable"
        let constraint_ = keyword "constraint"
        let include_ = keyword "include"
        let module_ = keyword "module"
        let of_ = keyword "of"
        let of_ng = nongrammar << of_
        let with_ = keyword "with"
        let try_ = keyword "try"
        let import_ = keyword "import"
        let export_ = keyword "export"

        let sig_ = keyword "sig"
        let sig_ng = nongrammar << keyword "sig"
    end

    module Name = struct
        let ident = Trace.point "ident" @@ fun _ ->
            take_while1 Keyword.identifier's_character
        let c_ident first = consumed (skip first >> skip_while Keyword.identifier's_character)

        let l_ident = c_ident lower
        let u_ident = c_ident upper

        let longident firsts last =
            let p p =
                let rec loop res =
                    (
                        dot_ng >> peek_char_fail >>= function
                        | c when firsts c -> ident >>= fun str -> loop @@ Longident.Ldot (res, str)
                        | c when last c -> ident >>| fun str -> Longident.Ldot (res, str)
                        | _ -> fail "TODO"
                    )
                    <|>
                    (nongrammar >> Brackets.in_round p >>= fun t -> loop @@ Lapply (res, t))
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
    end

    module Tree = struct

        type mutually_recursive =
            {
                signature : signature t;
                structure : structure t;
                attribute : attribute t;
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
                tuple4
                    position attrs
                    (ng >> p) position
            in

            let use : 'a. 'a Helper.t t -> 'a t =
                fun mk ->
                    map4
                    (nongrammar >> position)
                    attrs
                    (nongrammar >> mk)
                    position
                    ~f:(fun loc_start attrs mk loc_end ->
                        mk ~loc_start ~loc_end ~attrs
                    )
            in

            let use_loc : 'a. 'a Helper.loc_end Helper.loc_start t -> 'a t =
                fun mk ->
                    map3 (ng >> position) mk position
                    ~f:(fun loc_start mk loc_end -> mk ~loc_start ~loc_end)
            in

            let _set_attrs : 'a. 'a Helper.t t -> 'a Helper.t t =
                fun mk ->
                    let%map attrs_tail = attrs
                    and mk = nongrammar >> mk
                    in
                    fun ~loc_start ~loc_end ~attrs -> mk ~loc_start ~loc_end ~attrs:(attrs @ attrs_tail)
            in

            let attributed: 'a 'b.
                (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) ->
                'a t -> 'b Helper.t t
                =
                fun f p ->
                    let%map res = p in
                    Helper.mk f res
            in

            let attributed2: 'a 'b 'c.
                (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) ->
                'a t -> 'b t -> 'c Helper.t t
                =
                fun f p1 p2 ->
                    let%map res1 = p1 and res2 = p2 in
                    Helper.mk f res1 res2
            in

            let no_loc_attrs: 'a.
                'a t -> 'a Helper.t t
                =
                fun p ->
                    let%map res = p in
                    fun ~loc_start:_ ~loc_end:_ ~attrs:_ -> res
            in

            let typexpr = Trace.point "typexr" @@ fun _ -> fix @@ fun typexpr ->
                let module Help = Ast_helper.Typ in

                let types = sequence1 (use typexpr) ~sep:comma_ng in

                let var = char '\'' >> map2
                    position
                    Name.l_ident
                    ~f:(fun loc_start name ->
                        fun ~loc_start:_ -> Helper.mk Help.var name ~loc_start
                    )
                in
                let any = attributed Help.any (char '_' >>$ ()) in

                let constr =
                    let args = Trace.point __LOC__ @@ fun _ ->
                        Brackets.in_angle (types << opt comma_ng) <|> return []
                    in
                    let%map name = loc Name.type_name and args = args in
                    Helper.mk Help.constr name args
                in

                let tuple =
                    let p = Brackets.in_round types in
                    attributed Help.tuple p
                in

                let unit =
                    let%map name = loc (unit >>$ Longident.Lident "unit") in
                    Helper.from_attrs @@ fun ~attrs ->
                        Help.constr ~attrs name []
                in

                let atom = Trace.point "typexpr atom" @@ fun _ ->
                    match%bind peek_char_fail with
                    | '_' -> any
                    | '\'' -> var
                    | '(' -> unit <|> tuple
                    | _ -> constr
                in

                let arrow_tail label arg =
                    arrow >>
                    map (use typexpr) ~f:(fun typ -> Helper.mk Help.arrow label arg typ)
                in
                
                let arrow = Trace.point "typexr arrow" @@ fun _ ->
                    let label =
                        let help = with_location Name.l_ident >>| fun (x, loc) -> Asttypes.Labelled x, Some loc in
                        option (Asttypes.Nolabel, None) (tilda >> help << colon_ng)
                    in

                    let arg =
                        let%map (label, loc) = label
                        and (loc_start, attrs, x, loc_end) = ng >> with_loc_attrs atom
                        in
                        let x = x ~loc_start ~loc_end in
                        let x =
                            match loc with
                            | None -> x ~attrs
                            | Some loc -> x ~attrs:((Location.mkloc "ns.namedArgLoc" loc, PStr []) :: attrs)
                        in
                        (label, x)
                    in

                    let with_args = fix @@ fun with_args ->
                        let%bind (label, arg) = arg in
                        any_char >>= function
                        | ',' ->
                            map (use with_args) ~f:(fun typ -> Helper.mk Help.arrow label arg typ)
                        | ')' -> ng >> arrow_tail label arg
                        | _ -> fail "TODO"
                    in

                    (opn_round >> ng >> with_args)
                    <|>
                    (arg >>= fun (label, arg) -> ng >> arrow_tail label arg)
                in

                let poly = Trace.point ("typexpr poly" ^ __LOC__) @@ fun _ ->
                    attributed2 Help.poly
                        (many1 @@ (nongrammar >> char '\'' >> loc Name.l_ident))
                        (dot_ng >> use typexpr)
                in

                poly <|> arrow <|> atom
            in

            let use_typexpr = use typexpr in

            let pattern = Trace.point ("pattern: " ^ __LOC__) @@ fun _ -> fix @@ fun pattern ->
                let module Help = Ast_helper.Pat in

                let with_constr =
                    ( attributed2 Help.constraint_
                        (use pattern)
                        use_typexpr
                    )
                    <|>
                    pattern
                in
                let tuple = Trace.point "pattern tuple" @@ fun _ ->
                     let%map (hd, list) = Brackets.in_round @@ both
                        (use with_constr << comma_ng)
                        (sequence1 ~sep:comma_ng (use with_constr))
                    in
                    Helper.mk Ast_helper.Pat.tuple (hd::list)
                in

                let array =
                    Brackets.in_curly @@ sequence1 ~sep:comma_ng (use with_constr)
                in

                let atom = Trace.point "pattern atom" @@ fun _ ->
                    match%bind peek_char_fail with
                    | '_' -> attributed Ast_helper.Pat.any (underscore >>$ ())
                    | '\'' ->
                        attributed2 Help.interval
                            (Constant.Character.p << nongrammar)
                            (string ".." >> nongrammar >> Constant.Character.p)
                    | '(' -> tuple <|> Brackets.in_round pattern
                    | '[' -> attributed Ast_helper.Pat.array array
                    | c when Constant.is_begin_symbol c ->
                        attributed Ast_helper.Pat.constant Constant.p
                    | _ ->
                        attributed Ast_helper.Pat.var (loc Name.ident)
                    (* <|>
                        TODO list *)
                in

                atom
            in

            let attrubute_id = take_while1 (fun x -> Keyword.identifier's_character x || x = '.') in
            let payload = Trace.point "payload" @@ fun _ ->
                check (char '(') >>= function
                | false -> return @@ PStr []
                | true ->
                    peek_char_fail >>= function
                    | '?' ->
                        advance 1 >> use pattern << cls_round_ng >>| fun x -> PPat (x, None)
                    | ':' ->
                        (
                            advance 1 >> nongrammar >> peek_char_fail >>= function
                            | 's' -> Keyword.sig_ng >> signature << cls_round_ng >>| fun x -> PSig x
                            | _ -> use_typexpr >>| fun x -> PTyp x
                        )
                    | _ -> structure << cls_round_ng >>| fun x -> PStr x
            in

            let id_payload_pair start =
                both
                    (loc (start >> attrubute_id))
                    payload
            in

            let attribute = Trace.point ("attribute: " ^ __LOC__) @@ fun _ ->
                id_payload_pair at
            in

            let extension = Trace.point ("extension: " ^ __LOC__) @@ fun _ ->
                id_payload_pair percent
            in

            let module_extension = Trace.point ("module extension @ " ^ __LOC__) @@ fun _ ->
                id_payload_pair @@ string "%%"
            in

            let module_attribute =
                id_payload_pair @@ string "@@"
            in

            let open_ = Trace.point "open" @@ fun _ ->
                let override = option Asttypes.Fresh (bang >>$ Asttypes.Override) in

                let p =
                    map2
                    (Keyword.open_ >> override)
                    (loc Name.module_name)
                    ~f:(fun override name ->
                        Helper.mk Ast_helper.Opn.mk ?docs:None ?override:(Some override) name
                    )
                in

                p
            in

            let label_declaration =
                map3
                (option Asttypes.Immutable (Keyword.mutable_ >>$ Asttypes.Mutable))
                (nongrammar >> loc Name.l_ident)
                (colon_ng >> use_typexpr)
                ~f:(fun mut name typ ->
                    Helper.mk Ast_helper.Type.field ?info:None ?mut:(Some mut) name typ
                )
            in

            let constr_args =
                Brackets.in_round
                (
                    (
                        peek_char_fail >>= function
                        | '{' ->
                            Brackets.in_curly (sequence1 ~sep:comma_ng (use label_declaration) << opt comma_ng)
                            >>|
                            fun x -> Pcstr_record x
                        | _ -> 
                            sequence1 ~sep:comma_ng use_typexpr
                            >>|
                            fun x -> Pcstr_tuple x
                    )
                    <<
                    opt comma_ng
                )
            in

            let type_kind_variant = Trace.point "type_kind_variant" @@ fun _ ->
                let variant =
                    map2
                    (loc Name.u_ident)
                    (nongrammar >> constr_args)
                    ~f:(fun name args ->
                        Helper.mk Ast_helper.Type.constructor ?info:None ?args:(Some args) ?res:None name
                    )
                in

                let first = use variant in
                let other = use (char '|' >> variant) in

                map2
                first
                (many other)
                ~f:(fun hd tail ->
                    Ptype_variant (hd::tail)
                )
            in

            let type_params =
                let variance =
                    option Asttypes.Invariant
                    (
                        (char '-' >>$ Asttypes.Contravariant)
                        <|>
                        (char '+' >>$ Asttypes.Covariant)
                    )
                in

                let param =
                    map2 variance use_typexpr ~f:(fun v t -> (t, v))
                in

                Brackets.in_angle (sequence1 ~sep:comma_ng param)
            in

            let type_desc = Trace.point "mk_type_desc" @@ fun _ ->
                (
                    map2
                    (opt type_params)
                    (eq_ng >> type_kind_variant)
                    ~f:(fun params kind ->
                        Helper.mk Ast_helper.Type.mk
                            ?docs:None ?text:None ?params ?cstrs:None ?kind:(Some kind) ?priv:None ?manifest:None
                    )
                )
                <|>
                (
                    map3
                    (opt type_params)
                    (eq_ng >> use_typexpr)
                    (opt (eq_ng >> type_kind_variant))
                    ~f:(fun params manifest kind ->
                        Helper.mk Ast_helper.Type.mk
                            ?docs:None ?text:None ?params ?cstrs:None ?kind ?priv:None ?manifest:(Some manifest)
                    )
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

                let atom = Trace.point "module type atom" @@ fun _ ->
                    match%bind peek_char_fail with
                    | '(' -> Brackets.in_round module_type
                    | '{' -> attributed Help.signature (Brackets.in_curly signature)
                    | '%' -> attributed Help.extension extension
                    | 'm' ->
                        let p = Keyword.module_ >> Keyword.type_ng >> Keyword.of_ng >> ng >> module_expr in
                        attributed Help.typeof_ p
                    | _ -> attributed Help.ident (loc Name.module_name)
                in

                let atom = memo_by_pos atom in

                let with_constraint = Trace.point "with constraint" @@ fun _ ->
                    Keyword.type_ >>
                    loc Name.type_name >>= fun name ->

                    let help str =
                        type_desc >>| fun mk ->
                            let decl = mk {txt = str; loc = name.loc} in
                            Helper.from_attrs @@ fun ~attrs ->
                                Pwith_type (name, decl ~loc_start:name.loc.loc_start ~loc_end:name.loc.loc_end ~attrs)
                    in

                    match name.txt with
                    | Lident str -> help str
                    | Ldot (_, str) -> help str
                    | _ -> fail "TODO"
                in

                let atom_with = map2
                    (use_loc atom)
                    (ng >> Keyword.with_ >> sequence1 ~sep:Keyword.and_ (use with_constraint))
                    ~f:(fun typ constrs ->
                        fun ~loc_start ~loc_end ~attrs ->
                            let typ = typ ~attrs in
                            Helper.mk Help.with_ ~loc_start ~loc_end ~attrs:[] typ constrs
                    )
                in

                let atom_with = atom_with <|> atom in
                let atom_with = memo_by_pos atom_with in

                let functor_arg =
                    both
                    (loc (Name.u_ident <|> string "_"))
                    (check colon_ng >>= function
                    | true -> use module_type >>| Base.Option.some
                    | false -> return None
                    )
                in

                let tail = arrow >> use module_type in
                let tail_ng = nongrammar >> tail in

                let with_functor_args = fix @@ fun with_functor_args ->
                    map2
                    functor_arg
                    (
                        nongrammar >> any_char >>= function
                        | ',' -> use with_functor_args
                        | ')' -> tail_ng
                        | _ -> fail "TODO"
                    )
                    ~f:(fun (par_name, par_type) mt ->
                        Helper.mk Help.functor_ par_name par_type mt
                    )
                in

                (opn_round >> nongrammar >> with_functor_args)
                <|>
                (
                    map2
                    (loc (unit >>$ "*"))
                    tail_ng
                    ~f:(fun name tail ->
                        Helper.mk Help.functor_ name None tail
                    )
                )
                <|>
                (
                    map2
                    (use atom_with)
                    tail_ng
                    ~f:(fun arg tail ->
                        Helper.mk Help.functor_ (Location.mknoloc "_") (Some arg) tail
                    )
                )
                <|>
                atom_with
            in

            let ignore_attrs_start p =
                map2
                position
                p
                ~f:(fun pos res ->
                    fun ~loc_start:_ -> res ~loc_start:pos
                )
            in

            let extension_kind = Trace.point "extension constr kind" @@ fun _ ->
                peek_char_fail >>= function
                | '(' ->
                    nongrammar >> constr_args >>| fun x -> Pext_decl (x, None)
                | '=' ->
                    advance 1 >> nongrammar >> loc Name.module_name >>| fun x -> Pext_rebind x
                | ':' ->
                    advance 1 >> use_typexpr >>| fun x -> Pext_decl (Pcstr_tuple [], Some x)
                | _ -> fail "TODO"
            in

            let extension_constructor = Trace.point "extension constr" @@ fun _ ->
                map2
                (loc Name.u_ident)
                (
                    (nongrammar >> extension_kind)
                    <|>
                    (return @@ Pext_decl (Pcstr_tuple [], None))
                )
                ~f:(fun name kind ->
                    Helper.mk Ast_helper.Te.constructor ?docs:None ?info:None name kind
                )
            in

            let exception_sig = Trace.point "exception" @@ fun _ ->
                Keyword.exception_ >> extension_constructor
            in

            let exception_sig = ignore_attrs_start exception_sig in

            let include_sig =
                Keyword.include_ >>
                let p =
                    use_loc module_type >>| fun typ -> Helper.from_attrs typ
                in
                use p >>| fun typ -> Helper.mk Ast_helper.Incl.mk ?docs:None typ
            in

            let value_sig = Trace.point "sig value" @@ fun _ ->
                map2
                (Keyword.let_ >> loc Name.l_ident)
                (colon_ng >> use_typexpr)
                ~f:(fun name typ ->
                    Helper.mk Ast_helper.Val.mk ?docs:None ?prim:None name typ
                )
            in

            let value_sig = ignore_attrs_start value_sig in

            let modtype_base = Trace.point "modtype" @@ fun _ ->
                let%map name = loc Name.ident
                and mt =
                    check eq_ng >>= function
                    | true -> use module_type >>| Base.Option.some
                    | false -> return None
                in
                let helper = Ast_helper.Mtd.mk ?docs:None ?text:None in

                let helper = match mt with
                    | None -> helper ?typ:None
                    | Some typ -> helper ~typ
                in
                (name, helper)
            in

            let modtype_sig = Trace.point "modtype sig" @@ fun _ ->
                Keyword.module_ >> Keyword.type_ng >>
                ng >> modtype_base >>| fun (name, helper) ->
                    Helper.from_attrs @@ fun ~attrs -> helper ~attrs name
            in

            let modtype_str = Trace.point "modtype str" @@ fun _ ->
                Keyword.module_ >> Keyword.type_ng >>
                let%map loc_start = nongrammar >> position
                and (name, helper) = modtype_base
                in
                fun ~loc_start:_ -> Helper.mk helper ~loc_start name
            in

            let type_desc =
                type_desc
                <|>
                return
                (
                    Helper.mk Ast_helper.Type.mk
                        ?docs:None ?text:None ?params:None ?cstrs:None ?kind:None ?priv:None ?manifest:None
                )
            in

            let type_ = Trace.point "type" @@ fun _ ->
                let first =
                    Keyword.type_ >>
                    map3
                    (option Asttypes.Nonrecursive (ng >> Keyword.rec_ >>$ Asttypes.Recursive))
                    (ng >> loc Name.l_ident)
                    (type_desc)
                    ~f:(fun rec_flag name desc ->
                        fun ~loc_start ~loc_end -> (rec_flag, desc ~loc_start ~loc_end name)
                    )
                in
    
                let other =
                    map2
                    (Keyword.and_ >> nongrammar >> loc Name.l_ident)
                    (nongrammar >> type_desc)
                    ~f:(fun name desc -> desc name)
                in

                map2
                (use_loc first)
                (many @@ use other)
                ~f:(fun (rec_flag, hd) tail ->
                    Helper.from_attrs @@ fun ~attrs ->
                        (rec_flag, (hd ~attrs :: tail))
                )
            in

            let type_ext_sig = Trace.point "sig type" @@ fun _ ->
                map4
                (Keyword.type_ >> loc Name.type_name)
                (opt type_params)
                (ng >> string "+=" >> option Asttypes.Public (Keyword.private_ng >>$ Asttypes.Private))
                (both
                    (use (opt (nongrammar >> char '|') >> extension_constructor))
                    (many @@ use (nongrammar >> char '|' >> extension_constructor))
                )
                ~f:(fun name params priv (hd, tail) ->
                    Helper.from_attrs @@ fun ~attrs ->
                        Ast_helper.Te.mk ~attrs ?params ~priv name (hd::tail)
                )
            in

            let external_ = Trace.point "external" @@ fun _ ->
                Keyword.external_ >> nongrammar
                >>
                map3
                (loc Name.l_ident)
                (colon_ng >> use typexpr)
                (eq_ng >> many1 (nongrammar >> Constant.String.string))
                ~f:(fun name typ prim -> 
                    Helper.mk Ast_helper.Val.mk ?docs:None ?prim:(Some prim) name typ
                )
            in

            let module_decl = Trace.point "module decl" @@ fun _ ->
                let module_alias =
                    map
                    (loc Name.module_name)
                    ~f:(fun name ->
                        Helper.from_attrs @@ fun ~attrs -> Ast_helper.Mty.alias ~attrs name
                    )
                in
                
                map2
                (loc Name.u_ident)
                (
                    (colon_ng >> use module_type)
                    <|>
                    (eq_ng >> use module_alias)
                )
                ~f:(fun name typ ->
                    Helper.mk Ast_helper.Md.mk ?docs:None ?text:None name typ
                )
            in

            let module_ = Trace.point "module" @@ fun _ ->
                map2
                (Keyword.module_ >> nongrammar >> position)
                module_decl
                ~f:(fun loc_start mk ->
                    fun ~loc_start:_ -> mk ~loc_start
                )
            in

            let module_rec = Trace.point "module rec" @@ fun _ ->
                let first = Keyword.module_ >> nongrammar >> Keyword.rec_ >> nongrammar >> module_decl in
                let other = use (Keyword.and_ >> module_decl) in

                map3
                first
                position
                (many other)
                ~f:(fun hd hd_end tail ->
                    Helper.from_loc @@ fun ~loc ~attrs ->
                        Ast_helper.Sig.rec_module ~loc ((hd ~loc_start:loc.loc_start ~loc_end:hd_end ~attrs)::tail)
                )
            in

            let item_helper : 'a 'b.
                (?loc:Warnings.loc -> 'a -> 'b) ->
                'a Helper.t t ->
                'b Helper.t t
                =
                fun h p ->
                    let%map res = p
                    and res_loc_end = position
                    in

                    fun ~loc_start ~loc_end ~attrs ->
                        let res = res ~loc_start ~loc_end:res_loc_end ~attrs in
                        h ~loc:(make_location loc_start loc_end) res
            in

            let item_helper2 : 'a 'b 'c.
                (?loc:Warnings.loc -> 'a -> 'b -> 'c) ->
                ('a * 'b) Helper.t t ->
                'c Helper.t t
                =
                fun h p ->
                    let%map res = p
                    and res_loc_end = position
                    in

                    fun ~loc_start ~loc_end ~attrs ->
                        let (res1, res2) = res ~loc_start ~loc_end:res_loc_end ~attrs in
                        h ~loc:(make_location loc_start loc_end) res1 res2
            in

            let item_nohelper : 'a.
                'a Helper.t t -> 'a Helper.t t
                =
                fun p ->
                    item_helper (fun ?loc:_ x -> x) p
            in

            let use_item : 'a.
                'a Helper.t t -> 'a t
                =
                fun p ->
                    let%map loc_start = nongrammar >> position
                    and attrs = attrs
                    and mk = nongrammar >> p
                    and loc_end = position_delimiter
                    in
                    mk ~loc_start ~loc_end ~attrs
            in

            let signature = Trace.point "signature" @@ fun _ ->
                let attribute = item_helper Ast_helper.Sig.attribute @@ no_loc_attrs module_attribute in
                let extension = item_nohelper @@ attributed Ast_helper.Sig.extension module_extension in
                let exception_ = item_helper Ast_helper.Sig.exception_ exception_sig in
                let external_ = item_helper Ast_helper.Sig.value external_ in
                let include_ = item_helper Ast_helper.Sig.include_ include_sig in
                let let_ = item_helper Ast_helper.Sig.value value_sig in
                let modtype = item_helper Ast_helper.Sig.modtype modtype_sig in
                let module_ = item_helper Ast_helper.Sig.module_ module_ in
                let module_rec = item_nohelper module_rec in
                let open_ = item_helper Ast_helper.Sig.open_ @@ ignore_attrs_start open_ in
                let type_ = item_helper2 Ast_helper.Sig.type_ type_ in
                let type_ext = item_helper Ast_helper.Sig.type_extension type_ext_sig in

                let sig_item =
                    Trace.point ("signature item @ " ^ __LOC__) @@ fun _ ->
                    match%bind peek_char_fail with
                    | '@' -> attribute
                    | '%' -> extension
                    | 'e' -> exception_ <|> external_
                    | 'i' -> include_
                    | 'l' -> let_
                    | 'm' -> modtype <|> module_rec <|> module_
                    | 'o' -> open_
                    | 't' -> type_ext <|> type_
                    | _ -> fail "TODO"
                in
                many @@ use_item sig_item
            in

            let expression = Trace.point "expression" @@ fun _ -> fix @@ fun expression ->
                let fn = Trace.point "expression fn" @@ fun _ ->
                    let arrow_tail = arrow >> use expression in

                    let arrow = Trace.point "expression arrow" @@ fun _ ->
                        let%map args =
                            Brackets.in_round (opt dot >> sequence1 (use pattern) ~sep:comma_ng)
                            <|>
                            (let%map x = use pattern in [x])
                        and next = ng >> arrow_tail
                        in

                        fun ~loc_start ~loc_end ->
                            let [@warning "-8"] x::xs = args in

                            let res = Base.List.fold_right
                                ~f:(fun arg acc -> Ast_helper.Exp.fun_ ~loc:(make_location arg.ppat_loc.loc_start loc_end) Nolabel None arg acc)
                                ~init:next
                                xs
                            in
                            Helper.mk Ast_helper.Exp.fun_ ~loc_start ~loc_end Nolabel None x res
                    in

                    let tuple = Trace.point "expr tuple" @@ fun _ ->
                        Brackets.in_round
                        (
                            both
                            (use expression << comma_ng)
                            (sequence (use expression) ~sep:comma_ng)
                        )
                        >>| fun (hd, tail) ->
                            Helper.mk Ast_helper.Exp.tuple (hd::tail)
                    in

                    let atom = Trace.point "expr atom" @@ fun _ ->
                        attributed Ast_helper.Exp.ident (loc Name.type_name)
                        <|>
                        attributed Ast_helper.Exp.constant Constant.p
                        <|>
                        tuple
                        <|>
                        Brackets.in_round expression
                        (* <|>
                            TODO list *)
                    in

                    let apply = Trace.point "expr apply" @@ fun _ ->
                        let%map atom = use atom
                        and params =
                            let help = Name.l_ident >>| fun x -> Asttypes.Labelled x in
                            let labelled =
                                both
                                    (option Asttypes.Nolabel (tilda >> help << colon_ng))
                                    (use expression)
                            in
                            Brackets.in_round (sequence1 ~sep:comma_ng (nongrammar >> labelled))
                        in
                        Helper.mk Ast_helper.Exp.apply atom params
                    in

                    arrow <|> apply <|> atom
                in
                fn
            in

            let value_binding_first =
                map2
                (Keyword.let_ >> use pattern)
                (eq_ng >> use expression)
                ~f:(fun pattern expr ->
                    Helper.mk Ast_helper.Vb.mk ?docs:None ?text:None pattern expr
                )
            in

            let value_str = Trace.point "sig value" @@ fun _ ->
                map2
                value_binding_first
                position
                ~f:(fun first end_pos ->
                    Helper.from_loc @@ fun ~loc ~attrs ->
                        let first = first ~loc_start:loc.Location.loc_start ~loc_end:end_pos ~attrs in
                        Ast_helper.Str.value ~loc Asttypes.Nonrecursive [first]
                )
            in

            let structure = Trace.point "structure" @@ fun _ ->
                let structure_item = Trace.point "structure_item" @@ fun _ ->
                    peek_char_fail >>= function
                    | 'l' -> item_nohelper value_str
                    | 'm' -> item_helper Ast_helper.Str.modtype modtype_str
                    | 't' ->
                        item_helper Ast_helper.Str.type_extension type_ext_sig
                        <|>
                        item_helper2 Ast_helper.Str.type_ type_
                    | _ ->
                        item_nohelper @@ attributed Ast_helper.Str.eval (use expression)
                in
                many @@ use_item structure_item
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
