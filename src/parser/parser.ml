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

let opt p = option None (p >>| Option.some)

let sequence ?(sep = return ()) p =
    let p1 = sep >> p in
    match%bind opt p with
    | None -> return []
    | Some first -> let%map tail = many p1 in first::tail
let sequence1 ?(sep = return ()) p =
    let p1 = sep >> p in
    let%map first = p
    and others = many p1 in
    first :: others

let upper = function 'A' .. 'Z' -> true | _ -> false
let lower = function 'a' .. 'z' -> true | _ -> false

let make_location loc_start loc_end = Location.{loc_start; loc_end; loc_ghost = false}
let comb_location loc1 loc2 = make_location loc1.Location.loc_start loc2.Location.loc_end

let with_location p =
    let%map loc_start = position
    and res = p
    and loc_end = end_position
    in
    (res, make_location loc_start loc_end)
let loc p = let%map (x, loc) = with_location p in Location.mkloc x loc

let check p = option false (p >>$ true)

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
    whitespace << comments
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

    let delimiter_ = Trace.point __LOC__ @@ fun _ -> semicolon_ <|> newline_skipped <|> end_of_input
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
                let category = Res_diagnostics.message msg in
                let%bind pos = position in
                Diagnostics.diagnostic pos pos category >> advance 1 >>$ Some 'n'
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
            skip_while (Fn.compose not Character.only_escaped) >> any_char >>= function
            | '\\' -> Character.escaped >> p
            | '\"' -> return ()
            | c -> Trace.point ("unexpected symbol: " ^ String.make 1 c) @@ fun _ -> fail "TODO"
        let p : Parsetree.constant Parser.t = Trace.point "string" @@ fun _ ->
            let%map literal = consumed (char '\"' >> p) in
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
        (take_while1 Keyword.identifier's_character << whitespace)
    let c_ident first = consumed (skip first >> skip_while Keyword.identifier's_character) << whitespace

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
    let type_name = Trace.point __LOC__ @@ fun _ -> longident upper lower
    let vartype_name = c_ident ((=) '\'')
end

module Tree = struct
    open Parsetree

    let not_fixed signature structure attribute =
        let push_attributes = Trace.point ("push attributes: " ^ __LOC__) @@ fun _ ->
            let%bind pos = position
            and attrs = many attribute
            in
            map_state @@ fun s -> {s with attrs = (pos, attrs) :: s.attrs}
        in

        let loc_attrs = Trace.point ("pop attributes: " ^ __LOC__) @@ fun _ ->
            let%bind state = get_state in
            match state.attrs with
            | [] -> fail "TODO"
            | (pos, attrs) :: tail ->
                let%bind end_pos = end_position in
                set_state {state with attrs = tail} >>$ (make_location pos end_pos, attrs)
        in

        let mk_loc_attrs : 'a. (?loc:Warnings.loc -> ?attrs:attributes -> 'a) -> 'a t =
            fun f ->
                let%map (loc, attrs) = loc_attrs in
                f ~loc ~attrs
        in

        let attributed: 'a 'b. (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) -> 'a t -> 'b t = fun f p ->
            let%map res = p
            and (loc, attrs) = loc_attrs
            in
            f ~loc ~attrs res
        in

        let attributed2: 'a 'b 'c. (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t = fun f p1 p2 ->
            let%map res1 = p1
            and res2 = p2
            and (loc, attrs) = loc_attrs
            in
            f ~loc ~attrs res1 res2
        in

        let _located: 'a 'b. (?loc:Warnings.loc -> 'a -> 'b) -> 'a t -> 'b t = fun f p ->
            let%map (res, loc) = with_location p in
            f ~loc res
        in

        let _located2: 'a 'b 'c. (?loc:Warnings.loc -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t = fun f p1 p2 ->
            let%map ((res1, res2), loc) = with_location @@ both p1 p2 in
            f ~loc res1 res2
        in

        let _labelled p =
            let label =
                match%bind peek_char_fail with
                | '~' -> advance 1 >> whitespace >> (let%map label = Name.ident in Asttypes.Labelled label) << colon_
                | '?' -> advance 1 >> whitespace >> (let%map label = Name.ident in Asttypes.Optional label) << colon_
                | _ -> return Asttypes.Nolabel
            in
            both p label
        in

        let typexpr = Trace.point __LOC__ @@ fun _ -> fix @@ fun typexpr ->
            let atom = Trace.point __LOC__ @@ fun _ ->
                let%bind () = push_attributes in

                (attributed Ast_helper.Typ.var Name.vartype_name)
                <|>
                (attributed Ast_helper.Typ.any @@ char_ '_')
                <|>
                (
                    let args = Trace.point __LOC__ @@ fun _ ->
                        Brackets.in_chevrons_ (sequence1 typexpr ~sep:comma_ << opt comma_) <|> return []
                    in
                    attributed2 Ast_helper.Typ.constr (loc Name.type_name) args
                )
            in

            let tuple_to_type loc =
                function
                | [] -> failwith "unreachable"
                | [x] -> {x with ptyp_loc = loc}
                | xs -> Ast_helper.Typ.tuple ~loc xs
            in

            let tuple =
                Brackets.in_parentheses_ (sequence1 typexpr ~sep:comma_)
                <|>
                (let%map x = push_attributes >> atom in [x])
            in

            let arrow_tail = string_ "=>" >> typexpr in

            let try_arrow tuple =
                (
                    let%map next = arrow_tail
                    and (loc, attrs) = loc_attrs
                    in
                    let typ = List.fold_right
                        ~f:(fun arg acc -> Ast_helper.Typ.arrow ~loc:(comb_location arg.ptyp_loc loc) Nolabel arg acc)
                        ~init:next
                        tuple
                    in
                    {typ with ptyp_attributes = attrs; ptyp_loc = loc}
                )
                <|>
                (
                    match tuple with
                    | [] -> failwith "unreachable ?"
                    | [x] ->
                        let%map (loc, attrs) = loc_attrs in
                        { x with
                            ptyp_attributes = x.ptyp_attributes @ attrs;
                            ptyp_loc = loc;
                        }
                    | xs ->
                        let%map mk = mk_loc_attrs Ast_helper.Typ.tuple in
                        mk xs
                )
            in

            let arrow =
                let%bind pos = position
                and (tuple, loc) = with_location tuple in
                let%bind tuple = match%bind check @@ string_ "as" with
                    | true ->
                        let%map alias = Name.vartype_name
                        and end_pos = end_position
                        in
                        [Ast_helper.Typ.alias ~loc:(make_location pos end_pos) (tuple_to_type loc tuple) alias]
                    | false -> return tuple
                in
                try_arrow tuple
            in

            let poly = Trace.point ("poly " ^ __LOC__) @@ fun _ ->
                let%bind pos = position
                and vars = many @@ loc Name.vartype_name
                in
                match vars with
                | [] -> arrow
                | xs ->
                    match%bind peek_char_fail with
                    | '.' ->
                        let%map typ = dot_ >> typexpr in
                        Ast_helper.Typ.poly ~loc:(make_location pos typ.ptyp_loc.loc_end) vars typ
                    | _ ->
                        let%map tail = arrow_tail in
                        let arg = List.hd_exn xs in
                        let arg = Ast_helper.Typ.var ~loc:arg.loc arg.txt in
                        Ast_helper.Typ.arrow ~loc:(make_location pos tail.ptyp_loc.loc_end) Nolabel arg tail
            in

            poly
        in

        let pattern = Trace.point ("pattern: " ^ __LOC__) @@ fun _ -> fix @@ fun pattern ->
            let with_constr =
                let%map pattern = pattern
                and typ = opt (colon_ >> typexpr)
                in
                match typ with
                | Some typ -> Ast_helper.Pat.constraint_ ~loc:{pattern.ppat_loc with loc_end = typ.ptyp_loc.loc_end} pattern typ
                | None -> pattern
            in
            let tuple =
                let%map (list, loc) = with_location @@ Brackets.in_parentheses_ @@ sequence1 ~sep:comma_ with_constr in
                match list with
                | [] -> failwith "unreachable"
                | [x] -> {x with ppat_loc = loc}
                | _ -> Ast_helper.Pat.tuple ~loc list
            in

            let array =
                Brackets.in_brackets_ @@ sequence1 ~sep:comma_ with_constr
            in

            let atom = Trace.point "pattern atom" @@ fun _ ->
                let%bind _ = push_attributes in

                (attributed Ast_helper.Pat.any (char_ '_'))
                <|>
                (attributed Ast_helper.Pat.var (loc Name.ident))
                <|>
                (attributed2 Ast_helper.Pat.interval (Constant.Character.p << string_ "..") Constant.Character.p)
                <|>
                (attributed Ast_helper.Pat.constant Constant.p)
                <|>
                tuple
                (* <|>
                    TODO list *)
                <|>
                (attributed Ast_helper.Pat.array array)
            in

            atom
        in

        let attrubute_id = take_while (fun x -> Keyword.identifier's_character x || x = '.') << whitespace in
        let payload =
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
        in

        let attribute = Trace.point ("attribute: " ^ __LOC__) @@ fun _ ->
            id_payload_pair at
        in

        let _extension = Trace.point ("extension: " ^ __LOC__) @@ fun _ ->
            id_payload_pair percent
        in

        let module_extension = Trace.point ("module extension @ " ^ __LOC__) @@ fun _ ->
            id_payload_pair @@ string_ "%%"
        in

        let open_ = Trace.point "open" @@ fun _ ->
            let override = option Asttypes.Fresh (bang_ >>$ Asttypes.Override) in

            let%bind override = Keyword.open_ >> override in
            attributed (Ast_helper.Opn.mk ~override ?docs:None) @@ loc Name.module_name
        in

        let module_type = fix @@ fun module_type ->
            let functor_arg =
                let%map par_name = loc (Name.u_ident <|> p_ (string "_"))
                and par_type = match%bind check (char_ ':') with
                    | true -> module_type >>| Option.some
                    | false -> return None
                in
                (par_name, par_type)
            in

            let with_functor_args = fix @@ fun with_functor_args ->
                let mt =
                    match%bind p_ any_char with
                    | ',' -> with_functor_args
                    | ')' -> string_ "=>" >> module_type
                    | _ -> fail "TODO"
                in
                let%bind (par_name, par_type) = functor_arg in
                attributed (fun ?loc ?attrs mt -> Ast_helper.Mty.functor_ ?loc ?attrs par_name par_type mt) mt
            in

            let desc =
                let module Help = Ast_helper.Mty in

                (attributed Help.ident @@ loc Name.module_name)
                <|>
                (attributed Help.alias @@ Brackets.in_parentheses_ @@ loc Name.module_name)
                <|>
                (attributed Help.signature @@ Brackets.in_braces_ signature)
                <|>
                with_functor_args
            in

            push_attributes >> desc
        in

        let constr_args =
            begin
                Brackets.in_parentheses_ (sequence1 ~sep:comma_ typexpr << opt comma_)
                    >>| fun x -> Parsetree.Pcstr_tuple x
            end
            (* TODO
            | Pcstr_record of label_declaration list
            *)
        in

        let exception_sig =
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
                    let%map typ = typexpr in
                    Parsetree.Pext_decl (Pcstr_tuple [], Some typ)
                | _ -> return @@ Parsetree.Pext_decl (Pcstr_tuple [], None)
            in
            push_attributes >> Keyword.exception_ >> begin
                let%map name = loc Name.u_ident
                and kind = kind
                and (loc, attrs) = loc_attrs
                in
                Ast_helper.Te.constructor ~loc ~attrs name kind
            end
        in

        let include_sig = attributed (Ast_helper.Incl.mk ?docs:None) (Keyword.include_ >> module_type) in

        let value_sig = Trace.point __LOC__ @@ fun _ ->
            let%map _ = Keyword.let_
            and name = loc Name.l_ident
            and _ = colon_
            and typ = typexpr
            in
            Ast_helper.Val.mk ~loc:(comb_location name.loc typ.ptyp_loc) name typ
        in

        let modtype = Trace.point "modtype" @@ fun _ ->
            let%map name = Keyword.module_ >> Keyword.type_ >> loc Name.ident
            and mt = opt (char_ '=' >> module_type)
            and (loc, attrs) = loc_attrs
            in

            let mk = Ast_helper.Mtd.mk ~loc ~attrs in
            let mk = match mt with
                | None -> mk ?typ:None
                | Some typ -> mk ~typ
            in
            mk name
        in

        let signature =
            let open_ = open_ >>| fun desc -> Ast_helper.Sig.open_ ~loc:desc.popen_loc desc in
            let include_ = include_sig >>| fun infos -> Ast_helper.Sig.include_ ~loc:infos.pincl_loc infos in
            let let_ =
                let%map (value, loc) = with_location (value_sig << delimiter_) in
                Ast_helper.Sig.value ~loc {value with pval_loc = loc} (* just for compatibility *)
            in
            let extension = attributed Ast_helper.Sig.extension module_extension in
            let exception_ = exception_sig >>| fun x -> Ast_helper.Sig.exception_ ~loc:x.pext_loc x in

            let sig_item = Trace.point ("signature item @ " ^ __LOC__) @@ fun _ -> push_attributes >> match%bind peek_char_fail with
                | 'm' ->
                    let%map mt = modtype in
                    Ast_helper.Sig.modtype ~loc:mt.pmtd_loc mt
                | 'o' -> open_
                | 'i' -> include_
                | 'l' -> let_
                | '%' -> extension
                | 'e' -> exception_
                | _ -> fail "TODO"
            in
            many sig_item
        in

        let expression = fix @@ fun expression ->
            let fn =
                let arrow_tail = string_ "=>" >> expression in

                let arrow =
                    let%map args =
                        Brackets.in_parentheses_ (sequence1 (both pattern position) ~sep:comma_)
                        <|>
                        (let%map x = both pattern position in [x])
                    and next = arrow_tail
                    and end_pos = end_position
                    in

                    List.fold_right
                        ~f:(fun (arg, _pos) acc -> Ast_helper.Exp.fun_ ~loc:(make_location arg.ppat_loc.loc_start end_pos) Nolabel None arg acc)
                        ~init:next
                        args
                in

                let tuple = Trace.point "expr tuple" @@ fun _ ->
                    let%map list = Brackets.in_parentheses_ (sequence expression ~sep:comma_)
                    and (loc, attrs) = loc_attrs
                    in
                    match list with
                    | [] -> failwith "unimplemented"
                    | [x] -> {x with pexp_loc = loc; pexp_attributes = attrs @ x.pexp_attributes}
                    | list -> Ast_helper.Exp.tuple ~loc ~attrs list
                in

                let atom = Trace.point "expr atom" @@ fun _ ->
                    (attributed Ast_helper.Exp.ident (loc Name.type_name))
                    <|>
                    (attributed Ast_helper.Exp.constant Constant.p)
                    <|>
                    tuple
                    (* <|>
                        TODO list *)
                in
                let atom = push_attributes >> atom in

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
                attributed Ast_helper.Str.eval expression
            in
            let structure_item = Trace.point "structure_item" @@ fun _ ->
                push_attributes >> expression
            in
            many structure_item
        in

        (signature, structure, attribute)


    let (signature, structure) =
        let rec ps = lazy (not_fixed sig_ str_ attr_)
        and sig_ = { run = fun input -> let (p, _, _) = Lazy.force ps in p.run input }
        and str_ = { run = fun input -> let (_, p, _) = Lazy.force ps in p.run input }
        and attr_ = { run = fun input -> let (_, _, p) = Lazy.force ps in p.run input }
        in
        (sig_, str_)
end

let with_print p =
    let%map res = p
    and state = get_state
    in
    Res_diagnostics.printReport state.diagnostics "TODO";
    res

let parse p state filename =
    let in_c = In_channel.create filename in
    let text = In_channel.input_all in_c in
    let () = In_channel.close in_c in
    parse_string p state ~filename text

let parse_interface = parse (with_print Tree.signature) State.default
let parse_implementation = parse (with_print Tree.structure) State.default
