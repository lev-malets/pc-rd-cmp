open Base
open Parsetree
open Ast_helper


module type APOS = Angstrom_pos.S with type s = Pc_syntax.Basic.LogElement.t

let identifier's_character = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
    | _ -> false

module Make (APos : APOS) = struct
    module Base = struct
        module Comb = APos
        open Comb
        open Pc_syntax.Token

        let k x = s x >> failed (satisfy identifier's_character)

        let and' = k"and" >>$ And
        let as' = k"as" >>$ As
        let mutable' = k"mutable" >>$ Mutable
        let constraint' = k"constraint" >>$ Constraint
        let private' = k"private" >>$ Private
        let unpack = k"unpack" >>$ Unpack
        let export = k"export" >>$ Export
        let external' = k"external" >>$ External
        let import = k"import" >>$ Import
        let from = k"from" >>$ From
        let let' = k"let" >>$ Let
        let module' = k"module" >>$ Module
        let with' = k"with" >>$ With
        let open' = k"open" >>$ Open
        let exception' = k"exception" >>$Exception
        let switch = k"switch" >>$Switch
        let try' = k"try" >>$Try
        let catch = k"catch" >>$Catch
        let else' = k"else" >>$Else
        let to' = k"to" >>$To
        let downto' = k"downto" >>$Downto
        let for' = k"for" >>$For
        let of' = k"of" >>$ Of
        let in' = k"in" >>$ In
        let if' = k"if" >>$ If
        let json_tag = k"json" >>$ Json
        let while' = k"while" >>$ While
        let assert' = k"assert" >>$ Assert
        let lazy' = k"lazy" >>$ Lazy
        let true' = k"true" >>$ True
        let type' = k"type" >>$ Type
        let false' = k"false" >>$ False
        let sig' = k"sig" >>$ Sig
        let include' = k"include" >>$ Include
        let rec' = k"rec" >>$ Rec
        let nonrec' = k"nonrec" >>$ Nonrec
        let when' = k"when" >>$ When

        let _' = k"_" >>$ Underscore

        let ampersand = s"&" >>$ Ampersand
        let ampersand_ampersand = s"&&" >>$ AmpersandAmptersand
        let arrow = s"=>" >>$ Arrow
        let asterisk = s"*" >>$ Asterisk
        let asterisk_asterisk = s"**" >>$ AsteriskAsterisk
        let asterisk_dot = s"*." >>$ AsteriskDot
        let at = s"@" >>$ At
        let at_at = s"@@" >>$ AtAt
        let bang = s"!" >>$ Bang
        let bang_eq = s"!=" >>$ BangEq
        let bang_eq_eq = s"!==" >>$ BangEqEq
        let colon = s":" >>$ Colon
        let colon_eq = s":=" >>$ ColonEq
        let colon_gt = s":>" >>$ ColonGt
        let comma = s"," >>$ Comma
        let dot = s"." >>$ Dot
        let dot_dot = s".." >>$ DotDot
        let ellipsis = s"..." >>$ Ellipsis
        let eq = s"=" >>$ Eq
        let eq_eq = s"==" >>$ EqEq
        let eq_eq_eq = s"===" >>$ EqEqEq
        let eq_op = failed arrow >> s"=" >>$ Eq
        let gt = s">" >>$ Gt
        let gt_eq = s">=" >>$ GtEq
        let hash = s"#" >>$ Hash
        let hash_eq = s"#=" >>$ HashEq
        let l_brace = s"{" >>$ LBrace
        let l_bracket = s"[" >>$ LBracket
        let l_paren = s"(" >>$ LParen
        let list = s"list{" >>$ List
        let lt = s"<" >>$ Lt
        let lt_eq = s"<=" >>$ LtEq
        let minus = s"-" >>$ Minus
        let minus_dot = s"-." >>$ MinusDot
        let minus_gt = s"->" >>$ MinusGt
        let percent = s"%" >>$ Percent
        let percent_percent = s"%%" >>$ PercentPercent
        let pipe = s"|" >>$ Pipe
        let pipe_gt = s"|>" >>$ PipeGt
        let pipe_pipe = s"||" >>$ PipePipe
        let plus = s"+" >>$ Plus
        let plus_dot = s"+." >>$ PlusDot
        let plus_eq = s"+=" >>$ PlusEq
        let plus_plus = s"++" >>$ PlusPlus
        let question = s"?" >>$ Question
        let r_brace = s"}" >>$ RBrace
        let r_bracket = s"]" >>$ RBracket
        let r_paren = s")" >>$ RParen
        let slash = s"/" >>$ Slash
        let slash_dot = s"/." >>$ SlashDot
        let tilda = s"~" >>$ Tilda

        let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

        let value_part digit exp =
            let skip_digits = skip_while (fun c -> Caml.(||) (digit c) (Char.equal c '_')) in

            let int = skip digit >> skip_digits in
            let float = int >> dot >> skip_digits in

            match exp with
            | None -> (float >>$ true) <|> (int >>$ false)
            | Some exp ->
                let exp = skip exp >> opt exp_sign >> skip digit >> skip_digits in

                    float >> exp >>$ true
                ||  float >>$ true
                ||  int >> exp >>$ true
                ||  int >>$ false

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
            let nondec =
                s"0" >> peek_first
                    [ s"b" >> value_part_2
                    ; s"B" >> value_part_2
                    ; s"o" >> value_part_8
                    ; s"O" >> value_part_8
                    ; s"x" >> value_part_16
                    ; s"X" >> value_part_16
                    ; value_part_8
                    ]
            in
                nondec
            <|> value_part_10

        let suffix_part = opt @@ satisfy (function 'g'..'z' | 'G'..'Z' -> true | _ -> false)

        let number =
            memo & named "const:number" &
            mapping begin fun (is_float, value) suffix ->
                match is_float with
                | true -> Pconst_float (value, suffix)
                | false -> Pconst_integer (value, suffix)
            end
            +with_literal(value_part) +suffix_part

        let code offset basic c = Int.(Char.to_int c - Char.to_int basic + offset)
        let octal_code =
            satisfy (function '0'..'7' -> true | _ -> false) >>| code 0 '0'
        let decimal_code =
            satisfy (function '0'..'9' -> true | _ -> false) >>| code 0 '0'
        let hexadecimal_code =
                decimal_code
            <|> (satisfy (function 'a'..'f' -> true | _ -> false) >>| code 10 'a')
            <|> (satisfy (function 'A'..'F' -> true | _ -> false) >>| code 10 'A')

        let escaped =
            let hex_cc =
                mapping Int.(fun _1 _2 -> Char.of_int_exn @@ _1 * 16 + _2)
                -s"x" +hexadecimal_code +hexadecimal_code
            in

            let dec_cc =
                mapping Int.(fun _1 _2 _3 -> Char.of_int_exn @@ _1 * 100 + _2 * 10 + _3)
                +(satisfy (function '0'..'2' -> true | _ -> false) >>| code 0 '0')
                +decimal_code +decimal_code
            in

            let oct_cc =
                mapping Int.(fun _1 _2 _3 -> Char.of_int_exn @@ _1 * 64 + _2 * 8 + _3)
                -s"o" +(satisfy (function '0'..'3' -> true | _ -> false) >>| code 0 '0')
                +octal_code +octal_code
            in

            peek_first
            [ s"\\" >>$ '\\'; s"\'" >>$ '\''; s" " >>$ ' '
            ; s"n" >>$ '\n'; s"t" >>$ '\t' ; s"b" >>$ '\b'; s"r" >>$ '\r'
            ; hex_cc; dec_cc; oct_cc
            ]

        let p =
            named "const:char" begin
                s"\'" >>
                ((s"\\" >> escaped) <|> satisfy begin function
                    | '\\' | '\'' | '\n' | '\t' | '\b' | '\r' -> false
                    | _ -> true
                    end
                )
                << s"\'"
            end
        let character = p >>| Const.char

        let string_raw =
            let parts =
            s"\"" >>
            fix @@ fun loop ->
                let not_escaped = function
                    | '\\' | '\"' | '\n' | '\t' | '\b' | '\r' -> false
                    | _ -> true
                in

                cons
                +take_while not_escaped
                +(
                        s"\"" >>$ []
                    ||  cons
                        +(
                                s"\\" >> escaped >>| String.make 1
                            ||  s"\\\"" >>$ "\""
                        )
                        +loop
                )
            in
            parts >>| String.concat

        let string_multiline ~q =
            let list =
                s q >>
                fix @@ fun loop ->
                cons
                +take_while (function '\n' | '\\' | '\r' -> false | c -> not (Char.equal c q.[0]))
                +(
                        s q >>$ []
                    ||  cons
                        +(new_line <|> s ("\\" ^ q) <|> s"\\\\" <|> s"\\")
                        +loop
                )
            in
            list >>| fun l -> Const.string ~quotation_delimiter:"js" @@ String.concat l

        let string =
            named "const:string" &
            string_raw >>| Const.string ~quotation_delimiter:"js"

        let constant =
            peek_first
            [ number
            ; character
            ; string
            ]

        let upper = function 'A' .. 'Z' -> true | _ -> false
        let lower = function 'a' .. 'z' | '_' -> true | _ -> false

        let ident = named "ident" @@
            take_while1 identifier's_character
        let c_ident first = consumed (skip first >> skip_while identifier's_character)

        let l_ident = named "l_ident" @@ (failed @@ k"_") >> c_ident lower
        let u_ident = named "u_ident" @@ c_ident upper

        let type_var = s"\'" >> ident
        let integer =
            run & mapping begin fun n ->
                let open Simple in
                match n with
                | Pconst_integer (n, s) -> return (n, s)
                | _ -> fail
            end
            +number


        let single_line_comment =
            let p = s"//" >> loc @@ take_while (function '\n' | '\r' -> false | _ -> true) in
            p >>| fun {txt; loc} -> Res_comment.makeSingleLineComment ~loc txt

        let multi_line_comment =
            let parts =
                fix @@ fun parts ->

                cons
                +take_while (function '*' | '\n' | '/' | '\r' -> false | _ -> true)
                +(
                        s"*/" >>$ []
                    ||  (cons +new_line +parts)
                    ||  (mapping (fun l tail -> "/*" :: (l @ "*/" :: tail)) -s"/*" +parts +parts)
                    ||  (cons +(any_char >>| String.make 1) +parts)
                )
            in
            let p = s"/*" >> parts in
            loc (p >>| String.concat)
            >>|
            fun {txt; loc} -> Res_comment.makeMultiLineComment ~loc txt

        let comment =
            single_line_comment <|> multi_line_comment
            >>|
            fun x pos comments -> Pc_syntax.Basic.LogElement.Comment (Res_comment.setPrevTokEndPos x pos; x) :: comments

        let comments =
            fold_left_0_n
                (t2 +comment +pos -whitespace)
                (t2 +comment +pos -whitespace)
                ~f:begin fun (f1, p1) (f2, p2) ->
                    (fun pos comments -> f2 p1 @@ f1 pos comments), p2
                end
        let ng =
            memo & named "nongrammar" &

            let p = t2 +pos -whitespace +opt(comments) in
            let p2 =
                p >>= fun (pos, x) ->
                    match x with
                    | Some (hlp, _) -> Simple.log_many (hlp pos [])
                    | _ -> Simple.return ()
            in
            {p2 with info = p.info}

        let del_pos =
            (t2 +pos -ng +peek_char)
            >>=
            fun (p1, c) ->
                let open Angstrom in
                let open Simple in
                match c with
                | Some '}' -> return p1
                | Some ')' -> return p1
                | Some ';' -> advance 1 >> APos.pos.p
                | None -> return p1
                | _ ->
                    APos.pos.p >>= fun p2 ->
                    match p1.pos_lnum = p2.pos_lnum with
                    | true -> fail
                    | false -> return p1

        let del =
            (t2 +pos -ng +peek_char)
            >>=
            fun (p1, c) ->
                let open Angstrom in
                let open Simple in
                match c with
                | Some '}' -> return ()
                | Some ')' -> return ()
                | Some ';' -> advance 1 >>$ ()
                | None -> return ()
                | _ ->
                    APos.pos.p >>= fun p2 ->
                    match p1.pos_lnum = p2.pos_lnum with
                    | true -> fail
                    | false -> return ()

        let interpolated_string ~quote_tag ~expression =
            let open Pc_syntax.Basic in
            let open Parsetree in
            let open Ast_helper in
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
                    ||  cons +(s"$" << failed l_brace) +parts
                    ||  return []
                )
            in
            let string =
                mapping begin fun l p1 p2 ->
                    let str = String.concat l in
                    Exp.constant ~loc:(make_location p1 p2) ~attrs:[Hc.attr "res.template"] @@
                    Const.string ~quotation_delimiter:quote_tag str
                end
                +parts
            in

            let string_part =
                fix @@ fun string_part ->

                let tail =
                    mapping begin fun expr pos tail prev ->
                        tail (Exp.apply ~loc:(make_location prev.pexp_loc.loc_start pos) op [Nolabel, prev; Nolabel, expr])
                    end
                    -s"${" -ng +expression -ng -r_brace +pos +string_part
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
                            +pos -s"${" -ng +expression -ng -r_brace +pos +string_part
                    )
                )
            )
        let string_ident = s"\\" >> string_raw
    end

    include Pc_syntax.Parser_basic.Make(Base)
end
