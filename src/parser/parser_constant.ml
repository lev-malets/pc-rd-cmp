open Base
open Basic
open APos
open Parsetree
open Ast_helper
open Sigs

module Make (Ext: EXT) (Utils: UTILS): CONSTANT = struct
    open Ext
    open Utils

    module Number = struct
        let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

        let value_part digit exp =
            let skip_digits = skip_while (fun c -> digit c || Char.equal c '_') in

            let int = skip digit >> skip_digits in
            let float = int >> char '.' >> skip_digits in

            match exp with
            | None -> (float >>$ true) <|> (int >>$ false)
            | Some exp ->
                let exp = skip exp >> opt exp_sign >> skip digit >> skip_digits in
                    (float >> exp >>$ true)
                <|> (float >>$ true)
                <|> (int >> exp >>$ true)
                <|> (int >>$ false)

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
                s"0" >> Peek.first
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

        let p =
            Named.p "const:number" begin
                mapping begin fun (is_float, value) suffix ->
                    match is_float with
                    | true -> Pconst_float (value, suffix)
                    | false -> Pconst_integer (value, suffix)
                end
                <*> with_literal value_part <*> suffix_part
            end
    end

    module Character = struct
        let code offset basic c = (Char.to_int c - Char.to_int basic + offset)
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
                mapping (fun _1 _2 -> Char.of_int_exn @@ _1 * 16 + _2)
                << s"x" <*> hexadecimal_code <*> hexadecimal_code
            in

            let dec_cc =
                mapping (fun _1 _2 _3 -> Char.of_int_exn @@ _1 * 100 + _2 * 10 + _3)
                <*> (satisfy (function '0'..'2' -> true | _ -> false) >>| code 0 '0')
                <*> decimal_code <*> decimal_code
            in

            let oct_cc =
                mapping (fun _1 _2 _3 -> Char.of_int_exn @@ _1 * 64 + _2 * 8 + _3)
                << s"o" <*> (satisfy (function '0'..'3' -> true | _ -> false) >>| code 0 '0')
                <*> octal_code <*> octal_code
            in

            Peek.first
            [ char '\\'; char '\'' ; char ' '
            ; s"n" >>$ '\n'; s"t" >>$ '\t' ; s"b" >>$ '\b'; s"r" >>$ '\r'
            ; hex_cc; dec_cc; oct_cc
            ]

        let p =
            Named.p "const:char" begin
                s"\'" >>
                ((s"\\" >> escaped) <|> satisfy begin function
                    | '\\' | '\'' | '\n' | '\t' | '\b' | '\r' -> false
                    | _ -> true
                    end
                )
                << s"\'"
            end
        let p = p >>| Const.char
    end

    module String = struct
        let string =
            let buf = mk_bufs () in

            s"\"" >> wrapped buf.mk buf.drop
            begin
                fix @@ fun p ->
                    let not_escaped = function
                        | '\\' | '\"' | '\n' | '\t' | '\b' | '\r' -> false
                        | _ -> true
                    in

                    (take_while not_escaped >>| buf.add_string) >>
                    (
                            (string "\\" >> (Character.escaped >>| buf.add_char) >> p)
                        <|> ((string "\\\"" >>| fun _ -> buf.add_char '\"') >> p)
                        <|> (string "\"" >> buf.contents)
                    )
            end

        let multiline ~q =
            let buf = mk_bufs () in
            let mk_const str =
                Const.string ~quotation_delimiter:"js" str
            in

            wrapped buf.mk buf.drop
            (
                char q >>
                fix @@ fun loop ->
                (take_while (function '\n' | '\\' | '\r' -> false | c -> not (Char.equal c q)) >>| buf.add_string)
                >>
                (
                        (char q >> buf.contents >>| mk_const)
                    <|> ((new_line >>| buf.add_string) >> loop)
                    <|> ((s"\\\"" >>| fun _ -> buf.add_string "\\\"") >> loop)
                    <|> ((s"\\\\" >>| fun _ -> buf.add_string "\\\\") >> loop)
                    <|> ((s"\\" >>| fun _ -> buf.add_char '\\') >> loop)
                )
            )

        let p =
            Named.p "const:string" begin
                string >>| Const.string ~quotation_delimiter:"js"
            end
    end

    let p =
        Peek.first
        [ Number.p
        ; Character.p
        ; String.p
        ]
end
