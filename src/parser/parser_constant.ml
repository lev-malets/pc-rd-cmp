open Basic
open Angstrom
open Angstrom.Let_syntax
open Parsetree
open Ast_helper
open Sigs

module Make (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser) (Utils: UTILS): CONSTANT = struct
    open Utils

    module Number = struct
        let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

        let value_part digit exp =
            let skip_digits = skip_while (fun c -> digit c || c = '_') in
            skip digit >> skip_digits >>
            option false (char '.' >> skip_digits >>$ true) >>= fun is_float ->
            match exp with
            | None -> return is_float
            | Some exp -> option is_float (skip exp >> opt exp_sign >> skip digit >> skip_digits >>$ true)

        let value_part_2 = value_part
            (function '0'..'1' -> true | _ -> false)
            None
        let value_part_8 = value_part
            (function '0'..'7' -> true | _ -> false)
            None
        let value_part_10 =
            Named.p "value_part_10" @@ value_part
                (function '0'..'9' -> true | _ -> false)
                (Some (function 'e' | 'E' -> true | _ -> false))
        let value_part_16 = value_part
            (function '0'..'9' | 'A'..'F' | 'a'..'f' -> true | _ -> false)
            (Some (function 'p' | 'P' -> true | _ -> false))

        let value_part =
                ((s"0b" <|> s"0B") >> value_part_2)
            <|> ((s"0o" <|> s"0O") >> value_part_8)
            <|> ((s"0x" <|> s"0X") >> value_part_16)
            <|> (s"0" >> value_part_8)
            <|> value_part_10

        let suffix_part = opt @@ satisfy (function 'g'..'z' | 'G'..'Z' -> true | _ -> false)

        let p =
            mapping begin fun (is_float, value) suffix ->
                match is_float with
                | true -> Pconst_float (value, suffix)
                | false -> Pconst_integer (value, suffix)
            end
            <*> with_literal value_part <*> suffix_part
    end

    module Character = struct
        let code offset basic c = (Char.code c - Char.code basic + offset)
        let octal_code = satisfy (function '0'..'7' -> true | _ -> false) >>| code 0 '0'
        let decimal_code = satisfy (function '0'..'9' -> true | _ -> false) >>| code 0 '0'
        let hexadecimal_code =
                decimal_code
            <|> (satisfy (function 'a'..'f' -> true | _ -> false) >>| code 10 'a')
            <|> (satisfy (function 'A'..'F' -> true | _ -> false) >>| code 10 'A')

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
                ~f:Pervasives.(fun _1 _2 -> Char.chr @@ _1 * 16 + _2)
            | '0' .. '2' as ch ->
                let _1 = code 0 '0' ch in
                map2 decimal_code decimal_code
                ~f:Pervasives.(fun _2 _3 -> Char.chr @@ _1 * 100 + _2 * 10 + _3)
            | 'o' ->
                map3
                (satisfy (function '0'..'3' -> true | _ -> false) >>| code 0 '0')
                octal_code
                octal_code
                ~f:Pervasives.(fun _1 _2 _3 -> Char.chr @@ _1 * 64 + _2 * 8 + _3)
            | _ -> fail "TODO"

        let p = s"\'" >>
            ((s"\\" >> escaped) <|> satisfy begin function
                | '\\' | '\"' | '\'' | '\n' | '\t' | '\b' | '\r' -> false
                | _ -> true
                end
            )
            << s"\'"
        let p = p >>| Const.char
    end

    module String = struct
        let string =
            s"\"" >>= fun _ ->
            let buf = Buffer.create 16 in
            fix @@ fun p ->

            let not_escaped = function
                | '\\' | '\"' | '\n' | '\t' | '\b' | '\r' -> false
                | _ -> true
            in

            (take_while not_escaped >>| Buffer.add_string buf) >>
            (
                (s"\\" >> (Character.escaped >>| Buffer.add_char buf) >> p)
                <|>
                (s"\"" >>| fun _ -> Buffer.contents buf)
            )

        let p =
            (string >>| Const.string ~quotation_delimiter:"js")
    end

    let p =
        match%bind peek_char_fail with
        | '0'..'9' | '-' | '.' -> Number.p
        | '\'' -> Character.p
        | '\"' -> String.p
        | _ -> fail "TODO"
end
