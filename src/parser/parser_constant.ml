open Base
open Parsetree
open Ast_helper
open Sigs

module Make (APos: APOS) = struct
    open APos

    let exp_sign = skip @@ function '-' | '+' -> true | _ -> false

    let value_part digit exp =
        let skip_digits = skip_while (fun c -> Caml.(||) (digit c) (Char.equal c '_')) in

        let int = skip digit >> skip_digits in
        let float = int >> s"." >> skip_digits in

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
                    +(new_line <|> s"\\\"" <|> s"\\\\" <|> s"\\")
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
end
