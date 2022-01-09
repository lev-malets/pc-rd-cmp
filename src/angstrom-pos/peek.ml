
module MakePeek (Pos : Sigs.POS): Sigs.PEEK with module Parser = Pos.Parser = struct
    module Parser = Pos.Parser

    let first expected =
        let open Pos in
        let open Parser in
        let open Pos.Angstrom in

        let arr = Array.make 256 (fail "") in
        let first = ref Charset.empty in

        let rec loop =
            function
            | [] -> ()
            | p::xs ->
                let p_first =
                    match p.info with
                    | Consume {empty=false; first} -> first
                    | _ -> failwith ""
                in

                first := Charset.union !first p_first;

                loop xs;

                p_first |> Charset.iter_code
                    begin fun c ->
                        arr.(c) <- p.p <|> arr.(c)
                    end
        in
        loop expected;

        { p = (peek_char_fail >>= fun c -> arr.(Char.code c))
        ; info =
            Consume
            { empty = false
            ; first = !first
            }
        ; typ = Parser
        }
end

module MakeNotPeek (Pos : Sigs.POS): Sigs.PEEK with module Parser = Pos.Parser = struct
    module Parser = Pos.Parser
    open Pos

    let first expected =
        List.fold_right (fun c p -> c <|> p) expected (fail "")
end
