
module MakePeek (Angstrom : Sigs.POS): Sigs.PEEK with module Parser = Angstrom.Parser = struct
    module Parser = Angstrom.Parser
    open Angstrom

    let char_fail f ~expected:_ = peek_char_fail >>= f
end

module MakeNotPeek (Angstrom : Sigs.POS): Sigs.PEEK with module Parser = Angstrom.Parser = struct
    module Parser = Angstrom.Parser
    open Angstrom

    let char_fail f ~expected =
        List.fold_right (fun c p -> (f c) <|> p) expected (f @@ Char.chr 0)
end
