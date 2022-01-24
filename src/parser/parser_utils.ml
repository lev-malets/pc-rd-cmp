open Basic
open APos
open Core_kernel
open Sigs

module Make (Named: Angstrom_pos.Sigs.NAMED with module Parser = Basic.APos.Parser): UTILS = struct
    let s =
        Fix.Memoize.String.memoize @@ fun x ->
        Named.p ("\'" ^ x ^ "\'") begin
            begin
                match String.length x with
                | 0 -> return ""
                | 1 -> char x.[0] >>$ x
                | _ -> string x
            end
        end

    let single_line_comment =
        let p = s"//" >> take_while (fun c -> c <> '\n' && c <> '\r') in
        loc (consumed p)
        >>|
        fun {txt; loc} -> Res_comment.makeSingleLineComment ~loc txt

    let multi_line_comment =
        let loop = fix @@ fun loop ->
            take_while (function '*' | '\n' | '\r' -> false | _ -> true) >> (
                (new_line >> loop)
                <|>
                (s"*/" >>$ "*/")
                <|>
                (advance 1 >> loop)
            )
        in
        let p = s"/*" >> loop in
        loc (consumed p)
        >>|
        fun {txt; loc} -> Res_comment.makeMultiLineComment ~loc txt

    let comments =
        let push_comment c =
            c << whitespace >>= fun comment ->
            (state_map (fun s -> {s with comments = comment :: s.comments})).p
        in
        seq 0 (push_comment single_line_comment <|> push_comment multi_line_comment) >>$ ()
    let ng =
        Named.p "nongrammar" begin
            whitespace << comments
        end

    let (~-) p = ng >> p
    let ng_no_new_line =
        (
            mapping t2
            <*> pos << ng <*> pos
        )
        >>=
        fun (p1, p2) ->
            let open Angstrom in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> return ()
            | false -> fail ""

    let ng_new_line =
        (
            mapping t2
            <*> pos << ng <*> pos
        )
        >>=
        fun (p1, p2) ->
            let open Angstrom in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> fail ""
            | false -> return ()

    let del_pos =
        (mapping t2 <*> pos << ng <*> peek_char)
        >>=
        fun (p1, c) ->
            let open Angstrom in
            match c with
            | Some '}' -> return p1
            | Some ')' -> return p1
            | Some ';' -> advance 1 >> APos.pos.p
            | None -> return p1
            | _ ->
                APos.pos.p >>= fun p2 ->
                match p1.pos_lnum = p2.pos_lnum with
                | true -> fail ""
                | false -> return p1

    let del =
        (mapping t2 <*> pos << ng <*> peek_char)
        >>=
        fun (p1, c) ->
            let open Angstrom in
            match c with
            | Some '}' -> return ()
            | Some ')' -> return ()
            | Some ';' -> advance 1 >>$ ()
            | None -> return ()
            | _ ->
                APos.pos.p >>= fun p2 ->
                match p1.pos_lnum = p2.pos_lnum with
                | true -> fail ""
                | false -> return ()


    let identifier's_character = function
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
        | _ -> false

    let k = Fix.Memoize.String.memoize @@ fun x ->
        s x >> failed (satisfy identifier's_character)

    let operator's_character = function
        | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '.' | '!' -> true
        | _ -> false

    let o = fun x ->
        let res = Longident.Lident x in
        s x >> return res

    let op_alias = fun x res ->
        let res = Longident.Lident res in
        s x >> return res

    let upper = function 'A' .. 'Z' -> true | _ -> false
    let lower = function 'a' .. 'z' | '_' -> true | _ -> false


    let ident = Named.p "ident" @@
        take_while1 identifier's_character
    let c_ident first = consumed (skip first >> skip_while identifier's_character)

    let l_ident = Named.p "l_ident" @@ (failed @@ k"_") >> c_ident lower
    let u_ident = Named.p "u_ident" @@ c_ident upper

    let u_longident =
        fold_left_0_n ~f:begin fun lid str -> Longident.Ldot (lid, str) end
            (u_ident >>| fun str -> Longident.Lident str)
            (-s"." >> ng >> u_ident)

    let l_longident =
        (
            mapping (fun a b -> Longident.Ldot (a, b))
            <*> u_longident << -s"." << ng <*> l_ident
        )
        <|>
        (l_ident >>| fun s -> Longident.Lident s)

    let longident = l_longident <|> u_longident

    let _longident =
        fold_left_0_n ~f:begin fun acc x -> Longident.Ldot (acc, x) end
            (ident >>| fun x -> Longident.Lident x)
            (-s"." >> ng >> ident)

(*
    let ll_longident =
        let rec loop res =
            (ng >> s"." >> ng >> l_ident >>= fun str -> loop @@ Longident.Ldot (res, str))
            <|>
            return res

        in
        l_ident >>= fun str -> loop @@ Longident.Lident str
*)
    let rec exact_longident = function
        | Longident.Lident x -> s x >>$ ()
        | Longident.Ldot (lid, x) -> exact_longident lid >> -s"." >> ng << s x
        | Longident.Lapply (lid, arg) -> exact_longident lid >> -s"(" >> ng >> exact_longident arg << -s")"
end
