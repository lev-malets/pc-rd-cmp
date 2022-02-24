open Base
open Parsetree

open Sigs

module Make (APos: APOS): UTILS = struct
    open APos

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
                ||  (mapping (fun l tail -> "/*" :: (l @ "*/" :: tail) ) -s"/*" +parts +parts)
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
        fun x pos comments -> Basic.LogElement.Comment (Res_comment.setPrevTokEndPos x pos; x) :: comments

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
                | Some (hlp, _) -> Angstrom.log_many (hlp pos [])
                | _ -> Angstrom.return ()
        in
        {p2 with info = p.info}

    let ng_no_new_line =
        (
            t2
            +pos -ng +pos
        )
        >>=
        fun (p1, p2) ->
            let open Angstrom in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> return ()
            | false -> fail ""

    let ng_new_line =
        (
            t2
            +pos -ng +pos
        )
        >>=
        fun (p1, p2) ->
            let open Angstrom in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> fail ""
            | false -> return ()

    let del_pos =
        (t2 +pos -ng +peek_char)
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
        (t2 +pos -ng +peek_char)
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

    let with_del p =
        mapping (fun p1 f p2 -> f (make_location p1 p2))
        +pos +p +del_pos

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

    let sep = ng >> s"," >> ng

    let ident = named "ident" @@
        take_while1 identifier's_character
    let c_ident first = consumed (skip first >> skip_while identifier's_character)

    let l_ident = named "l_ident" @@ (failed @@ k"_") >> c_ident lower
    let u_ident = named "u_ident" @@ c_ident upper

    let u_longident =
        fold_left_0_n ~f:begin fun lid str -> Longident.Ldot (lid, str) end
            (u_ident >>| fun str -> Longident.Lident str)
            (ng >> s"." >> ng >> u_ident)

    let l_longident =
            mapping (fun a b -> Longident.Ldot (a, b))
            +u_longident -ng -s"." -ng +l_ident

        || l_ident >>| fun s -> Longident.Lident s

    let longident = l_longident <|> u_longident

    let _longident =
        fold_left_0_n ~f:begin fun acc x -> Longident.Ldot (acc, x) end
            (ident >>| fun x -> Longident.Lident x)
            (ng >> s"." >> ng >> ident)

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
        | Longident.Lident x -> k x >>$ ()
        | Longident.Ldot (lid, x) -> exact_longident lid >> ng >> s"." >> ng << s x
        | Longident.Lapply (lid, arg) -> exact_longident lid >> ng >> s"(" >> ng >> exact_longident arg << ng << s")"

    let parens p = s"(" >> ng >> p << ng << s")"
    let brackets p = s"[" >> ng >> p << ng << s"]"
    let braces p = s"{" >> ng >> p << ng << s"}"
    let chevrons p = s"<" >> ng >> p << ng << s">"

    let na_hlp (f: ?loc:Warnings.loc -> 'a -> 'b) =
        return @@ fun a loc -> f ~loc a

    let hlp (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) =
        return @@ fun a loc -> f ~loc a

    let hlp2 (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) =
        return @@ fun a b loc -> f ~loc a b

    let hlp3 (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) =
        return @@ fun a b c loc -> f ~loc a b c

    let hlp4 (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) =
        return @@ fun a b c d loc -> f ~loc a b c d

    let hlp_a (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) =
        return @@ fun attrs a loc -> f ~loc ~attrs a

    let hlp2_a (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) =
        return @@ fun attrs a b loc -> f ~attrs ~loc a b

    let hlp3_a (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) =
        return @@ fun attrs a b c loc -> f ~attrs ~loc a b c

    let hlp4_a (f: ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) =
        return @@ fun attrs a b c d loc -> f ~attrs ~loc a b c d
end
