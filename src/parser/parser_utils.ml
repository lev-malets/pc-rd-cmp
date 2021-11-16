open Basic.Angstrom
open Basic
open Core_kernel
open Sigs
open Parsetree

module Attrs = struct
    let ns_namedArgLoc loc =
        Location.mkloc "ns.namedArgLoc" loc, PStr []

    let ns_braces = Location.mknoloc "ns.braces", PStr []

    let bs = Location.mknoloc "bs", PStr []

    let jsx = Location.mknoloc "JSX", PStr []
end

module Hc = struct
    let attr s = Location.mknoloc s, PStr []
    let attr_loc s loc = Location.mkloc s loc, PStr []

    let id m f = Ast_helper.Exp.ident @@ Location.mknoloc @@ Longident.Ldot (Longident.Lident m, f)
    let unit_expr loc = Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
end

module Make (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser): UTILS = struct
    let s =
        Fix.Memoize.String.memoize @@ fun x ->
        Named.p ("\'" ^ x ^ "\'") begin
            memo @@ begin
                match String.length x with
                | 0 -> return ()
                | 1 -> (char x.[0] >>$ ())
                | _ -> string x >>$ ()
            end
        end

    let single_line_comment =
        let p = s"//" >> take_while (fun c -> c <> '\n' && c <> '\r') in
        map (loc (consumed p))
        ~f:begin fun {txt; loc} -> Res_comment.makeSingleLineComment ~loc txt end

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
        map (loc (consumed p))
        ~f:begin fun {txt; loc} -> Res_comment.makeMultiLineComment ~loc txt end

    let comments =
        let push_comment c =
            c << whitespace >>= fun comment ->
            state_map (fun s -> {s with comments = comment :: s.comments})
        in
        many (push_comment single_line_comment <|> push_comment multi_line_comment) >>$ ()
    let ng = memo
        begin
            whitespace << comments
        end
    let ng_no_new_line =
        position >>= fun pos ->
        ng >> position >>= fun pos1 ->
        match pos1.pos_lnum = pos.pos_lnum with
        | true -> return ()
        | false -> fail ""

    let ng_new_line =
        position >>= fun pos ->
        ng >> position >>= fun pos1 ->
        match pos1.pos_lnum = pos.pos_lnum with
        | true -> fail ""
        | false -> return ()

    let del_pos =
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

    let del =
        position >>= fun pos ->
        ng >> peek_char >>= function
        | Some '}' -> return ()
        | Some ')' -> return ()
        | Some ';' -> advance 1
        | None -> return ()
        | _ ->
            position >>= fun pos1 ->
            match pos1.pos_lnum = pos.pos_lnum with
            | true -> fail ""
            | false -> return ()

    let _pos = ng >> pos
    let _loc p = ng >> loc p
    let _loc_of p = ng >> loc_of p
    let _set_loc p = ng >> set_loc p

    let _s = Fix.Memoize.String.memoize @@ fun x ->
        ng >> s x

    let s_ = Fix.Memoize.String.memoize @@ fun x ->
        s x << ng

    let _s_ = Fix.Memoize.String.memoize @@ fun x ->
        ng >> s x << ng

    let identifier's_character = function
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
        | _ -> false

    let k = Fix.Memoize.String.memoize @@ fun x ->
        s x >> peek_char >>= function
        | Some c when identifier's_character c -> fail ""
        | _ -> return ()
    let _k = Fix.Memoize.String.memoize @@ fun x ->
        ng >> k x

    let k_ = Fix.Memoize.String.memoize @@ fun x ->
        k x << ng

    let _k_ = Fix.Memoize.String.memoize @@ fun x ->
        ng >> k x << ng

    let operator's_character = function
        | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '.' | '!' -> true
        | _ -> false

    let o = fun x ->
        let res = Longident.Lident x in
        s x >> return res
(*
        >> peek_char >>= function
        | Some c when operator's_character c -> fail "TODO"
        | _ -> return res
*)

    let op_alias = fun x res ->
        let res = Longident.Lident res in
        s x >> return res
(*
        >> peek_char >>= function
        | Some c when operator's_character c -> fail "TODO"
        | _ -> return res
*)

    let ident = Named.p "ident" @@
        take_while1 identifier's_character
    let c_ident first = consumed (skip first >> skip_while identifier's_character)

    let l_ident = Named.p "l_ident" @@ failed @@ k"_" >> c_ident lower
    let u_ident = Named.p "u_ident" @@ c_ident upper

    let u_longident =
        let rec loop res =
            (ng >> s"." >> ng >> u_ident >>= fun str -> loop @@ Longident.Ldot (res, str))
            <|>
(*
            (ng >> in_ '(' ')' u_longident >>= fun t -> loop @@ Lapply (res, t))
            <|>
*)
            return res

        in
        u_ident >>= fun str -> loop @@ Longident.Lident str

    let l_longident =
        map2 ~f:(fun a b -> Longident.Ldot (a, b))
        u_longident (ng >> s"." >> ng >> l_ident)
        <|>
        (l_ident >>| fun s -> Longident.Lident s)

    let longident = l_longident <|> u_longident

    let _longident =
        fold_left_0_n ~f:begin fun acc x -> Longident.Ldot (acc, x) end
            (ident >>| fun x -> Longident.Lident x)
            (_s"." >> ng >> ident)

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
        | Longident.Lident x -> s x
        | Longident.Ldot (lid, x) -> exact_longident lid >> _s"." >> ng >> s x
        | Longident.Lapply (lid, arg) -> exact_longident lid >> _s"(" >> ng >> exact_longident arg << _s")"
end
