open Base
open Parsetree

module Make (Base : Sigs.BASIC_BASE): Sigs.BASIC = struct
    include Base
    module type CORE = Sigs.CORE with module Comb = Comb
    module type MODEXPR = Sigs.MODEXPR with module Comb = Comb
    module type EXPRESSION = Sigs.EXPRESSION with module Comb = Comb
    module type PATTERN = Sigs.PATTERN with module Comb = Comb
    module type TYPE = Sigs.TYPE with module Comb = Comb
    module type MODTYPE = Sigs.MODTYPE with module Comb = Comb

    open Comb

    let ng_not_empty =
        run & mapping begin fun p1 p2 ->
            let open Simple in
            let open Lexing in
            match p1.pos_cnum = p2.pos_cnum with
            | true -> fail
            | false -> return ()
        end
        +pos_end -ng +pos

    let ng_no_new_line =
        run & mapping begin fun p1 p2 ->
            let open Simple in
            let open Lexing in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> return ()
            | false -> fail
        end
        +pos_end -ng +pos

    let ng_new_line =
        run & mapping begin fun p1 p2 ->
            let open Simple in
            let open Lexing in
            match p1.pos_lnum = p2.pos_lnum with
            | true -> fail
            | false -> return ()
        end
        +pos_end -ng +pos

    let with_del p =
        mapping (fun p1 f p2 -> f (make_location p1 p2))
        +pos +p +del_pos

    let sep = ng >> comma >> ng

    let u_longident =
        named "u_longident" &
        fold_left_0_n ~f:begin fun lid str -> Longident.Ldot (lid, str) end
            (u_ident >>| fun str -> Longident.Lident str)
            (ng >> dot >> ng >> u_ident)

    let l_longident =
        named "l_longident" &
            mapping (fun a b -> Longident.Ldot (a, b))
            +u_longident -ng -dot -ng +l_ident

        ||  l_ident >>| fun s -> Longident.Lident s

    let longident =
        named "longident" &

            mapping begin fun a b ->
                match b with
                | None -> a
                | Some b -> Longident.Ldot (a, b)
            end
            +u_longident +opt(ng >> dot >> ng >> l_ident)

        ||  l_ident >>| fun s -> Longident.Lident s

    let attribute_id =
        fold_left_0_n
            ~f:begin fun buf x ->
                Buffer.add_char buf '.';
                Buffer.add_string buf x;
                buf
            end
            (ident >>| fun x ->
                let buf = Buffer.create (String.length x) in
                Buffer.add_string buf x;
                buf
            )
            (ng >> dot >> ng >> ident)
        >>| Buffer.contents
    let variant_tag =
        hash >> ng >> (
                ident
            ||  string_raw
            ||  (integer >>| fun (x, _) -> x)
        )

    let parens p = l_paren >> ng >> p << ng << r_paren
    let brackets p = l_bracket >> ng >> p << ng << r_bracket
    let braces p = l_brace >> ng >> p << ng << r_brace
    let chevrons p = lt >> ng >> p << ng << gt

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
