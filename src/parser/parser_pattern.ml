
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic.Angstrom
open Basic

module Make
        (Named: Angstrom_pos.Sigs.NAMED with module Parser = Angstrom.Parser)
        (Utils: UTILS) (Constant: CONSTANT) (Core: CORE) (Type: TYPE)
        : PATTERN = struct
    open Utils
    open Core
    open Type

    let x = fix_poly @@ fun getter ->
        (module struct
            let pattern = getter.get @@ fun (module M: PATTERN) -> M.pattern
            let pattern_constrainted =
                Named.p "pattern:constraint" begin
                    fold_hlp_left_0_1 ~f:(mk_helper2 Pat.constraint_)
                        pattern (_s":" >> _use core_type)
                end

            let pattern_poly_constrainted =
                Named.p "pattern:constraint:poly" begin
                    fold_hlp_left_0_1 ~f:(mk_helper2 Pat.constraint_)
                        pattern (_s":" >> _use core_type_poly)
                end

            let tuple =
                Named.p "pattern:tuple" begin
                    mapping (mk_helper ~f:Pat.tuple)
                    << s_"(" <*> seq 2 ~sep:(_s",") ~trail (_use pattern_constrainted) << _s")"
                end

            let array =
                Named.p "pattern:array" begin
                    mapping (mk_helper ~f:Pat.array)
                    <* s"[" <*> seq 0 ~sep:(_s",") ~trail (_use pattern_constrainted) <* _s"]"
                end

            let record =
                let lid2str lid =
                    let str =
                        match [@warning "-8"] lid.Location.txt with
                        | Longident.Lident str -> str
                        | Longident.Ldot (_, str) -> str
                    in
                    let loc_end = lid.Location.loc.loc_end in
                    Location.mkloc str @@ make_location {loc_end with pos_cnum = loc_end.pos_cnum - String.length str} loc_end
                in

                mapping @@ mk_helper2 Pat.record
                << s"{" <*> (seq 1 ~sep:(_s",")
                    (map2 (_loc l_longident) (opt (_s":" >> _use pattern_constrainted)) ~f:(fun lid pat ->
                        match pat with
                        | Some pat -> lid, pat
                        | None -> lid, let str = lid2str lid in Pat.var ~loc:str.loc str
                    )))
                <*> (
                    (opt @@ _s"," >> _s"}" >>$ Closed)
                    <|>
                    (_s"," >> _s"_" >> opt @@ _s"," >> _s"}" >>$ Open)
                )

            let list_helper =
                make_list_helper ~constr:Pat.construct ~tuple:Pat.tuple ~get_loc:(fun x -> x.ppat_loc)

            let list =
                Named.p "pattern:list" begin
                        begin
                            mapping (list_helper [] None)
                            <* s"list{" <* _s"}"
                        end
                    <|> (s"list{" >> _s_"..." >> pattern_constrainted << opt @@ _s"," << _s"}")
                    <|> begin
                            mapping list_helper
                            <* s"list{" <*> seq 1 ~sep:(_s",") (_use pattern_constrainted)
                            <*> (
                                        (_s"," >> _s"..." >> _use pattern_constrainted << opt @@ _s"," >>| Base.Option.some)
                                    <|> (opt @@ _s"," >>$ None)
                                )
                            <* _s"}"
                        end
                end

            let constructor_arguments =
                    use tuple
                <|> (s"(" >> _use pattern_constrainted << opt @@ _s"," << _s")")
                <|> use (s"(" >> _s")" >>$ mk_helper2 Pat.construct (Location.mknoloc @@ Longident.Lident "()") None)

            let construct =
                mapping @@ mk_helper2 Pat.construct
                <*> loc u_longident <*>? constructor_arguments

            let polyvariant =
                let tag =
                        ident
                    <|> Constant.String.string
                    <|> take_while1 (function '0'..'9' -> true | _ -> false)
                in
                mapping @@ mk_helper2 Pat.variant
                <* s"#" <*> tag <*>? constructor_arguments

            let typ =
                mapping (mk_helper ~f:Pat.type_)
                <* s"#" <* _s"..." <*> _loc l_longident

            let unpack =
                Named.p "pattern:unpack" begin
                    map2
                    (s"module" >> _s"(" >> ng >> loc u_ident)
                    (_s":" >> _use core_type_package << _s")")
                    ~f:begin fun m t ->
                        let m = Pat.unpack m in
                        mk_helper2 Pat.constraint_ m t
                    end
                    <|>
                    (
                        mapping (mk_helper ~f:Pat.unpack)
                        <* s_"module" <* s_"(" <*> loc u_ident <* _s")"
                    )
                end

            let signed_number =
                    (
                        mapping begin function [@warning "-8"]
                            | Pconst_integer (str, suf) ->
                                Pconst_integer ("-" ^ str, suf)
                            | Pconst_float (str, suf) ->
                                Pconst_float ("-" ^ str, suf)
                        end
                        <* s"-" <*> Constant.Number.p
                    )
                <|> (s"+" *> Constant.Number.p)

            let number_range =
                let number = signed_number <|> Constant.Number.p in

                mapping @@ mk_helper2 Pat.interval
                <*> number <* _s".." << ng <*> number

            let js_string qtag =
                Named.p "pattern:js_string" begin
                    s"`" >>= fun _ ->
                    let buf = Buffer.create 16 in
                    let mk_const () =
                        Const.string ~quotation_delimiter:qtag @@ Buffer.contents buf
                    in
                    fix @@ fun loop ->
                    (take_while (function '\n' | '`' | '\\' | '\r' -> false | _ -> true) >>| Buffer.add_string buf)
                    >>
                    (
                        (s"`" >>| fun _ ->
                            helper_add_attr "res.template" @@
                            mk_helper ~f:Pat.constant (mk_const ())
                        )
                        <|>
                        ((new_line >>| Buffer.add_string buf) >> loop)
                        <|>
                        ((s"\\`" >>| fun _ -> Buffer.add_char buf '`') >> loop)
                        <|>
                        ((s"\\\\" >>| fun _ -> Buffer.add_char buf '\\') >> loop)
                    )
                end

            let true_false =
                mapping (fun a -> mk_helper2 Pat.construct a None)
                <*> (loc ((k"true" >>$ Longident.Lident "true") <|> (k"false" >>$ Longident.Lident "false")))

            let pattern_atom =
                Named.p "pattern:atom" begin
                    (k"_" >>$ mk_helper ~f:Pat.any ())
                    <|> (
                            mapping @@ mk_helper2 Pat.interval
                            <*> Constant.Character.p << _s".." << ng <*> Constant.Character.p
                        )
                    <|> number_range
                    <|> (
                            mapping @@ (fun a -> mk_helper2 Pat.construct a None)
                            <*> loc (s"()" >>$ Longident.Lident "()")
                        )
                    <|> unpack
                    <|> tuple <|> (s"(" >> ng >> add_attrs pattern_constrainted << _s")")
                    <|> array
                    <|> record
                    <|> ((signed_number <|> Constant.p) >>| mk_helper ~f:Pat.constant)
                    <|> js_string "js"
                    <|> list <|> construct <|> polyvariant <|> typ
                    <|> true_false
                    <|> (loc ident >>| mk_helper ~f:Pat.var)
                end

            let pattern_exception =
                    (k"exception" >> _use pattern_atom >>| mk_helper ~f:Pat.exception_)
                <|> (extension >>| mk_helper ~f:Pat.extension)
                <|> pattern_atom

            let primary =
                    (k"lazy" >> _use pattern_exception >>| mk_helper ~f:Pat.lazy_)
                <|> pattern_exception

            let alias =
                fold_hlp_left_0_n ~f:begin mk_helper2 Pat.alias end
                    primary (_k"as" >> ng >> loc l_ident)

            let or_ =
                fold_hlp_left_0_n ~f:begin mk_helper2 Pat.or_ end
                    alias (_s"|" >> _use alias)

            let pattern = Named.p "pattern" or_
        end: PATTERN)

    let pattern = let (module M) = x in M.pattern
    let pattern_atom = let (module M) = x in M.pattern_atom
    let pattern_constrainted = let (module M) = x in M.pattern_constrainted
    let pattern_poly_constrainted = let (module M) = x in M.pattern_poly_constrainted
end
