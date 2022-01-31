open Base
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic

module Make
        (APos: APOS)
        (Utils: UTILS) (Constant: CONSTANT) (Core: CORE) (Type: TYPE)
        : PATTERN = struct
    open APos
    open Utils
    open Constant
    open Core
    open Type

    let x = fix_poly @@ fun getter ->
        (module struct
            let pattern = getter.get @@ fun (module M: PATTERN) -> M.pattern
            let pattern_constrainted =
                named "pattern:constraint" begin
                        with_loc & hlp2 Pat.constraint_
                        +pattern -ng -s":" -ng +core_type

                    ||  pattern
                end

            let pattern_poly_constrainted =
                named "pattern:constraint:poly" begin
                        with_loc & hlp2 Pat.constraint_
                        +pattern -ng -s":" -ng +core_type_poly

                    ||  pattern
                end

            let tuple =
                named "pattern:tuple" begin
                    with_loc & parens & hlp Pat.tuple
                    +(seq ~n:2 pattern_constrainted ~sep ~trail)
                end

            let array =
                named "pattern:array" begin
                    with_loc & brackets & hlp Pat.array
                    +(seq pattern_constrainted ~sep ~trail)
                end

            let record =
                let lid2str lid =
                    let str =
                        match [@warning "-8"] lid.Location.txt with
                        | Longident.Lident str -> str
                        | Longident.Ldot (_, str) -> str
                    in
                    let loc_end = lid.Location.loc.loc_end in
                    Location.mkloc str @@ make_location {loc_end with pos_cnum = Int.(loc_end.pos_cnum - String.length str)} loc_end
                in

                with_loc & hlp2 Pat.record
                -s"{" -ng
                +(seq ~n:1 ~sep
                        (
                            mapping begin fun lid pat ->
                                match pat with
                                | Some pat -> lid, pat
                                | None -> lid, let str = lid2str lid in Pat.var ~loc:str.loc str
                            end
                            +loc l_longident +opt(ng >> s":" >> ng >> pattern_constrainted)
                        )
                )
                +(
                        opt sep >> ng >> s"}" >>$ Closed
                    ||  sep >> s"_" >> opt sep >> ng >> s"}" >>$ Open
                )

            let list_helper =
                make_list_helper ~constr:Pat.construct ~tuple:Pat.tuple ~get_loc:(fun x -> x.ppat_loc)

            let list =
                named "pattern:list" begin
                        with_loc & mapping (list_helper [] None)
                        -s"list{" -ng -s"}"

                    ||  s"list{" >> ng >> s"..." >> ng >> pattern_constrainted << opt sep << ng << s"}"

                    ||  with_loc & mapping list_helper
                        -s"list{" -ng +(seq ~n:1 pattern_constrainted ~sep)
                        +(
                                sep >> ng >> s"..." >> ng >> pattern_constrainted >>| Base.Option.some

                            ||  return None
                        )
                        -opt(sep) -ng -s"}"
                end

            let constructor_arguments =
                    tuple
                ||  s"(" >> ng >> pattern_constrainted << opt sep << ng << s")"
                ||  with_loc & mapping begin fun loc ->
                        Pat.construct ~loc (Location.mknoloc @@ Longident.Lident "()") None
                    end
                    -s"(" -ng -s")"

            let construct =
                with_loc & hlp2 Pat.construct
                +loc u_longident +opt(constructor_arguments)

            let polyvariant =
                let tag =
                        ident
                    ||  string_raw
                    ||  take_while1 (function '0'..'9' -> true | _ -> false)
                in
                with_loc & hlp2 Pat.variant
                -s"#" +tag +opt(constructor_arguments)

            let typ =
                with_loc & hlp Pat.type_
                -s"#" -ng -s"..." -ng +loc l_longident

            let unpack =
                named "pattern:unpack" begin
                        with_loc & mapping begin fun m t loc ->
                            let m = Pat.unpack m in
                            Pat.constraint_ ~loc m t
                        end
                        -s"module" -ng -s"(" -ng +loc u_ident
                        -ng -s":" -ng +core_type_package -ng -s")"

                    ||  with_loc & hlp Pat.unpack
                        -s"module" -ng -s"(" -ng +loc u_ident -ng -s")"
                end

            let signed_number =
                    mapping begin function [@warning "-8"]
                        | Pconst_integer (str, suf) ->
                            Pconst_integer ("-" ^ str, suf)
                        | Pconst_float (str, suf) ->
                            Pconst_float ("-" ^ str, suf)
                    end
                    -s"-" +number

                ||  s"+" >> number

            let number_range =
                let number = signed_number <|> number in

                with_loc & hlp2 Pat.interval
                +number -ng -s".." -ng +number

            let js_string =
                named "pattern:js_string" begin
                    with_loc & mapping begin fun c loc ->
                        Pat.constant ~loc ~attrs:[Hc.attr "res.template"] c
                    end
                    +string_multiline ~q:"`"
                end

            let true_ =
                with_loc & mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid ["true"]) None)
                +loc_of(k"true")
            let false_ =
                with_loc & mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid ["false"]) None)
                +loc_of(k"false")

            let pattern_atom =
                named "pattern:atom" begin
                    peek_first
                    [
                        with_loc & mapping (fun loc -> Pat.any ~loc ())
                        -k"_"
                    ;
                        with_loc & hlp2 Pat.interval
                        +character -ng -s".." -ng +character
                    ; number_range
                    ;
                        with_loc & mapping (fun loc -> Pat.construct ~loc (mkloc ~loc @@ Hc.lid ["()"]) None)
                        -s"(" -ng -s")"
                    ; unpack
                    ; tuple
                    ;
                        with_loc & parens & mapping pat_loc
                        +pat_attrs pattern_constrainted
                    ; array
                    ; record
                    ;
                        with_loc & hlp Pat.constant
                        +(signed_number || constant)
                    ; js_string
                    ; list
                    ; construct
                    ; polyvariant
                    ; typ
                    ; true_
                    ; false_
                    ;
                        with_loc & hlp Pat.var
                        +loc(ident)
                    ]
                end

            let pattern_exception =
                    with_loc & hlp Pat.exception_
                    -k"exception" -ng +pattern_atom

                ||  with_loc & hlp Pat.extension
                    +extension

                || pattern_atom

            let primary =
                    with_loc & hlp Pat.lazy_
                    -k"lazy" -ng +pattern_exception
                ||  pattern_exception

            let alias =
                fold_left_cont_0_n
                    primary
                    (
                        mapping begin fun name prev ->
                            Pat.alias ~loc:(loc_comb prev.ppat_loc name.loc) prev name
                        end
                        -ng -k"as" -ng +loc l_ident
                    )

            let or_ =
                fold_left_cont_0_n
                    alias
                    (
                        mapping begin fun alias prev ->
                            Pat.or_ ~loc:(loc_comb prev.ppat_loc alias.ppat_loc) prev alias
                        end
                        -ng -s"|" -ng +alias
                    )

            let pattern = memo & named "pattern" or_
        end: PATTERN)

    let pattern = let (module M) = x in M.pattern
    let pattern_atom = let (module M) = x in M.pattern_atom
    let pattern_constrainted = let (module M) = x in M.pattern_constrainted
    let pattern_poly_constrainted = let (module M) = x in M.pattern_poly_constrainted
end
