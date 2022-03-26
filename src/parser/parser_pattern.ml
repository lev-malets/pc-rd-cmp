open Base
open Sigs
open Asttypes
open Parsetree
open Ast_helper
open Basic

module Make
        (Basic: BASIC)
        (Core: CORE with module Comb = Basic.Comb)
        (Type: TYPE with module Comb = Basic.Comb)
        = struct
    open Basic
    open Core
    open Type
    open Comb

    module Comb = Comb
    module type THIS = PATTERN with module Comb = Basic.Comb

    let x = fix_poly @@ fun getter ->
        (module struct
            module Comb = Comb

            let pattern = getter.get @@ fun (module M: THIS) -> M.pattern
            let pattern_constrainted =
                named "pattern:constraint" begin
                        with_loc & hlp2 Pat.constraint_
                        +pattern -ng -colon -ng +core_type

                    ||  pattern
                end

            let pattern_poly_constrainted =
                named "pattern:constraint:poly" begin
                        with_loc & hlp2 Pat.constraint_
                        +pattern -ng -colon -ng +core_type_poly

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
                -l_brace -ng
                +(seq ~n:1 ~sep
                        (
                            mapping begin fun lid pat ->
                                match pat with
                                | Some pat -> lid, pat
                                | None -> lid, let str = lid2str lid in Pat.var ~loc:str.loc str
                            end
                            +loc l_longident +opt(ng >> colon >> ng >> pattern_constrainted)
                        )
                )
                +(
                        opt sep >> ng >> r_brace >>$ Closed
                    ||  sep >> _' >> opt sep >> ng >> r_brace >>$ Open
                )

            let list_helper =
                make_list_helper ~constr:Pat.construct ~tuple:Pat.tuple ~get_loc:(fun x -> x.ppat_loc)

            let list =
                named "pattern:list" begin
                        with_loc & mapping (list_helper [] None)
                        -list -ng -r_brace

                    ||  list >> ng >> ellipsis >> ng >> pattern_constrainted << opt sep << ng << r_brace

                    ||  with_loc & mapping list_helper
                        -list -ng +(seq ~n:1 pattern_constrainted ~sep)
                        +(
                                sep >> ng >> ellipsis >> ng >> pattern_constrainted >>| Base.Option.some

                            ||  return None
                        )
                        -opt(sep) -ng -r_brace
                end

            let constructor_arguments =
                    tuple
                ||  l_paren >> ng >> pattern_constrainted << opt sep << ng << r_paren
                ||  with_loc & mapping begin fun loc ->
                        Pat.construct ~loc (Location.mknoloc @@ Longident.Lident "()") None
                    end
                    -l_paren -ng -r_paren

            let construct =
                with_loc & hlp2 Pat.construct
                +loc u_longident +opt(constructor_arguments)

            let polyvariant =
                with_loc & hlp2 Pat.variant
                +variant_tag +opt(constructor_arguments)

            let typ =
                with_loc & hlp Pat.type_
                -hash -ng -ellipsis -ng +loc l_longident

            let unpack =
                named "pattern:unpack" begin
                        with_loc & mapping begin fun m t loc ->
                            let m = Pat.unpack m in
                            Pat.constraint_ ~loc m t
                        end
                        -module'-ng -l_paren -ng +loc u_ident
                        -ng -colon -ng +core_type_package -ng -r_paren

                    ||  with_loc & hlp Pat.unpack
                        -module'-ng -l_paren -ng +loc u_ident -ng -r_paren
                end

            let signed_number =
                    mapping begin function [@warning "-8"]
                        | Pconst_integer (str, suf) ->
                            Pconst_integer ("-" ^ str, suf)
                        | Pconst_float (str, suf) ->
                            Pconst_float ("-" ^ str, suf)
                    end
                    -minus +number

                ||  plus >> number

            let number_range =
                let number = signed_number <|> number in

                with_loc & hlp2 Pat.interval
                +number -ng -dot_dot -ng +number

            let js_string =
                named "pattern:js_string" begin
                    with_loc & mapping begin fun c loc ->
                        Pat.constant ~loc ~attrs:[Hc.attr "res.template"] c
                    end
                    +string_multiline ~q:"`"
                end

            let true_ =
                with_loc & mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid ["true"]) None)
                +loc_of(true')
            let false_ =
                with_loc & mapping (fun l loc -> Pat.construct ~loc (mkloc ~loc:l @@ Hc.lid ["false"]) None)
                +loc_of(false')

            let pattern_atom =
                named "pattern:atom" begin
                    peek_first
                    [
                        with_loc & mapping (fun loc -> Pat.any ~loc ())
                        -_'
                    ;
                        with_loc & hlp2 Pat.interval
                        +character -ng -dot_dot -ng +character
                    ; number_range
                    ;
                        with_loc & mapping (fun loc -> Pat.construct ~loc (mkloc ~loc @@ Hc.lid ["()"]) None)
                        -l_paren -ng -r_paren
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
                    -exception' -ng +pattern_atom

                ||  with_loc & hlp Pat.extension
                    +extension

                || pattern_atom

            let primary =
                    with_loc & hlp Pat.lazy_
                    -lazy' -ng +pattern_exception
                ||  pattern_exception

            let alias =
                fold_left_cont_0_n
                    primary
                    (
                        mapping begin fun name prev ->
                            Pat.alias ~loc:(loc_comb prev.ppat_loc name.loc) prev name
                        end
                        -ng -as' -ng +loc l_ident
                    )

            let or_ =
                fold_left_cont_0_n
                    alias
                    (
                        mapping begin fun alias prev ->
                            Pat.or_ ~loc:(loc_comb prev.ppat_loc alias.ppat_loc) prev alias
                        end
                        -ng -pipe -ng +alias
                    )

            let pattern = memo & named "pattern" or_
        end: THIS)

    let pattern = let (module M) = x in M.pattern
    let pattern_atom = let (module M) = x in M.pattern_atom
    let pattern_constrainted = let (module M) = x in M.pattern_constrainted
    let pattern_poly_constrainted = let (module M) = x in M.pattern_poly_constrainted
end
