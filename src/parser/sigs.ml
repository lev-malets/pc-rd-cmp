
open Basic
open Parsetree

module type EXT = sig
    module Named: Angstrom_pos.Sigs.NAMED with module Parser = APos.Parser
    module Peek: Angstrom_pos.Sigs.PEEK with module Parser = APos.Parser
end

module type PARSE = sig
    val parse_interface : src:string -> filename:string -> (signature, string) result
    val parse_implementation : src:string -> filename:string -> (structure, string) result
end

module type CORE = sig
    val attribute : attribute parser
    val extension : extension parser
    val signature : signature parser
    val structure : structure parser

    val attrs_ : attributes parser
    val attrs1_ : attributes parser
    val use : 'a helper parser -> 'a parser
    val use_na : 'a na_helper parser -> 'a parser

    val use_del : 'a helper parser -> 'a parser
    val use_na_del : 'a na_helper parser -> 'a parser

    val add_attrs : 'a helper parser -> 'a helper parser
    val set_p1 : 'a helper parser -> 'a helper parser

    val variant_tag : string parser
end

module type CONSTANT = sig
    module Number : sig
        val p : constant parser
    end

    module Character : sig
        val p : constant parser
    end

    module String : sig
        val string : string parser
        val multiline : q:char -> constant parser
        val p : constant parser
    end

    val p : constant parser
end

module type UTILS = sig
    val single_line_comment : Res_comment.t parser
    val multi_line_comment : Res_comment.t parser
    val comments : unit parser
    val ng : unit parser
    val ng_no_new_line : unit parser
    val ng_new_line : unit parser

    val (~-) : 'a parser -> 'a parser

    val del_pos : Lexing.position parser
    val del : unit parser
    val s : string -> unit parser

    val identifier's_character : char -> bool
    val k : string -> unit parser
    val operator's_character : char -> bool
    val o : string -> Longident.t parser

    val op_alias : string -> string -> Longident.t parser

    val ident : string parser

    val l_ident : string parser
    val u_ident : string parser

    val longident : Longident.t parser
    val u_longident : Longident.t parser
    val l_longident : Longident.t parser
    val exact_longident : Longident.t -> unit parser
end


module type MODEXPR = sig
    open Parsetree

    val modexpr : module_expr helper parser
    val modexpr_constrainted : module_expr helper parser
    val modtype : module_type helper parser
    val modtype_functor : module_type helper parser
    val modtype_with : module_type helper parser
end

module type TYPE = sig
    val core_type_atom : core_type helper parser
    val core_type_arrow : core_type helper parser
    val core_type : core_type helper parser
    val core_type_poly : core_type helper parser
    val core_type_package : core_type helper parser

    val type_extension_constructor : extension_constructor helper parser
    val type_extension : type_extension helper parser

    val type_decl_params : (core_type * Asttypes.variance) list parser
    val type_decl_constraints : (core_type * core_type * Location.t) list parser
    val type_declaration : type_declaration helper parser
end

module type EXPRESSION = sig
    val expression : expression helper parser
    val expression_arrow : expression helper parser
    val expression_sequence : expression helper parser
    val expression_p0 : expression helper parser
end

module type PATTERN = sig
    val pattern : pattern helper parser
    val pattern_atom : pattern helper parser
    val pattern_constrainted : pattern helper parser
    val pattern_poly_constrainted : pattern helper parser
end
