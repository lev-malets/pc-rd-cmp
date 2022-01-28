
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

    val variant_tag : string parser

    val pat_attrs : pattern parser -> pattern parser
    val exp_attrs : expression parser -> expression parser
    val mod_attrs : module_expr parser -> module_expr parser
    val mty_attrs : module_type parser -> module_type parser
    val typ_attrs : core_type parser -> core_type parser
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
    val sep : unit parser
    val parens : 'a parser -> 'a parser
    val brackets : 'a parser -> 'a parser
    val braces : 'a parser -> 'a parser
    val chevrons : 'a parser -> 'a parser

    val del_pos : Lexing.position parser
    val del : unit parser
    val with_del : (Location.t -> 'a) parser -> 'a parser
    val s : string -> string parser

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

module type MODTYPE = sig
    open Parsetree

    val modtype : module_type parser
    val modtype_functor : module_type parser
    val modtype_with : module_type parser
end

module type MODEXPR = sig
    open Parsetree

    val modexpr : module_expr parser
    val modexpr_constrainted : module_expr parser
end

module type TYPE = sig
    val core_type_atom : core_type parser
    val core_type_arrow : core_type parser
    val core_type : core_type parser
    val core_type_poly : core_type parser
    val core_type_package : core_type parser

    val type_extension_constructor : extension_constructor parser
    val type_extension : type_extension parser

    val type_decl_params : (core_type * Asttypes.variance) list parser
    val type_decl_constraints : (core_type * core_type * Location.t) list parser
    val type_declaration : type_declaration parser
end

module type EXPRESSION = sig
    val expression : expression parser
    val expression_arrow : expression parser
    val expression_sequence : expression parser
    val expression_p0 : expression parser
end

module type PATTERN = sig
    val pattern : pattern parser
    val pattern_atom : pattern parser
    val pattern_constrainted : pattern parser
    val pattern_poly_constrainted : pattern parser
end
