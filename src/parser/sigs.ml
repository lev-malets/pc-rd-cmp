open Parsetree
open Basic

module type COMB = Pc.Sigs.COMB with type log_elem = LogElement.t

module type PARSE = sig
    val parse_interface : src:string -> filename:string -> signature parse_result option
    val parse_implementation : src:string -> filename:string -> structure parse_result option
end

module type CORE = sig
    module Comb : COMB

    val attribute : attribute Comb.t
    val extension : extension Comb.t
    val signature : signature Comb.t
    val structure : structure Comb.t

    val attrs_ : attributes Comb.t
    val attrs1_ : attributes Comb.t

    val pat_attrs : pattern Comb.t -> pattern Comb.t
    val exp_attrs : expression Comb.t -> expression Comb.t
    val mod_attrs : module_expr Comb.t -> module_expr Comb.t
    val mty_attrs : module_type Comb.t -> module_type Comb.t
    val typ_attrs : core_type Comb.t -> core_type Comb.t
end
module type MODTYPE = sig
    module Comb : COMB
    open Parsetree

    val modtype : module_type Comb.t
    val modtype_functor : module_type Comb.t
    val modtype_with : module_type Comb.t
end

module type MODEXPR = sig
    module Comb : COMB
    open Parsetree

    val modexpr : module_expr Comb.t
    val modexpr_constrainted : module_expr Comb.t
end

module type TYPE = sig
    module Comb : COMB

    val core_type_atom : core_type Comb.t
    val core_type_fun : core_type Comb.t
    val core_type : core_type Comb.t
    val core_type_poly : core_type Comb.t
    val core_type_package : core_type Comb.t

    val type_extension_constructor : extension_constructor Comb.t
    val type_extension : type_extension Comb.t

    val type_decl_params : (core_type * Asttypes.variance) list Comb.t
    val type_decl_constraints : (core_type * core_type * Location.t) list Comb.t
    val type_declaration : type_declaration Comb.t
end

module type EXPRESSION = sig
    module Comb : COMB

    val expression : expression Comb.t
    val expression_fun : expression Comb.t
    val expression_sequence : expression Comb.t
    val expression_p0 : expression Comb.t
end

module type PATTERN = sig
    module Comb : COMB

    val pattern : pattern Comb.t
    val pattern_atom : pattern Comb.t
    val pattern_constrainted : pattern Comb.t
    val pattern_poly_constrainted : pattern Comb.t
end

module type BASIC_BASE = sig
    module Comb : COMB

    val and' : Token.t Comb.t
    val as' : Token.t Comb.t
    val assert' : Token.t Comb.t
    val catch : Token.t Comb.t
    val constraint' : Token.t Comb.t
    val downto' : Token.t Comb.t
    val else' : Token.t Comb.t
    val exception' : Token.t Comb.t
    val export : Token.t Comb.t
    val external' : Token.t Comb.t
    val false' : Token.t Comb.t
    val for' : Token.t Comb.t
    val from : Token.t Comb.t
    val if' : Token.t Comb.t
    val in' : Token.t Comb.t
    val include' : Token.t Comb.t
    val import : Token.t Comb.t
    val json_tag : Token.t Comb.t
    val lazy' : Token.t Comb.t
    val let' : Token.t Comb.t
    val module' : Token.t Comb.t
    val mutable' : Token.t Comb.t
    val nonrec' : Token.t Comb.t
    val of' : Token.t Comb.t
    val open' : Token.t Comb.t
    val private' : Token.t Comb.t
    val rec' : Token.t Comb.t
    val sig' : Token.t Comb.t
    val switch : Token.t Comb.t
    val to' : Token.t Comb.t
    val true' : Token.t Comb.t
    val try' : Token.t Comb.t
    val type' : Token.t Comb.t
    val unpack : Token.t Comb.t
    val when' : Token.t Comb.t
    val while' : Token.t Comb.t
    val with' : Token.t Comb.t

    val _' : Token.t Comb.t

    val ampersand : Token.t Comb.t
    val ampersand_ampersand : Token.t Comb.t
    val arrow : Token.t Comb.t
    val asterisk : Token.t Comb.t
    val asterisk_asterisk : Token.t Comb.t
    val asterisk_dot : Token.t Comb.t
    val at : Token.t Comb.t
    val at_at : Token.t Comb.t
    val bang : Token.t Comb.t
    val bang_eq : Token.t Comb.t
    val bang_eq_eq : Token.t Comb.t
    val colon : Token.t Comb.t
    val colon_eq : Token.t Comb.t
    val colon_gt : Token.t Comb.t
    val comma : Token.t Comb.t
    val dot : Token.t Comb.t
    val dot_dot : Token.t Comb.t
    val ellipsis : Token.t Comb.t
    val eq : Token.t Comb.t
    val eq_eq : Token.t Comb.t
    val eq_eq_eq : Token.t Comb.t
    val eq_op : Token.t Comb.t
    val gt : Token.t Comb.t
    val gt_eq : Token.t Comb.t
    val hash : Token.t Comb.t
    val hash_eq : Token.t Comb.t
    val l_brace : Token.t Comb.t
    val l_bracket : Token.t Comb.t
    val l_paren : Token.t Comb.t
    val list : Token.t Comb.t
    val lt : Token.t Comb.t
    val lt_eq : Token.t Comb.t
    val minus : Token.t Comb.t
    val minus_dot : Token.t Comb.t
    val minus_gt : Token.t Comb.t
    val percent : Token.t Comb.t
    val percent_percent : Token.t Comb.t
    val pipe : Token.t Comb.t
    val pipe_gt : Token.t Comb.t
    val pipe_pipe : Token.t Comb.t
    val plus : Token.t Comb.t
    val plus_dot : Token.t Comb.t
    val plus_eq : Token.t Comb.t
    val plus_plus : Token.t Comb.t
    val question : Token.t Comb.t
    val r_brace : Token.t Comb.t
    val r_bracket : Token.t Comb.t
    val r_paren : Token.t Comb.t
    val slash : Token.t Comb.t
    val slash_dot : Token.t Comb.t
    val tilda : Token.t Comb.t

    val ident : string Comb.t
    val l_ident : string Comb.t
    val u_ident : string Comb.t
    val type_var : string Comb.t
    val string_raw : string Comb.t
    val interpolated_string : quote_tag:string -> expression:(expression Comb.t) -> expression Comb.t
    val string_ident : string Comb.t

    val integer : (string * char option) Comb.t
    val number : constant Comb.t
    val character : constant Comb.t

    val string : constant Comb.t
    val string_multiline : q:string -> constant Comb.t

    val constant : constant Comb.t

    val ng : unit Comb.t
    val del_pos : Lexing.position Comb.t
    val del : unit Comb.t
end

module type BASIC = sig
    include BASIC_BASE

    module type CORE = CORE with module Comb = Comb
    module type MODEXPR = MODEXPR with module Comb = Comb
    module type EXPRESSION = EXPRESSION with module Comb = Comb
    module type PATTERN = PATTERN with module Comb = Comb
    module type TYPE = TYPE with module Comb = Comb
    module type MODTYPE = MODTYPE with module Comb = Comb

    val ng_no_new_line : unit Comb.t
    val ng_new_line : unit Comb.t
    val sep : unit Comb.t
    val parens : 'a Comb.t -> 'a Comb.t
    val brackets : 'a Comb.t -> 'a Comb.t
    val braces : 'a Comb.t -> 'a Comb.t
    val chevrons : 'a Comb.t -> 'a Comb.t

    val with_del : (Location.t -> 'a) Comb.t -> 'a Comb.t
    val ng_not_empty : unit Comb.t

    val longident : Longident.t Comb.t
    val u_longident : Longident.t Comb.t
    val l_longident : Longident.t Comb.t
    val attribute_id : string Comb.t
    val variant_tag : string Comb.t

    val na_hlp : (?loc:Warnings.loc -> 'a -> 'b) -> ('a -> Location.t -> 'b) Comb.t
    val hlp : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) -> ('a -> Location.t -> 'b) Comb.t
    val hlp2 : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c ) -> ('a -> 'b -> Location.t -> 'c) Comb.t
    val hlp3 : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) -> ('a -> 'b -> 'c -> Location.t -> 'd) Comb.t
    val hlp4 : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) -> ('a -> 'b -> 'c -> 'd -> Location.t -> 'e) Comb.t

    val hlp_a : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) -> (attributes -> 'a -> Location.t -> 'b) Comb.t
    val hlp2_a : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c ) -> (attributes -> 'a -> 'b -> Location.t -> 'c) Comb.t
    val hlp3_a : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) -> (attributes -> 'a -> 'b -> 'c -> Location.t -> 'd) Comb.t
    val hlp4_a : (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) -> (attributes -> 'a -> 'b -> 'c -> 'd -> Location.t -> 'e) Comb.t
end
