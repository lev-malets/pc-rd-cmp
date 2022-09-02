open Compilerlibs406
open Parsetree
open Basic

module type CONF = Pc.CONF with type Log.elem = LogElement.t
module type COMB = Pc.COMB with type Conf.Log.elem = LogElement.t

module type PARSE = sig
  val parse_interface :
    src:string -> filename:string -> signature parse_result option

  val parse_implementation :
    src:string -> filename:string -> structure parse_result option
end

module type PARSER = sig
  include PARSE
  module Comb : COMB

  val signature_parser : signature Comb.t
  val structure_parser : structure Comb.t
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

  val modtype : module_type Comb.t
  val modtype_functor : module_type Comb.t
  val modtype_with : module_type Comb.t
end

module type MODEXPR = sig
  module Comb : COMB

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

  val and' : unit Comb.t
  val as' : unit Comb.t
  val async : unit Comb.t
  val assert' : unit Comb.t
  val await : unit Comb.t
  val catch : unit Comb.t
  val constraint' : unit Comb.t
  val downto' : unit Comb.t
  val else' : unit Comb.t
  val exception' : unit Comb.t
  val export : unit Comb.t
  val external' : unit Comb.t
  val false' : unit Comb.t
  val for' : unit Comb.t
  val from : unit Comb.t
  val if' : unit Comb.t
  val in' : unit Comb.t
  val include' : unit Comb.t
  val import : unit Comb.t
  val json_tag : unit Comb.t
  val lazy' : unit Comb.t
  val let' : unit Comb.t
  val module' : unit Comb.t
  val mutable' : unit Comb.t
  val nonrec' : unit Comb.t
  val of' : unit Comb.t
  val open' : unit Comb.t
  val private' : unit Comb.t
  val rec' : unit Comb.t
  val sig' : unit Comb.t
  val switch : unit Comb.t
  val to' : unit Comb.t
  val true' : unit Comb.t
  val try' : unit Comb.t
  val type' : unit Comb.t
  val unpack : unit Comb.t
  val when' : unit Comb.t
  val while' : unit Comb.t
  val with' : unit Comb.t
  val _' : unit Comb.t
  val ampersand : unit Comb.t
  val ampersand_ampersand : unit Comb.t
  val arrow : unit Comb.t
  val asterisk : unit Comb.t
  val asterisk_asterisk : unit Comb.t
  val asterisk_dot : unit Comb.t
  val at : unit Comb.t
  val at_at : unit Comb.t
  val bang : unit Comb.t
  val bang_eq : unit Comb.t
  val bang_eq_eq : unit Comb.t
  val colon : unit Comb.t
  val colon_eq : unit Comb.t
  val colon_gt : unit Comb.t
  val comma : unit Comb.t
  val dot : unit Comb.t
  val dot_dot : unit Comb.t
  val ellipsis : unit Comb.t
  val eq : unit Comb.t
  val eq_eq : unit Comb.t
  val eq_eq_eq : unit Comb.t
  val eq_op : unit Comb.t
  val gt : unit Comb.t
  val gt_eq : unit Comb.t
  val hash : unit Comb.t
  val hash_eq : unit Comb.t
  val l_brace : unit Comb.t
  val l_bracket : unit Comb.t
  val l_paren : unit Comb.t
  val list : unit Comb.t
  val lt : unit Comb.t
  val lt_eq : unit Comb.t
  val minus : unit Comb.t
  val minus_dot : unit Comb.t
  val minus_gt : unit Comb.t
  val percent : unit Comb.t
  val percent_percent : unit Comb.t
  val pipe : unit Comb.t
  val pipe_gt : unit Comb.t
  val pipe_pipe : unit Comb.t
  val plus : unit Comb.t
  val plus_dot : unit Comb.t
  val plus_eq : unit Comb.t
  val plus_plus : unit Comb.t
  val question : unit Comb.t
  val r_brace : unit Comb.t
  val r_bracket : unit Comb.t
  val r_paren : unit Comb.t
  val slash : unit Comb.t
  val slash_dot : unit Comb.t
  val tilda : unit Comb.t
  val ident : string Comb.t
  val l_ident : string Comb.t
  val u_ident : string Comb.t
  val type_var : string Comb.t
  val string_raw : string Comb.t

  val template :
    quote_tag:string -> expression:expression Comb.t -> expression Comb.t

  val string_ident : string Comb.t
  val integer : (string * char option) Comb.t
  val number : constant Comb.t
  val character : constant Comb.t
  val string_multiline : constant Comb.t
  val template_no_template : constant Comb.t
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

  val na_hlp :
    (?loc:Warnings.loc -> 'a -> 'b) -> ('a -> Location.t -> 'b) Comb.t

  val hlp :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) ->
    ('a -> Location.t -> 'b) Comb.t

  val hlp2 :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) ->
    ('a -> 'b -> Location.t -> 'c) Comb.t

  val hlp3 :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) ->
    ('a -> 'b -> 'c -> Location.t -> 'd) Comb.t

  val hlp4 :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) ->
    ('a -> 'b -> 'c -> 'd -> Location.t -> 'e) Comb.t

  val hlp_a :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) ->
    (attributes -> 'a -> Location.t -> 'b) Comb.t

  val hlp2_a :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) ->
    (attributes -> 'a -> 'b -> Location.t -> 'c) Comb.t

  val hlp3_a :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) ->
    (attributes -> 'a -> 'b -> 'c -> Location.t -> 'd) Comb.t

  val hlp4_a :
    (?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e) ->
    (attributes -> 'a -> 'b -> 'c -> 'd -> Location.t -> 'e) Comb.t

  module Sugar : sig
    val async : expression Comb.t -> expression Comb.t
    val await : expression Comb.t -> expression Comb.t
    val optional : expression Comb.t -> expression Comb.t
    val optional1 : ('a * expression) Comb.t -> ('a * expression) Comb.t
    val optional_pat : pattern Comb.t -> pattern Comb.t
    val optional1_pat : ('a * pattern) Comb.t -> ('a * pattern) Comb.t
  end
end
