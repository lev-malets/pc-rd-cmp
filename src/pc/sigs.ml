open Base

module type CONF_LOG = sig
  type elem
end

type regexp_pair = {accept : Str.regexp; decline : Str.regexp}

module Config = struct
  module Peek = struct
    module Auto = struct
      type t = Disable | Enable of {min_variants : int}

      let default_min_variants = 3
      let default = Disable
      let default_enable = Enable {min_variants = default_min_variants}
    end

    type t = {filter : regexp_pair; auto : Auto.t}
  end

  type t =
    {debug : bool; memoize : regexp_pair; trace : regexp_pair; peek : Peek.t}
end

module type CONF = sig
  module Log : CONF_LOG

  val config : Config.t
end

module type COMB_COMMON = sig
  module Conf : CONF

  module Simple : sig
    type 'a t

    val ( >> ) : _ t -> 'a t -> 'a t
    val ( << ) : 'a t -> _ t -> 'a t
    val ( >>$ ) : _ t -> 'a -> 'a t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : _ t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val exec : (unit -> 'a) -> 'a t
    val log : Conf.Log.elem -> unit t
    val log_many : Conf.Log.elem list -> unit t
  end

  type 'a t
  type 'b getter = {get : 'a 'c. ?info:'c t -> ('b -> 'a t) -> 'a t}

  val simple : 'a t -> 'a Simple.t
  val ( >> ) : _ t -> 'a t -> 'a t
  val ( << ) : 'a t -> _ t -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( >>$ ) : _ t -> 'a -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val parse_string : 'a t -> ?filename:string -> string -> 'a option
  val fix_gen : ('x getter -> 'x) -> 'x
  val return : 'a -> 'a t
  val fail : _ t
  val pos : Compilerlibs406.Lexing.position t
  val pos_end : Compilerlibs406.Lexing.position t
  val fold_log : 'a -> ('a -> Conf.Log.elem -> 'a) -> 'a t
  val failed : _ t -> unit t
  val opt : 'a t -> 'a option t
  val seq : ?n:int -> ?sep:_ t -> ?trail:bool -> 'a t -> 'a list t
  val eof : unit t
  val t2 : ('a -> 'b -> 'a * 'b) t
  val t3 : ('a -> 'b -> 'c -> 'a * 'b * 'c) t
  val t4 : ('a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd) t
  val cons : ('a -> 'a list -> 'a list) t
  val run : 'a Simple.t t -> 'a t
  val exec : (unit -> 'a) -> 'a t
  val print_info : _ t -> unit
  val peek_first : 'a t list -> 'a t
  val memo : 'a t -> 'a t
  val traced : 'a t -> 'a t
  val modify : simple:'a Simple.t -> _ t -> 'a t
  val touch : 'a t -> 'a t
  val not_empty : _ t -> bool
  val id : _ t -> int

  val parse_string_with_trace :
    'a t -> ?filename:string -> string -> 'a option * Exec_info.entry list
end

module type COMB_BASE = sig
  include COMB_COMMON

  val ( <|> ) : 'a t -> 'a t -> 'a t
  val first_size : _ t -> int option
  val first_size_max : int
  val first_iter : f:(int -> unit) -> _ t -> unit
end

module type COMB = sig
  include COMB_COMMON

  val fix : ?info:_ t -> ('a t -> 'a t) -> 'a t
  val mapping : ('a -> 'b) -> ('a -> 'b) t
  val fold_left_0_n : f:('a -> 'b -> 'a) -> 'a t -> 'b t -> 'a t
  val fold_left_0_1 : f:('a -> 'b -> 'a) -> 'a t -> 'b t -> 'a t
  val fold_left_cont_0_n : 'a t -> ('a -> 'a) t -> 'a t
  val fold_left_cont_0_1 : 'a t -> ('a -> 'a) t -> 'a t
  val ( && ) : 'a t -> ('a -> 'b) t -> 'b t
  val choice : ?name:string -> 'a t list -> 'a t
  val loc : 'a t -> 'a Compilerlibs406.Location.loc t
  val loc_of : 'a t -> Compilerlibs406.Location.t t
  val with_loc : (Compilerlibs406.Location.t -> 'a) t -> 'a t
  val memoid2id : (int, int) Hashtbl.t
  val id2memoid : (int, int) Hashtbl.t
  val named : string -> 'a t -> 'a t
  val name2id : (string, int) Hashtbl.t
  val id2name : (int, string) Hashtbl.t
  val name_of_id : int -> string
  val name_of : 'a t -> string
end
