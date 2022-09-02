open Base

module type TOKENIZER = sig
  module Tag : sig
    include Equal.S

    val show : t -> string
  end

  val scan_space : Lexing.lexbuf -> unit
  val scan_token : Lexing.lexbuf -> Tag.t Parser.scan_result
end

module type TPC = sig
  type tag
  type s

  include
    Pc.COMB
      with type Conf.Log.elem = s
       and type 'a Simple.t = ('a, tag, s) Parser.simple
       and type 'a t = ('a, tag, s) Parser.t

  val eof : unit t
  val tkn : tag -> (tag * 'a) t
  val tkn_ : tag -> unit t
  val tkn_tag : tag -> tag t
  val tkn_payload : tag -> 'a t
  val peek : 'a t -> 'a t
end
