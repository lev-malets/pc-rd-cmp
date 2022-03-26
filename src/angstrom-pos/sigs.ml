open Base

module type POS = sig
    type s

    include Pc.Sigs.COMB
        with type log_elem = s
        and type 'a Simple.t = 'a Angstrom.t
        and type 'a t = 'a Parser.t

    module Id : sig
        val get : unit -> int
    end

    val advance : int -> unit t
    val any_char : char t
    val peek_char : char option t
    val s : string -> string t

    val take_while : (char -> bool) -> string t
    val take_while1 : (char -> bool) -> string t

    val skip : (char -> bool) -> unit t
    val skip_while : (char -> bool) -> unit t
    val satisfy : (char -> bool) -> char t

    val new_line : string t
    val whitespace : unit t

    val with_literal : 'a t -> ('a * string) t

    val consumed : _ t -> string t

    module Expose : sig
        val make_position : int -> Lexing.position
    end
end
