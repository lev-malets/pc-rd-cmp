
module type POS = sig
    include Angstrom_mod.Sigs.ANGSTROM
    open Parser
    type state

    val (>>) : _ t -> 'a t -> 'a t
    val (<<) : 'a t -> _ t -> 'a t
    val (>>$) : _ t -> 'a -> 'a t

    val get_state : state t
    val set_state : state -> unit t
    val map_state : (state -> state) -> unit t

    val parse_string : 'a t -> state -> ?filename:string -> string -> ('a, string) result
    val parse_consumed : consumer:(_ t) -> 'a t -> 'a t

    val newline_skipped : unit t
    val whitespace : unit t
    val whitespace_nepu : unit t
    val position : Lexing.position t
    val end_position : Lexing.position t

    module Alt : sig
        val position : Lexing.position t
        val end_position : Lexing.position t
    end
end
