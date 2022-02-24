open Base

module type POS = sig
    type s
    module Angstrom : sig
        include module type of Angstrom

        val (>>) : _ t -> 'a t -> 'a t
        val (<<) : 'a t -> _ t -> 'a t
        val (>>$) : _ t -> 'a -> 'a t
        val exec : (unit -> 'a) -> 'a t

        val log : s -> unit t
        val log_many : s list -> unit t
    end

    type 'a t = 'a Parser.t

    type 'b getter = { get: 'a. ('b -> 'a t) -> 'a t }

    module Id : sig
        val fail : int

        val next : int ref
        val get : unit -> int
    end

    val (>>) : _ t -> 'a t -> 'a t
    val (<<) : 'a t -> _ t -> 'a t
    val (<|>) : 'a t -> 'a t -> 'a t

    val (>>$) : _ t -> 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b Angstrom.t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val parse_string : 'a t -> ?filename:string -> string -> 'a option

    val fix : ('a t -> 'a t) -> 'a t
    val fix_poly : ('x getter -> 'x) -> 'x

    val return : 'a -> 'a t
    val advance : int -> unit t
    val fail : _ t
    val any_char : char t
    val peek_char : char option t
    val s : string -> string t

    val take_while : (char -> bool) -> string t
    val take_while1 : (char -> bool) -> string t

    val skip : (char -> bool) -> unit t
    val skip_while : (char -> bool) -> unit t
    val satisfy : (char -> bool) -> char t

    val pos : Lexing.position t
    val new_line : string t
    val whitespace : unit t

    val with_literal : 'a t -> ('a * string) t

    val consumed : _ t -> string t
    val fold_log : 'a -> ('a -> s -> 'a) -> 'a t
    val failed : _ t -> unit t
    val opt : 'a t -> 'a option t
    val seq : ?n:int -> ?sep:(_ t) -> ?trail:bool -> 'a t -> 'a list t
    val trail : bool

    val make_location : Lexing.position -> Lexing.position -> Location.t
    val loc_comb : Location.t -> Location.t -> Location.t

    val (+) : ('a -> 'b) t -> 'a t -> 'b t
    val (-) : 'a t -> _ t -> 'a t

    val mapping : ('a -> 'b) -> ('a -> 'b) t

    val loc : 'a t -> 'a Location.loc t
    val loc_of : 'a t -> Location.t t

    val t2 : ('a -> 'b -> 'a * 'b) t
    val t3 : ('a -> 'b -> 'c -> 'a * 'b * 'c) t
    val t4 : ('a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd) t
    val cons : ('a -> 'a list -> 'a list) t

    val exec : (unit -> 'a) -> 'a t

    val fold_left_0_n : f:('a -> 'b -> 'a) -> 'a t -> 'b t -> 'a t
    val fold_left_0_1 : f:('a -> 'b -> 'a) -> 'a t -> 'b t -> 'a t
    val fold_left_cont_0_n : 'a t -> ('a -> 'a) t -> 'a t
    val fold_left_cont_0_1 : 'a t -> ('a -> 'a) t -> 'a t

    val (&&) : 'a t -> ('a -> 'b) t -> 'b t
    val (||) : 'a t -> 'a t -> 'a t
    val (&) : ('a -> 'b) -> 'a -> 'b

    val with_loc : (Location.t -> 'a) t -> 'a t
    val peek_first : 'a t list -> 'a t

    val memo : 'a t -> 'a t
    val memoid2id : (int, int) Hashtbl.t
    val id2memoid : (int, int) Hashtbl.t

    val named : string -> 'a t -> 'a t
    val name2id : (string, int) Hashtbl.t
    val id2name : (int, string) Hashtbl.t

    val name_of_id : int -> string
    val name_of : 'a t -> string

    val print_info : _ t -> unit

    module Expose : sig
        val make_position : int -> Lexing.position
    end
end
