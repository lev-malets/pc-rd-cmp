open Base

module type PARSER = sig
    type s

    module Angstrom: Angstrom_mod.Sigs.PARSER with type s = s State.t

    type _ typ =
        | Parser : _ typ
        | Return : 'a -> 'a typ
        | Value :
            { v: 'a
            ; p: _ Angstrom.t
            } -> 'a typ
        | Lift :
            { f: 'a -> 'b
            ; a: 'a Angstrom.t
            } -> 'b typ
        | Lift2 :
            { f: 'a -> 'b -> 'c
            ; a: 'a Angstrom.t
            ; b: 'b Angstrom.t
            } -> 'c typ
        | Lift3 :
            { f: 'a -> 'b -> 'c -> 'd
            ; a: 'a Angstrom.t
            ; b: 'b Angstrom.t
            ; c: 'c Angstrom.t
            } -> 'd typ

    type info =
        | Unknown
        | Empty
        | Consume of
            { empty: bool
            ; first: Charset.t
            }

    type 'a t =
        { p: 'a Angstrom.t
        ; info: info
        ; typ: 'a typ
        ; id : int
        }
end

module MkParser(T: sig type s end): PARSER with type s = T.s = struct
    type s = T.s

    module Angstrom = Angstrom_mod.Sigs.MkParser(struct type s = T.s State.t end)

    type _ typ =
        | Parser : _ typ
        | Return : 'a -> 'a typ
        | Value :
            { v: 'a
            ; p: _ Angstrom.t
            } -> 'a typ
        | Lift :
            { f: 'a -> 'b
            ; a: 'a Angstrom.t
            } -> 'b typ
        | Lift2 :
            { f: 'a -> 'b -> 'c
            ; a: 'a Angstrom.t
            ; b: 'b Angstrom.t
            } -> 'c typ
        | Lift3 :
            { f: 'a -> 'b -> 'c -> 'd
            ; a: 'a Angstrom.t
            ; b: 'b Angstrom.t
            ; c: 'c Angstrom.t
            } -> 'd typ

    type info =
        | Unknown
        | Empty
        | Consume of
            { empty: bool
            ; first: Charset.t
            }

    type 'a t =
        { p: 'a Angstrom.t
        ; info: info
        ; typ: 'a typ
        ; id : int
        }
end


module type POS = sig
    module Parser: PARSER
    module Angstrom : sig
        include Angstrom_mod.Sigs.ANGSTROM with module Parser = Parser.Angstrom

        val (>>$) : _ Parser.t -> 'a -> 'a Parser.t
        val exec : (unit -> 'a) -> 'a Parser.t
    end

    type 'b getter = { get: 'a. ('b -> 'a Parser.t) -> 'a Parser.t }

    module Id : sig
        val fail : int

        val next : int ref
        val get : unit -> int
    end

    val (>>) : _ Parser.t -> 'a Parser.t -> 'a Parser.t
    val (<<) : 'a Parser.t -> _ Parser.t -> 'a Parser.t
    val (<|>) : 'a Parser.t -> 'a Parser.t -> 'a Parser.t

    val (>>$) : _ Parser.t -> 'a -> 'a Parser.t
    val (>>=) : 'a Parser.t -> ('a -> 'b Angstrom.Parser.t) -> 'b Parser.t
    val (>>|) : 'a Parser.t -> ('a -> 'b) -> 'b Parser.t

    val parse_string : 'a Parser.t -> Parser.s -> ?filename:string -> string -> 'a option

    val fix : ('a Parser.t -> 'a Parser.t) -> 'a Parser.t
    val fix_poly : ('x getter -> 'x) -> 'x

    val return : 'a -> 'a Parser.t
    val advance : int -> unit Parser.t
    val fail : _ Parser.t
    val any_char : char Parser.t
    val peek_char : char option Parser.t
    val s : string -> string Parser.t

    val take_while : (char -> bool) -> string Parser.t
    val take_while1 : (char -> bool) -> string Parser.t

    val skip : (char -> bool) -> unit Parser.t
    val skip_while : (char -> bool) -> unit Parser.t
    val satisfy : (char -> bool) -> char Parser.t

    val pos : Lexing.position Parser.t
    val new_line : string Parser.t
    val whitespace : unit Parser.t

    val with_literal : 'a Parser.t -> ('a * string) Parser.t

    val consumed : _ Parser.t -> string Parser.t
    val state_get : Parser.s Parser.t
    val state_map : (Parser.s -> Parser.s) -> unit Parser.t
    val state_set : Parser.s -> unit Parser.t

    val failed : _ Parser.t -> unit Parser.t
    val opt : 'a Parser.t -> 'a option Parser.t
    val seq : ?n:int -> ?sep:(_ Parser.t) -> ?trail:bool -> 'a Parser.t -> 'a list Parser.t
    val trail : bool

    val make_location : Lexing.position -> Lexing.position -> Location.t
    val loc_comb : Location.t -> Location.t -> Location.t

    val (+) : ('a -> 'b) Parser.t -> 'a Parser.t -> 'b Parser.t
    val (-) : 'a Parser.t -> _ Parser.t -> 'a Parser.t

    val mapping : ('a -> 'b) -> ('a -> 'b) Parser.t

    val loc : 'a Parser.t -> 'a Location.loc Parser.t
    val loc_of : 'a Parser.t -> Location.t Parser.t

    val t2 : 'a -> 'b -> 'a * 'b
    val t3 : 'a -> 'b -> 'c -> 'a * 'b * 'c
    val t4 : 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
    val cons : 'a -> 'a list -> 'a list

    val exec : (unit -> 'a) -> 'a Parser.t

    val fold_left_0_n : f:('a -> 'b -> 'a) -> 'a Parser.t -> 'b Parser.t -> 'a Parser.t
    val fold_left_0_1 : f:('a -> 'b -> 'a) -> 'a Parser.t -> 'b Parser.t -> 'a Parser.t
    val fold_left_cont_0_n : 'a Parser.t -> ('a -> 'a) Parser.t -> 'a Parser.t
    val fold_left_cont_0_1 : 'a Parser.t -> ('a -> 'a) Parser.t -> 'a Parser.t

    val (&&) : 'a Parser.t -> ('a -> 'b) Parser.t -> 'b Parser.t
    val (||) : 'a Parser.t -> 'a Parser.t -> 'a Parser.t
    val (&) : ('a -> 'b) -> 'a -> 'b

    val with_loc : (Location.t -> 'a) Parser.t -> 'a Parser.t
    val peek_first : 'a Parser.t list -> 'a Parser.t

    val memo : 'a Parser.t -> 'a Parser.t
    val memoid2id : (int, int) Hashtbl.t
    val id2memoid : (int, int) Hashtbl.t

    val named : string -> 'a Parser.t -> 'a Parser.t
    val name2id : (string, int) Hashtbl.t
    val id2name : (int, string) Hashtbl.t

    val name_of_id : int -> string
    val name_of : 'a Parser.t -> string
end
