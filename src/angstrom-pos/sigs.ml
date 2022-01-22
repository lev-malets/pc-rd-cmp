
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
        }
end


module type POS = sig
    module Parser: PARSER
    module Angstrom : sig
        include Angstrom_mod.Sigs.ANGSTROM with module Parser = Parser.Angstrom
        val exec : (unit -> 'a) -> 'a Parser.t
    end
    open Parser

    type 'b getter = { get: 'a. ('b -> 'a t) -> 'a t }

    val pos : Lexing.position t

    val state_get : s t
    val state_map : (s -> s) -> unit t
    val state_set : s -> unit t

    val fail : _ t

    val (<|>) : 'a t -> 'a t -> 'a t
    val exec : (unit -> 'a) -> 'a t
end

module type NAMED = sig
    module Parser : PARSER

    val p : string -> 'a Parser.t -> 'a Parser.t
end

module type TRACED = sig
    include NAMED

    val tt: Exec_info.t
end

module type PEEK = sig
    module Parser : PARSER

    val first : 'a Parser.t list -> 'a Parser.t
end

module type CHARSET = sig
    type t
    val empty : t
    val full : t

    val add : t -> char -> t
    val union : t -> t -> t
    val equal : t -> t -> bool

    val of_list : char list -> t

    val singleton : char -> t
    val range : char -> char -> t

    val iter_code : (int -> unit) -> t -> unit
end
