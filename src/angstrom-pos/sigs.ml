
module type POS = sig
    include Angstrom_mod.Sigs.ANGSTROM

    type s

    type 'b getter = { get: 'a. ('b -> 'a Parser.t) -> 'a Parser.t }

    val ( >> ) : _ Parser.t -> 'a Parser.t -> 'a Parser.t
    val ( << ) : 'a Parser.t -> _ Parser.t -> 'a Parser.t

    val parse_string : 'a Parser.t -> s -> ?filename:string -> string -> ('a, string) result

    val fix_poly : ('a getter -> 'a) -> 'a

    val pos : Lexing.position Parser.t

    val new_line : string Parser.t

    val whitespace : unit Parser.t

    val state_get : s Parser.t
    val state_map : (s -> s) -> unit Parser.t
    val state_set : s -> unit Parser.t

    val failed : _ Parser.t -> unit Parser.t

    val memo : 'a Parser.t -> 'a Parser.t
end

module type NAMED = sig
    module Parser : Angstrom_mod.Sigs.PARSER

    val p : string -> 'a Parser.t -> 'a Parser.t
end

module type TRACED = sig
    include NAMED

    val tt: Exec_info.t
end

module type PEEK = sig
    module Parser : Angstrom_mod.Sigs.PARSER

    val char_fail : (char -> 'a Parser.t) -> expected:(char list) -> 'a Parser.t
end
