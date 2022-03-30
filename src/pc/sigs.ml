open Base

module type COMB = sig
    type log_elem

    module Simple : sig
        type 'a t

        val (>>) : _ t -> 'a t -> 'a t
        val (<<) : 'a t -> _ t -> 'a t
        val (>>$) : _ t -> 'a -> 'a t
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
        val return : 'a -> 'a t
        val fail : _ t
        val (<|>) : 'a t -> 'a t -> 'a t

        val exec : (unit -> 'a) -> 'a t
        val log : log_elem -> unit t
        val log_many : log_elem list -> unit t
    end

    type 'a t

    type 'b getter = { get: 'a. ('b -> 'a t) -> 'a t }

    val simple : 'a t -> 'a Simple.t

    val (>>) : _ t -> 'a t -> 'a t
    val (<<) : 'a t -> _ t -> 'a t
    val (<|>) : 'a t -> 'a t -> 'a t

    val (>>$) : _ t -> 'a -> 'a t
    val (>>=) : 'a t -> ('a -> 'b Simple.t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t

    val parse_string : 'a t -> ?filename:string -> string -> 'a option

    val fix : ('a t -> 'a t) -> 'a t
    val fix_poly : ('x getter -> 'x) -> 'x

    val return : 'a -> 'a t
    val fail : _ t

    val pos : Lexing.position t
    val pos_end : Lexing.position t

    val fold_log : 'a -> ('a -> log_elem -> 'a) -> 'a t
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

    val run : 'a Simple.t t -> 'a t
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

    val id : 'a t -> int
end

module type TRACE = sig
    val entries : Exec_info.entry list ref
end
