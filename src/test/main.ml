
module M (P: sig type t;; val to_string: t -> string end) = struct
    let f1: P.t -> P.t -> int -> unit = fun t1 t2 i -> print_endline @@ P.to_string t1 ^ " " ^ P.to_string t2 ^ " " ^ string_of_int i

    type t = {
        t1: P.t;
        t2: P.t;
        i: int;
    }

    let f2: t -> unit = fun t -> print_endline @@ P.to_string t.t1 ^ " " ^ P.to_string t.t2 ^ " " ^ string_of_int t.i
end


module M1 = M(
    struct
        type t = float
        let to_string t = string_of_float t
    end
)

let main =
    let f = float_of_string @@ Sys.argv.(1) in
    M1.f1 f f 0;
    M1.f2 {t1=f; t2=f; i=1};
