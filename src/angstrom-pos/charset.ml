open Z

type t = Z.t
let mod_ = one lsl 256
let empty = zero
let full = ~!mod_

let add t c = t lor (one lsl (Char.code c))
let union t1 t2 = t1 lor t2
let equal = Z.equal

let of_list = List.fold_left add empty

let singleton = add empty
let range a b =
    let end_ = one lsl Pervasives.(Char.code b - Char.code a + 1) in
    invert zero end_ lsl Char.code a

let iter_code f t =
    let rec loop t i =
        if Z.compare t zero = 0 then ()
        else begin
            if testbit t 0 then f i;
            loop (t asr 1) Pervasives.(i + 1)
        end
    in
    loop t 0
