type t =
    { w1 : Int64.t
    ; w2 : Int64.t
    ; w3 : Int64.t
    ; w4 : Int64.t
    }
let empty = { w1 = Int64.zero; w2 = Int64.zero; w3 = Int64.zero; w4 = Int64.zero }
let full = { w1 = Int64.minus_one; w2 = Int64.minus_one; w3 = Int64.minus_one; w4 = Int64.minus_one }

let add t c =
    let code = Char.code c in
    match [@warning "-8"] code lsr 6 with
    | 0 -> { t with w1 = Int64.logor t.w1 (Int64.shift_left Int64.one code) }
    | 1 -> { t with w2 = Int64.logor t.w2 (Int64.shift_left Int64.one (code - 64)) }
    | 2 -> { t with w3 = Int64.logor t.w3 (Int64.shift_left Int64.one (code - 128)) }
    | 3 -> { t with w4 = Int64.logor t.w4 (Int64.shift_left Int64.one (code - 192)) }

let union t1 t2 =
    { w1 = Int64.logor t1.w1 t2.w1
    ; w2 = Int64.logor t1.w2 t2.w2
    ; w3 = Int64.logor t1.w3 t2.w3
    ; w4 = Int64.logor t1.w4 t2.w4
    }
let equal t1 t2 =
    Int64.(equal t1.w1 t2.w1 && equal t1.w2 t2.w2 && equal t1.w3 t2.w3 && equal t1.w4 t2.w4)

let of_list = List.fold_left add empty

let singleton = add empty
let range a b =
    let acode = Char.code a in
    let bcode = Char.code b in

    match [@warning "-8"] acode lsr 6, bcode lsr 6 with
    | _, 0 ->
        { empty with w1 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode) }
    | 0, 1 ->
        let bcode = bcode - 64 in
        { empty with
            w1 = Int64.(shift_left minus_one acode);
            w2 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
        }
    | 1, 1 ->
        let bcode = bcode - 64 in
        let acode = acode - 64 in
        { empty with
            w2 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode);
        }
    | 0, 2 ->
        let bcode = bcode - 128 in
        { empty with
            w1 = Int64.(shift_left minus_one acode);
            w2 = Int64.minus_one;
            w3 = Int64.(shift_left minus_one (bcode + 1) |> lognot);

        }
    | 1, 2 ->
        let bcode = bcode - 128 in
        let acode = acode - 64 in
        { empty with
            w2 = Int64.(shift_left minus_one acode);
            w3 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
        }
    | 2, 2 ->
        let bcode = bcode - 128 in
        let acode = acode - 128 in
        { empty with
            w3 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode);
        }
    | 0, 3 ->
        let bcode = bcode - 192 in
        {
            w1 = Int64.(shift_left minus_one acode);
            w2 = Int64.minus_one;
            w3 = Int64.minus_one;
            w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
        }
    | 1, 3 ->
        let bcode = bcode - 192 in
        let acode = acode - 64 in
        { empty with
            w2 = Int64.(shift_left minus_one acode);
            w3 = Int64.minus_one;
            w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
        }
    | 2, 3 ->
        let bcode = bcode - 192 in
        let acode = acode - 128 in
        { empty with
            w3 = Int64.(shift_left minus_one acode);
            w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
        }
    | 3, 3 ->
        let bcode = bcode - 192 in
        let acode = acode - 192 in
        { empty with
            w4 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode);
        }


let iter_code f t =
    let open Int64 in
    let rec loop4 w i =
        if equal w zero then ()
        else begin
            if equal (logand one w) one then f i;
            loop4 (shift_right_logical w 1) (i + 1)
        end
    in
    let rec loop3 w i =
        if equal w zero then loop4 t.w4 192
        else begin
            if equal (logand one w) one then f i;
            loop3 (shift_right_logical w 1) (i + 1)
        end
    in
    let rec loop2 w i =
        if equal w zero then loop3 t.w3 128
        else begin
            if equal (logand one w) one then f i;
            loop2 (shift_right_logical w 1) (i + 1)
        end
    in
    let rec loop1 w i =
        if equal w zero then loop2 t.w2 64
        else begin
            if equal (logand one w) one then f i;
            loop1 (shift_right_logical w 1) (i + 1)
        end
    in
    loop1 t.w1 0
