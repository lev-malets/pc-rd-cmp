type t = { w1 : Int64.t; w2 : Int64.t; w3 : Int64.t; w4 : Int64.t }

type word_no = W1 | W2 | W3 | W4

let word_no_of_code c =
  match c lsr 6 with 0 -> W1 | 1 -> W2 | 2 -> W3 | 3 -> W4 | _ -> failwith @@ Printf.sprintf "invalid code: %d" c

let empty = { w1 = Int64.zero; w2 = Int64.zero; w3 = Int64.zero; w4 = Int64.zero }

let full = { w1 = Int64.minus_one; w2 = Int64.minus_one; w3 = Int64.minus_one; w4 = Int64.minus_one }

let add t code =
  match word_no_of_code code with
  | W1 -> { t with w1 = Int64.logor t.w1 (Int64.shift_left Int64.one code) }
  | W2 -> { t with w2 = Int64.logor t.w2 (Int64.shift_left Int64.one (code - 64)) }
  | W3 -> { t with w3 = Int64.logor t.w3 (Int64.shift_left Int64.one (code - 128)) }
  | W4 -> { t with w4 = Int64.logor t.w4 (Int64.shift_left Int64.one (code - 192)) }

let union t1 t2 =
  {
    w1 = Int64.logor t1.w1 t2.w1;
    w2 = Int64.logor t1.w2 t2.w2;
    w3 = Int64.logor t1.w3 t2.w3;
    w4 = Int64.logor t1.w4 t2.w4;
  }

let equal t1 t2 = Int64.(equal t1.w1 t2.w1 && equal t1.w2 t2.w2 && equal t1.w3 t2.w3 && equal t1.w4 t2.w4)

let of_list = List.fold_left add empty

let singleton = add empty

let range a b =
  let acode = a in
  let bcode = b in
  assert (acode <= bcode);

  match (word_no_of_code acode, word_no_of_code bcode) with
  | W1, W1 -> { empty with w1 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode) }
  | W1, W2 ->
      let bcode = bcode - 64 in
      { empty with w1 = Int64.(shift_left minus_one acode); w2 = Int64.(shift_left minus_one (bcode + 1) |> lognot) }
  | W2, W2 ->
      let bcode = bcode - 64 in
      let acode = acode - 64 in
      { empty with w2 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode) }
  | W1, W3 ->
      let bcode = bcode - 128 in
      {
        empty with
        w1 = Int64.(shift_left minus_one acode);
        w2 = Int64.minus_one;
        w3 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
      }
  | W2, W3 ->
      let bcode = bcode - 128 in
      let acode = acode - 64 in
      { empty with w2 = Int64.(shift_left minus_one acode); w3 = Int64.(shift_left minus_one (bcode + 1) |> lognot) }
  | W3, W3 ->
      let bcode = bcode - 128 in
      let acode = acode - 128 in
      { empty with w3 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode) }
  | W1, W4 ->
      let bcode = bcode - 192 in
      {
        w1 = Int64.(shift_left minus_one acode);
        w2 = Int64.minus_one;
        w3 = Int64.minus_one;
        w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
      }
  | W2, W4 ->
      let bcode = bcode - 192 in
      let acode = acode - 64 in
      {
        empty with
        w2 = Int64.(shift_left minus_one acode);
        w3 = Int64.minus_one;
        w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot);
      }
  | W3, W4 ->
      let bcode = bcode - 192 in
      let acode = acode - 128 in
      { empty with w3 = Int64.(shift_left minus_one acode); w4 = Int64.(shift_left minus_one (bcode + 1) |> lognot) }
  | W4, W4 ->
      let bcode = bcode - 192 in
      let acode = acode - 192 in
      { empty with w4 = Int64.(shift_left (shift_left minus_one (bcode - acode + 1) |> lognot) acode) }
  | _, _ -> failwith "unreachable"

let iter_code f t =
  let open Int64 in
  let rec loop4 w i =
    if equal w zero then ()
    else (
      if equal (logand one w) one then f i;
      loop4 (shift_right_logical w 1) (i + 1))
  in
  let rec loop3 w i =
    if equal w zero then loop4 t.w4 192
    else (
      if equal (logand one w) one then f i;
      loop3 (shift_right_logical w 1) (i + 1))
  in
  let rec loop2 w i =
    if equal w zero then loop3 t.w3 128
    else (
      if equal (logand one w) one then f i;
      loop2 (shift_right_logical w 1) (i + 1))
  in
  let rec loop1 w i =
    if equal w zero then loop2 t.w2 64
    else (
      if equal (logand one w) one then f i;
      loop1 (shift_right_logical w 1) (i + 1))
  in
  loop1 t.w1 0
