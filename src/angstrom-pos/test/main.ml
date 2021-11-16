(*
type ('a, 's) t = 'a -> 's -> 'a * 's

let c: (char, 's) t = fun a b -> a, b
let cx = c 'c' 'c'
let d: (char, 's) t = fun a b -> a, b
let dx = d 'd' 'd'
let f: ('a, 's) t -> ('a, 's) t -> ('a, 's) t =
    fun t1 t2 -> fun a b -> let x, _ = t1 a b in let _, y = t2 a b in x, y

let e: (char, 's) t = f c d
*)

(*
type l =
| Cons of l
| Ston

type exp =
    | Mul of exp * exp
    | Plus of exp * exp
    | N
    [@@deriving show]

module Unit = Angstrom_pos.Make(struct type s = unit;; type r = unit end)
module Char = Angstrom_pos.Make(struct type s = unit;; type r = char end)
module Exp = Angstrom_pos.Make(struct type s = unit;; type r = exp end)
module L = Angstrom_pos.Make(struct type s = unit;; type r = l end)

let unwrap = function
    | Ok x -> x
    | Error str -> failwith @@ "fail to unwrap" ^ str

let exp =
    let open Exp in
    let p = fix @@ fun p ->
        let p =
            (
                map2 ~f:(fun e1 e2 -> Mul (e1, e2))
                    (p << whitespace << char '*' << whitespace) p
            )
            <|>
            (
                map2 ~f:(fun e1 e2 -> Plus (e1, e2))
                    (p << whitespace << char '+' << whitespace) p
            )
            <|>
            (
                char 'n' >>$ N
            )
        in
        memo_cps p
    in
    p

let l =
    let open L in
    let p = fix @@ fun p ->
        let p =
            (
                fun i ->
                    map ~f:(fun s1 -> Cons s1)
                        ((p >>| fun x -> let _ = Printf.eprintf "%s\n" __LOC__ in x)  << whitespace << char 's') i
            )
            <|>
            (
                fun i ->
                    let _ = Printf.eprintf "%s\n" __LOC__ in
                    (char 's' >>$ Ston) i
            )
        in
        memo_cps p
    in
    p

let unit_return =
    let open Unit in
    memo_cps @@ return ()

let unit_fail =
    let open Unit in
    memo_cps @@ fail "fail"

let char_alt =
    let open Char in
    let p = memo_cps @@ char 'a' in
    (p >> char 'b') <|> p


let () =
    let answer = ref [] in

    let _ = unwrap @@ Unit.parse_string unit_return () "" in
        answer := "return: ok" :: !answer;
    ;

    match Unit.parse_string unit_fail () "" with
    | Ok _ -> failwith @@ "not failed"
    | Error _ -> answer := "fail: ok" :: !answer
    ;

    let res = unwrap @@ Char.parse_string char_alt () "ab" in
    if res = 'b' then
        answer := "alt 1: ok" :: !answer
    else
        answer := "alt 1: fail" :: !answer
    ;

    let res = unwrap @@ Char.parse_string char_alt () "a" in
    if res = 'a' then
        answer := "alt 2: ok" :: !answer
    else
        answer := "alt 2: fail" :: !answer
    ;

    List.iter
        (fun (s, r) ->
            let res = unwrap @@ L.parse_string l () s in
            if res = r then
                answer := (Printf.sprintf "%s: ok" s) :: !answer
            else
                answer := (Printf.sprintf "%s: fail" s) :: !answer
        )
        ["s", Ston
        ;"ss", Cons Ston]
    ;

    let res = unwrap @@ Exp.parse_string exp () "n + n" in
    if res = Plus (N, N) then
        answer := (Printf.sprintf "exp 1: ok") :: !answer
    else begin
        answer := (Printf.sprintf "exp 1: fail %s" @@ show_exp res) :: !answer
    end
    ;

    let res = unwrap @@ Exp.parse_string exp () "n + n * n" in
    if res = Mul (Plus (N, N), N) then
        answer := (Printf.sprintf "exp 2: ok") :: !answer
    else begin
        answer := (Printf.sprintf "exp 2: fail %s" @@ show_exp res) :: !answer
    end
    ;

    let res = unwrap @@ Exp.parse_string exp () "n + n * n + n" in
    if res = Mul (Plus (N, N), Plus (N, N)) then
        answer := (Printf.sprintf "exp 3: ok") :: !answer
    else begin
        answer := (Printf.sprintf "exp 3: fail %s" @@ show_exp res) :: !answer
    end
    ;

    List.iter
        (Printf.eprintf "%s\n")
        @@ List.rev !answer
*)
let _ = ()
