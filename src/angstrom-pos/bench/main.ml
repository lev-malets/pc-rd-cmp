open Base

module Pos = Angstrom_pos.Make(struct type s = unit end)

module MemoSpec = struct
    let memo_spec = ["ret"]
end

let parse parser contents =
    fun () ->
        match Pos.parse_string (parser ()) () contents with
        | Some _ -> ()
        | _ -> failwith "fail to parse"

let parse_nc parser contents =
    fun () ->
        match Pos.parse_string parser () contents with
        | Some _ -> ()
        | _ -> failwith "fail to parse"

let returns =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"named-return"
            begin
                let ret = Pos.return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"named-return128"
            begin
                let ret = Pos.return () in
                parse_nc (Array.create ~len:127 ret |> Array.fold ~f:Pos.(>>) ~init:ret) ""
            end;

        Bench.Test.create ~name:"memo-named-return"
            begin
                let ret = Pos.(memo @@ return ()) in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"memo-named-return128"
            begin
                let ret = Pos.(memo @@ return ()) in
                parse_nc (Array.create ~len:127 ret |> Array.fold ~f:Pos.(>>) ~init:ret) ""
            end;
    ]

let random_state = Random.State.make [|1; 2; 3|]
let random_int () = Random.State.int random_state (Int32.to_int_exn @@ Int32.shift_right Int32.max_value 2)
let ints10 = lazy (Array.create ~len:10 0 |> Array.map ~f:(fun _ -> random_int ()))
let ints100 = lazy (Array.create ~len:100 0 |> Array.map ~f:(fun _ -> random_int ()))
let ints10000 = lazy (Array.create ~len:10000 0 |> Array.map ~f:(fun _ -> random_int ()))
let ints100000 = lazy (Array.create ~len:100000 0 |> Array.map ~f:(fun _ -> random_int ()))
let ints1000000 = lazy (Array.create ~len:1000000 0 |> Array.map ~f:(fun _ -> random_int ()))

let hash =
    let open Core_bench in
    let create_tests name mk_x =
        [
            Bench.Test.create_with_initialization ~name
                begin
                    fun `init ->
                        let x = mk_x () in
                    fun _ ->
                        Hashtbl.hash x
                end;
            Bench.Test.create_with_initialization ~name:(name ^ ":repr:magic:lsr")
                begin
                    fun `init ->
                        let x = mk_x () in
                    fun _ ->
                        let i = (Caml.Obj.magic : _ -> int) @@ Caml.Obj.repr x in
                        let x = (i lsr 4, i land 0xF) in
                        Hashtbl.hash x
                end;
        ]
    in

    Bench.make_command
    (
        create_tests "int" random_int
        @
        create_tests "parser" (fun _ -> Pos.whitespace)
        @
        create_tests "a100000" (fun _ -> Lazy.force ints100000)
        @
        [
            Bench.Test.create_with_initialization ~name:"2ints"
                begin
                    fun `init ->
                        let x = (random_int (), random_int ()) in
                    fun _ ->
                        Hashtbl.hash x
                end;
            Bench.Test.create_with_initialization ~name:"5ints"
                begin
                    fun `init ->
                        let x = (random_int (), random_int (), random_int (), random_int (), random_int ()) in
                    fun _ ->
                        Hashtbl.hash x
                end;
        ]
    )

let create_list (lazy arr) =
    let len = Array.length arr in
    let list = ref [] in
    let rec loop i =
        if i = len then !list
        else
            let _ = list := arr.(i) :: !list in
            loop (i + 1)
    in
    loop 0

let create_vector initial (lazy arr) =
    let len = Array.length arr in
    let vector = CCVector.create_with ~capacity:initial 0 in
    let rec loop i =
        if i = len then vector
        else begin
            CCVector.push vector arr.(i);
            loop @@ i + 1
        end
    in
    loop 0

let create_map (lazy arr) =
    let len = Array.length arr in
    let map = ref (Map.Using_comparator.empty ~comparator:Int.comparator) in
    let rec loop i =
        if i = len then !map
        else begin
            begin match Map.add ~key:arr.(i) ~data:arr.(i) !map with
            | `Ok x -> map := x
            | `Duplicate -> ()
            end;
            loop (i + 1)
        end
    in
    loop 0

let create_hashtbl initial (lazy arr) =
    let len = Array.length arr in
    let table = Hashtbl.create (module Int) ~size:initial in
    let rec loop i =
        if i = len then table
        else begin
            let _ = Hashtbl.add table ~key:arr.(i) ~data:arr.(i) in
            loop @@ i + 1
        end
    in
    loop 0

let map_hashtable =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"empty:hashtbl" @@ (fun () -> Hashtbl.create (module Int));
        Bench.Test.create ~name:"empty:hashtbl2" @@ (fun () -> Hashtbl.create (module Int) ~size:16);

        Bench.Test.create ~name:"10:map"
            begin fun () -> create_map ints10 end;
        Bench.Test.create ~name:"10:hashtbl"
            begin fun () -> create_hashtbl 0 ints10 end;
        Bench.Test.create ~name:"10:hashtbl2"
            begin fun () -> create_hashtbl 16 ints10 end;

        Bench.Test.create ~name:"100:map"
            begin fun () -> create_map ints100 end;
        Bench.Test.create ~name:"100:hashtbl"
            begin fun () -> create_hashtbl 0 ints100 end;
        Bench.Test.create ~name:"100:hashtbl2"
            begin fun () -> create_hashtbl 16 ints100 end;

        Bench.Test.create ~name:"10000:map"
            begin fun () -> create_map ints10000 end;
        Bench.Test.create ~name:"10000:hashtbl"
            begin fun () -> create_hashtbl 0 ints10000 end;

        Bench.Test.create_with_initialization ~name:"find10:map"
            begin fun `init -> let map = create_map ints10 in fun () -> Map.find map 745 end;
        Bench.Test.create_with_initialization ~name:"find10:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints10 in fun () -> Hashtbl.find table 745 end;

        Bench.Test.create_with_initialization ~name:"find100:map"
            begin fun `init -> let map = create_map ints100 in fun () -> Map.find map 745 end;
        Bench.Test.create_with_initialization ~name:"find100:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints100 in fun () -> Hashtbl.find table 745 end;

        Bench.Test.create_with_initialization ~name:"find10000:map"
            begin fun `init -> let map = create_map ints10000 in fun () -> Map.find map 745 end;
        Bench.Test.create_with_initialization ~name:"find10000:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints10000 in fun () -> Hashtbl.find table 745 end;

        Bench.Test.create_with_initialization ~name:"find1000000:map"
            begin fun `init -> let map = create_map ints1000000 in fun () -> Map.find map 745 end;
        Bench.Test.create_with_initialization ~name:"find1000000:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints1000000 in fun () -> Hashtbl.find table 745 end;
    ]

let list_vector =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"10:list"
            begin fun () -> create_list ints10 end;
        Bench.Test.create ~name:"10:vector"
            begin fun () -> create_vector 0 ints10 end;
        Bench.Test.create ~name:"10:vector2"
            begin fun () -> create_vector 128 ints10 end;

        Bench.Test.create ~name:"100:list"
            begin fun () -> create_list ints100 end;
        Bench.Test.create ~name:"100:vector"
            begin fun () -> create_vector 0 ints100 end;
        Bench.Test.create ~name:"100:vector2"
            begin fun () -> create_vector 128 ints100 end;

        Bench.Test.create ~name:"10000:list"
            begin fun () -> create_list ints10000 end;
        Bench.Test.create ~name:"10000:vector"
            begin fun () -> create_vector 0 ints10000 end;
        Bench.Test.create ~name:"10000:vector2"
            begin fun () -> create_vector 128 ints10000 end;

        Bench.Test.create ~name:"100000:list"
            begin fun () -> create_list ints100000 end;
        Bench.Test.create ~name:"100000:vector"
            begin fun () -> create_vector 0 ints100000 end;
        Bench.Test.create ~name:"100000:vector2"
            begin fun () -> create_vector 128 ints100000 end;
        Bench.Test.create ~name:"100000:vector3"
            begin fun () -> create_vector 16384 ints100000 end;
    ]

let () =
    let open Core in
    Command.run @@
    Command.group ~summary:"benchmarks" [
        "returns", returns;
        "hash", hash;
        "map-and-hashtable", map_hashtable;
        "list-and-vector", list_vector;
    ]
