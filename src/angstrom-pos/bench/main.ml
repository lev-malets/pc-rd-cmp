
module Pos = Angstrom_pos.Make(struct type s = unit end)
module Stub_trace = Angstrom_pos.Trace.Stub (Pos)

module MemoSpec = struct
    let memo_spec = ["ret"]
end

let parse parser contents =
    fun () ->
        match Pos.parse_string (parser ()) () contents with
        | Ok _ -> ()
        | _ -> failwith "fail to parse"

let parse_nc parser contents =
    fun () ->
        match Pos.parse_string parser () contents with
        | Ok _ -> ()
        | _ -> failwith "fail to parse"

let returns =
    let open Core_bench in
    let mk_ret spec =
        let open Pos.Parser in
        let open Pos in
        let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = spec end) in
        Traced.p "n"
        { p = Angstrom.(return ())
        ; info = Unknown
        ; typ = Parser
        }
    in

    Bench.make_command
    [
        Bench.Test.create ~name:"named-return"
            begin
                let ret = mk_ret [] in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"named-return128"
            begin
                let ret = mk_ret [] in
                parse_nc (Array.make 127 ret |> Array.fold_left Pos.(>>) ret) ""
            end;

        Bench.Test.create ~name:"memo-named-return"
            begin
                let ret = mk_ret ["n"] in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"memo-named-return128"
            begin
                let ret = mk_ret ["n"] in
                parse_nc (Array.make 127 ret |> Array.fold_left Pos.(>>) ret) ""
            end;
    ]

module IntMap = Map.Make(struct type t = int;; let compare = compare end)

let random_state = Random.State.make [|1; 2; 3|]
let random_int () = Random.State.int random_state (Int32.to_int @@ Int32.shift_right Int32.max_int 2)
let ints10 = lazy (Array.make 10 0 |> Array.map (fun _ -> random_int ()))
let ints100 = lazy (Array.make 100 0 |> Array.map (fun _ -> random_int ()))
let ints10000 = lazy (Array.make 10000 0 |> Array.map (fun _ -> random_int ()))
let ints100000 = lazy (Array.make 100000 0 |> Array.map (fun _ -> random_int ()))
let ints1000000 = lazy (Array.make 1000000 0 |> Array.map (fun _ -> random_int ()))

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
                        let i = (Obj.magic : _ -> int) @@ Obj.repr x in
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
    let map = ref IntMap.empty in
    let rec loop i =
        if i = len then !map
        else
            let _ = map := IntMap.add arr.(i) arr.(i) !map in
            loop (i + 1)
    in
    loop 0

let create_hashtbl initial (lazy arr) =
    let len = Array.length arr in
    let table = Hashtbl.create initial in
    let rec loop i =
        if i = len then table
        else begin
            Hashtbl.add table arr.(i) arr.(i);
            loop @@ i + 1
        end
    in
    loop 0

let map_hashtable =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"empty:map" @@ (fun () -> IntMap.empty);
        Bench.Test.create ~name:"empty:hashtbl" @@ (fun () -> Hashtbl.create 0);
        Bench.Test.create ~name:"empty:hashtbl2" @@ (fun () -> Hashtbl.create 16);

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

        Bench.Test.create ~name:"1000000:hashtbl"
            begin fun () -> create_hashtbl 0 ints1000000 end;

        Bench.Test.create_with_initialization ~name:"find10:map"
            begin fun `init -> let map = create_map ints10 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create_with_initialization ~name:"find10:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints10 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create_with_initialization ~name:"find100:map"
            begin fun `init -> let map = create_map ints100 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create_with_initialization ~name:"find100:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints100 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create_with_initialization ~name:"find10000:map"
            begin fun `init -> let map = create_map ints10000 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create_with_initialization ~name:"find10000:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints10000 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create_with_initialization ~name:"find1000000:map"
            begin fun `init -> let map = create_map ints1000000 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create_with_initialization ~name:"find1000000:hashtbl"
            begin fun `init -> let table = create_hashtbl 0 ints1000000 in fun () -> Hashtbl.find_opt table 745 end;
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
