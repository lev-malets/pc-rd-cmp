
module Pos = Angstrom_pos.Make(struct type s = unit end)
module Stub_trace = Angstrom_pos.Trace.Stub (Pos)

module MemoSpec = struct
    let memo_spec = ["ret"]
end

open Pos

let parse parser contents =
    fun () ->
        match parse_string (parser ()) () contents with
        | Ok _ -> ()
        | _ -> failwith "fail to parse"

let parse_nc parser contents =
    fun () ->
        match parse_string parser () contents with
        | Ok _ -> ()
        | _ -> failwith "fail to parse"

let returns =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"return0" @@ (fun () -> return ());
        Bench.Test.create ~name:"return"  @@ parse (fun () -> let ret = return () in ret) "";
        Bench.Test.create ~name:"return2" @@ parse (fun () -> let ret = return () in ret >> ret) "";
        Bench.Test.create ~name:"return8" @@ parse (fun () -> let ret = return () in ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) "";

        Bench.Test.create ~name:"memo-return0" @@ (fun () -> memo @@ return ());
        Bench.Test.create ~name:"memo-return"  @@ parse (fun () -> let ret = memo @@ return () in ret) "";
        Bench.Test.create ~name:"memo-return2" @@ parse (fun () -> let ret = memo @@ return () in ret >> ret) "";
        Bench.Test.create ~name:"memo-return8" @@ parse (fun () -> let ret = memo @@ return () in ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) "";

        Bench.Test.create ~name:"nc-return"  @@ parse_nc (let ret = return () in ret) "";
        Bench.Test.create ~name:"nc-return2" @@ parse_nc (let ret = return () in ret >> ret) "";
        Bench.Test.create ~name:"nc-return8" @@ parse_nc (let ret = return () in ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) "";

        Bench.Test.create ~name:"nc-memo-return"  @@ parse_nc (let ret = memo @@ return () in ret) "";
        Bench.Test.create ~name:"nc-memo-return2" @@ parse_nc (let ret = memo @@ return () in ret >> ret) "";
        Bench.Test.create ~name:"nc-memo-return8" @@ parse_nc (let ret = memo @@ return () in ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) "";
    ]

let returns_traced =
    let open Core_bench in
    Bench.make_command
    [
        Bench.Test.create ~name:"named-stub-return"
            begin
                let module Traced = Angstrom_pos.Trace.Stub(Pos) in
                let ret = Traced.p "n" @@ return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"named-stub-return2"
            begin
                let module Traced = Angstrom_pos.Trace.Stub(Pos) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret) ""
            end;
        Bench.Test.create ~name:"named-stub-return8"
            begin
                let module Traced = Angstrom_pos.Trace.Stub(Pos) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) ""
            end;

        Bench.Test.create ~name:"named-return"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"named-return2"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret) ""
            end;
        Bench.Test.create ~name:"named-return8"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) ""
            end;

        Bench.Test.create ~name:"traced-return"
            begin
                let module Traced = Angstrom_pos.Trace.Traced(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"traced-return2"
            begin
                let module Traced = Angstrom_pos.Trace.Traced(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret) ""
            end;
        Bench.Test.create ~name:"traced-return8"
            begin
                let module Traced = Angstrom_pos.Trace.Traced(Pos)(struct let memo_spec = [] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) ""
            end;

        Bench.Test.create ~name:"memo-named-return"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"memo-named-return2"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret) ""
            end;
        Bench.Test.create ~name:"memo-named-return8"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) ""
            end;

        Bench.Test.create ~name:"memo-traced-return"
            begin
                let module Traced = Angstrom_pos.Trace.Traced(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc ret ""
            end;
        Bench.Test.create ~name:"memo-traced-return2"
            begin
                let module Traced = Angstrom_pos.Trace.Traced(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret) ""
            end;
        Bench.Test.create ~name:"memo-traced-return8"
            begin
                let module Traced = Angstrom_pos.Trace.Memoized(Pos)(struct let memo_spec = ["n"] end) in
                let ret = Traced.p "n" @@ return () in
                parse_nc (ret >> ret >> ret >> ret >> ret >> ret >> ret >> ret) ""
            end;
    ]

module IntMap = Map.Make(struct type t = int;; let compare = compare end)

let random_state = Random.State.make [|1; 2; 3|]
let random_int () = Random.State.int random_state (Int32.to_int @@ Int32.shift_right Int32.max_int 2)
let ints10 = Array.make 10 0 |> Array.map (fun _ -> random_int ())
let ints100 = Array.make 100 0 |> Array.map (fun _ -> random_int ())
let ints10000 = Array.make 10000 0 |> Array.map (fun _ -> random_int ())
let ints1000000 = Array.make 1000000 0 |> Array.map (fun _ -> random_int ())

let create_map arr =
    let len = Array.length arr in
    let rec loop i map =
        if i = len then map
        else loop (i + 1) (IntMap.add arr.(i) arr.(i) map)
    in
    loop 0 IntMap.empty

let create_hashtbl initial arr =
    let table = Hashtbl.create initial in
    let rec loop i =
        if i = 10 then table
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

        Bench.Test.create ~name:"10000:hashtbl"
            begin fun () -> create_hashtbl 0 ints10000 end;

        Bench.Test.create ~name:"1000000:hashtbl"
            begin fun () -> create_hashtbl 0 ints1000000 end;

        Bench.Test.create ~name:"find10:map"
            begin let map = create_map ints10 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create ~name:"find10:hashtbl"
            begin let table = create_hashtbl 0 ints10 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create ~name:"find100:map"
            begin let map = create_map ints100 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create ~name:"find100:hashtbl"
            begin let table = create_hashtbl 0 ints100 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create ~name:"find10000:map"
            begin let map = create_map ints10000 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create ~name:"find10000:hashtbl"
            begin let table = create_hashtbl 0 ints10000 in fun () -> Hashtbl.find_opt table 745 end;

        Bench.Test.create ~name:"find1000000:map"
            begin let map = create_map ints1000000 in fun () -> IntMap.find_opt 745 map end;
        Bench.Test.create ~name:"find1000000:hashtbl"
            begin let table = create_hashtbl 0 ints1000000 in fun () -> Hashtbl.find_opt table 745 end;
    ]

let () =
    let open Core in
    Command.run @@
    Command.group ~summary:"benchmarks" [
        "returns", returns;
        "returns-traced", returns_traced;
        "map-and-hashtable", map_hashtable;
    ]
