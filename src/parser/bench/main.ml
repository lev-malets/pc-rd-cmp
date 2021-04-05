
open Core
open Core_bench
open Pc_syntax.Angstrom

let parse parser contents =
    match parse_string parser Pc_syntax.Pcs.default contents with
    | Ok _ -> ()
    | _ -> failwith "fail to parse"

module Position = struct
    let data = " "
    let parse p = 
        let p = whitespace >> p in
        fun () -> parse p data

    let command = Bench.make_command [
        Bench.Test.create ~name:"position"      @@ parse position;
        Bench.Test.create ~name:"position1"     @@ parse Alt.position;
        Bench.Test.create ~name:"end_position"  @@ parse end_position;
        Bench.Test.create ~name:"end_position1" @@ parse Alt.end_position;
    ]
end

let () = Command.run @@
    Command.group ~summary:"various benchmarks" [
        "position", Position.command
    ]
