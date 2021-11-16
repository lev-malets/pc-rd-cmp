
open Core
open Core_bench
open Pc_syntax.Basic.Angstrom

let parse parser contents =
    match parse_string parser Pc_syntax.State.default contents with
    | Ok _ -> ()
    | _ -> failwith "fail to parse"

module Position = struct
    let data = " "
    let parse p =
        let p = whitespace >> p in
        fun () -> parse p data

    let command = Bench.make_command [
(*
        Bench.Test.create ~name:"position"      @@ parse position;
        Bench.Test.create ~name:"position1"     @@ parse Alt.position;
*)
    ]
end

let () = Command.run @@
    Command.group ~summary:"various benchmarks" [
        "position", Position.command
    ]
