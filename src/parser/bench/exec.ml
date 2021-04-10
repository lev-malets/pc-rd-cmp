open Core
open Core_bench

module Res = struct
    let signature file = Res_driver.parse_interface file
end

module Pc = struct
    module Parser = Pc_syntax.Parser.Make(Pc_syntax.Trace.Stub)

    let unwrap = function
        | Ok x -> x
        | Error str -> failwith @@ "fail to unwrap" ^ str

    let signature file = unwrap @@ Parser.parse_interface file
end

module type A = module type of Res

let () =
    let (module M): (module A) = match Sys.argv.(1) with
    | "res" -> (module Res)
    | "pc" -> (module Pc)
    | x -> failwith x
    in

    let file = Sys.argv.(3) in

    let test = match Sys.argv.(2) with
        | "signature" ->
            Bench.Test.create ~name:file @@ fun () -> M.signature file
        | x -> failwith x
    in
    Bench.bench [test]
