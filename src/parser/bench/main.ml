open Run_common

let parser = ref "res"
let memo_spec = ref []

let speclist =
    [ "--parser", Arg.Symbol (["res"; "pc"], ((:=) parser)), ""
    ; "--memo",
        Arg.Rest (fun s -> memo_spec := s :: !memo_spec),
        ""
    ] @ speclist

let () =
    Arg.parse speclist anon_fun "";

    let (module Parse: Pc_syntax.Sigs.PARSE) = match [@warning "-8"] !parser with
        | "res" -> (module ParseRes)
        | "pc" -> mk_memoized !memo_spec
    in
    let filename = !input in
    let src = Res_io.readFile ~filename in

    let test =
        if String.sub filename (String.length filename - 4) 4 = ".res" then
            Core_bench.Bench.Test.create ~name:filename @@ fun () -> Core.Result.ok @@ Parse.parse_implementation ~src ~filename
        else if String.sub filename (String.length filename - 5) 5 = ".resi" then
            Core_bench.Bench.Test.create ~name:filename @@ fun () -> Core.Result.ok @@ Parse.parse_interface ~src ~filename
        else
            failwith filename
    in

    Core_bench.Bench.bench [test]
