
module Memoized =
    Angstrom_pos.Trace.Memoized
        (Pc_syntax.Basic.APos)
        (struct let memo_spec = Pc_syntax.Parser.memo_spec end)
module Parse =
    Pc_syntax.Parser.Make
        (struct
            module Named = Memoized
            module Peek = Angstrom_pos.Peek.MakePeek(Pc_syntax.Basic.APos)
        end)

let input = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, "" ]

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in
    let src = Res_io.readFile ~filename in

    match Filename.extension filename with
    | ".res" -> let _ = Parse.parse_implementation ~filename ~src in ()
    | ".resi" -> let _ = Parse.parse_interface ~filename ~src in ()
    | _ -> failwith filename
