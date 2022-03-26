
module APos = Angstrom_pos.Make(Pc_syntax.Basic.LogElement)
module Basic = Parser_angstrom.Make(APos)
module Parse = Pc_syntax.Parser.Make(Basic)

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
