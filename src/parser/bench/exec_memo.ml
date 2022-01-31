
module APos_ = Angstrom_pos.Make(Pc_syntax.State)
module NotPeek_ = Angstrom_pos.Alt.MakeNotPeek(APos_)

module APos = struct
    include APos_
    include NotPeek_
end

module Parse = Pc_syntax.Parser.Make(APos)

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
