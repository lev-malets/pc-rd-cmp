
module Tpc = Tokenized.Make(Parser_tokenized.Lexer.Make())(Pc_syntax.Basic.LogElement)
module Parse = Parser_tokenized.Make(Tpc)

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
