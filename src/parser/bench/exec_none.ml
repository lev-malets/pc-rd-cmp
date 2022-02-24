
let input = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, "" ]

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in
    let src = Res_io.readFile ~filename in

    match Filename.extension filename with
    | ".res" -> assert (String.length src > 0)
    | ".resi" -> assert (String.length src > 0)
    | _ -> failwith filename
