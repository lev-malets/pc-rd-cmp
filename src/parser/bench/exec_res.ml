
let input = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, "" ]

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in

    match Filename.extension filename with
    | ".res" -> let _ = Res_driver.parse_implementation filename in ()
    | ".resi" -> let _ = Res_driver.parse_interface filename in ()
    | _ -> failwith filename
