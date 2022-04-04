open Run_common

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in
    let src = read_file ~filename in

    match Filename.extension filename with
    | ".res" -> assert (String.length src > 0)
    | ".resi" -> assert (String.length src > 0)
    | _ -> failwith filename
