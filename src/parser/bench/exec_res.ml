open Run_common

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in
    let src = read_file ~filename in

    match Filename.extension filename with
    | ".res" -> let _ = ParseRes.parse_implementation ~filename ~src in ()
    | ".resi" -> let _ = ParseRes.parse_interface ~filename ~src in ()
    | _ -> failwith filename
