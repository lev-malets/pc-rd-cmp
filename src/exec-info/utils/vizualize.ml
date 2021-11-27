
let input = ref ""
let output = ref ""
let anon_fun _ = ()

let speclist = [
    "--input", Arg.Set_string input, "";
    "--output", Arg.Set_string output, "";
]

let () =
    Arg.parse speclist anon_fun "";

    let ich =
        match !input with
        | "" -> stdin
        | file -> open_in file
    in

    let och =
        match !input with
        | "" -> stdout
        | file -> open_out file
    in

    let info: Exec_info.t = Marshal.from_channel ich in
    Exec_info.to_html och info
