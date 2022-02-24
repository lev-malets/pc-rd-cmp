
let input = ref ""
let anon_fun _ = ()

let speclist =
    [ "--input", Arg.Set_string input, "" ]

let () =
    Arg.parse speclist anon_fun "";

    let filename = !input in

    assert (String.length filename > 0)
