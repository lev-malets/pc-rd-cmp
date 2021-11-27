
let main =
    let f = Core_kernel.In_channel.read_all @@ Sys.argv.(1) in
    print_endline f
