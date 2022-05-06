open Run_common

let anon_fun _ = ()

let () =
  Arg.parse speclist anon_fun "";

  let filename = !input in
  let src = read_file ~filename in
  let (module Parse) = mk_parse ~tokenize:!tokenize () in

  match Filename.extension filename with
  | ".res" ->
      let _ = Parse.parse_implementation ~filename ~src in
      ()
  | ".resi" ->
      let _ = Parse.parse_interface ~filename ~src in
      ()
  | _ -> failwith filename
