
module Res = struct
  let signature file = Rescript_ast_fix.fix_signature (Res_driver.parse_interface file)
end

module Pc = struct
  module Parser = Pc_syntax.Parser.Make(Pc_syntax.Trace)

  let unwrap = function
      | Ok x -> x
      | Error str -> failwith @@ "fail to unwrap" ^ str

  let signature file = unwrap @@ Parser.parse_interface file
end

module type A = module type of Res

let () =
  let (module M): (module A) = match Sys.argv.(1) with
  | "res" -> (module Res)
  | "pc" -> (module Pc)
  | x -> failwith x
  in

  let file = Sys.argv.(3) in

  match Sys.argv.(2) with
  | "signature" ->
    let s = M.signature file in
    Ast_show.Parsetree.pp_signature (Format.std_formatter) s
  | x -> failwith x
