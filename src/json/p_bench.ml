open P_base
open Core
open Core_bench

module Make (Parser : Parser) = struct
  open Parser

  let make_bench_json name contents =
      Bench.Test.create ~name (fun () ->
          match parse_json contents with
          | Some _ -> ()
          | None -> failwith "fail to parse"
      )
  ;;

  let sources = Files.(read_files [ complex; ws; others ])

  let () = Command.run @@ Bench.make_command @@ List.map ~f:(fun (name, src) -> make_bench_json name src) sources
end
