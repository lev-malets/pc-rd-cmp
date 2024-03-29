open Core
open Run_common

let search_files dirs =
  let rec loop dirs acc =
    match dirs with
    | [] -> acc
    | dir :: dirs ->
        let files = Sys_unix.readdir dir in
        let rec loop_files i dirs acc =
          if i = Array.length files then loop dirs acc
          else
            let file = Filename.concat dir files.(i) in
            match Sys_unix.is_directory file with
            | `Yes -> loop_files (i + 1) (file :: dirs) acc
            | _ -> (
              match Filename.split_extension file with
              | _, Some "res" -> loop_files (i + 1) dirs ((file, false) :: acc)
              | _, Some "resi" -> loop_files (i + 1) dirs ((file, true) :: acc)
              | _ -> loop_files (i + 1) dirs acc)
        in
        loop_files 0 dirs acc
  in
  List.rev @@ loop dirs []

let files =
  search_files
    [ "data/res"
    ; "tmp/deps/syntax/tests/parsing/grammar"
    ; "tmp/deps/syntax/benchmarks/data" ]
  |> List.map ~f:(fun (n, f) -> (n, f, Stdio.In_channel.read_all n))

let signature : Compilerlibs406.Parsetree.signature Alcotest.testable =
  Alcotest.testable (fun fmt _ -> Fmt.pf fmt "<signature>") Poly.( = )

let structure : Compilerlibs406.Parsetree.structure Alcotest.testable =
  Alcotest.testable (fun fmt _ -> Fmt.pf fmt "<structure>") Poly.( = )

let () =
  let open Alcotest in
  let config = Some "data/configs/test.json" in
  let (module Parse) = mk_parse ~tokenize:false config in
  let (module ParseT) = mk_parse ~tokenize:true config in
  let och = Out_channel.create "out" in
  let mk_test_cases (module Parse : Pc_syntax.Sigs.PARSE) map_sig map_str =
    List.map files ~f:(fun (n, is_sig, s) ->
        let f =
          let open Syntax.Res_driver in
          if is_sig then fun () ->
            let expected =
              Parser.Rescript.parse_interface ~filename:n ~src:s
              |> Option.map ~f:(fun x -> map_sig x.parsetree)
            in
            let actual =
              Parse.parse_interface ~filename:n ~src:s
              |> Option.map ~f:(fun x -> map_sig x.parsetree)
            in
            check' (option signature) ~msg:n ~expected ~actual
          else fun () ->
            Printf.fprintf och "%s\n" n;
            let expected =
              Parser.Rescript.parse_implementation ~filename:n ~src:s
              |> Option.map ~f:(fun x -> map_str x.parsetree)
            in
            let actual =
              Parse.parse_implementation ~filename:n ~src:s
              |> Option.map ~f:(fun x -> map_str x.parsetree)
            in
            Alcotest.check' (option structure) ~msg:n ~expected ~actual
        in
        test_case n `Quick f)
  in
  run "Parse"
    [ ("parse", mk_test_cases (module Parse) (fun x -> x) (fun x -> x))
    ; ( "parse:noloc"
      , mk_test_cases
          (module Parse)
          (fun x ->
            Pc_syntax.Parsetree_mapping.signature Run_common.dump_loc_mapping x)
          (fun x ->
            Pc_syntax.Parsetree_mapping.structure Run_common.dump_loc_mapping x)
      )
    ; ("parse:tokenize", mk_test_cases (module ParseT) (fun x -> x) (fun x -> x))
    ; ( "parse:tokenize:noloc"
      , mk_test_cases
          (module ParseT)
          (fun x ->
            Pc_syntax.Parsetree_mapping.signature Run_common.dump_loc_mapping x)
          (fun x ->
            Pc_syntax.Parsetree_mapping.structure Run_common.dump_loc_mapping x)
      ) ]
