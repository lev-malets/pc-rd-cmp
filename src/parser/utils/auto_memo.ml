open Core
open Run_common
open Cmdliner
open Float
open Int

let inex_forced =
  let open Hashtbl in
  let t = create (module String) in
  add_exn t ~key:".*:nongrammar" ~data:Float.infinity;
  add_exn t ~key:"pattern" ~data:Float.infinity;
  add_exn t ~key:"expression:p0:p" ~data:Float.infinity;
  add_exn t ~key:"expression:p8" ~data:Float.infinity;
  add_exn t ~key:"expression:in_braces" ~data:Float.infinity;
  add_exn t ~key:"attrs" ~data:Float.infinity;

  add_exn t ~key:"tkn:.*" ~data:Float.neg_infinity;
  t

let mk_conf inex =
  let inc, exc =
    Hashtbl.fold inex ~init:([], []) ~f:(fun ~key ~data (i, e) ->
        if data >. 0. then (key :: i, e) else (i, key :: e))
  in

  let mi =
    String.concat ~sep:"," @@ List.map ~f:(fun x -> "\"" ^ x ^ "\"") inc
  in
  let me =
    String.concat ~sep:"," @@ List.map ~f:(fun x -> "\"" ^ x ^ "\"") exc
  in
  {| {"memoize":{"include":[ |} ^ mi ^ {| ], "exclude":[ |} ^ me
  ^ {| ]}, "peek":{"auto":{"min-variants":2}}, "trace":{"include":[".*"]}} |}

module type PARSER = sig
  include Pc_syntax.Sigs.PARSER

  val str_p : Compilerlibs406.Parsetree.structure Comb.t
  val sig_p : Compilerlibs406.Parsetree.signature Comb.t
end

let mk_parse conf =
  let (module Parse) = mk_parse ~tokenize:true ~src:conf None in
  (module struct
    include Parse
    open Comb

    let str_p = named "__root_str__" structure_parser
    let sig_p = named "__root_sig__" signature_parser
  end : PARSER)

let stats { filename; src } (module Parse : PARSER) agg_runs =
  let root_name, run =
    let open Parse in
    let open Comb in
    match Filename.split_extension filename with
    | _, Some "res" ->
        ( "__root_str__",
          fun () ->
            let _, x = parse_string_with_trace str_p ~filename src in
            x )
    | _, Some "resi" ->
        ( "__root_sig__",
          fun () ->
            let _, x = parse_string_with_trace sig_p ~filename src in
            x )
    | _ -> failwith filename
  in

  let entries_list = List.init agg_runs ~f:(fun _ -> run ()) in
  let stats_list = List.map ~f:Exec_info.to_stats @@ entries_list in
  let stats = Hashtbl.create (module Int) in
  List.iter stats_list
    ~f:
      (Hashtbl.iteri ~f:(fun ~key ~data ->
           let open Exec_info in
           Hashtbl.update stats key ~f:(function
             | None -> data
             | Some x ->
                 {
                   call_count = x.call_count + data.call_count;
                   pos_count = x.pos_count + data.pos_count;
                   time = FloatStatistics.append data.time x.time;
                   time_individual =
                     FloatStatistics.append data.time_individual
                       x.time_individual;
                 })));
  (root_name, stats)

let memo_candidate name_of_id stats (config : Pc.Config.t) change_to_stop =
  if Random.int 100 < change_to_stop then None
  else
    let list =
      Hashtbl.fold stats ~init:[] ~f:(fun ~key ~data acc ->
          let open Exec_info in
          let name = name_of_id key in
          let cond = true in
          let cond =
            let a = Pc.Utils.check_string config.memoize.accept name in
            cond && not a
          in
          let cond =
            let d = Pc.Utils.check_string config.memoize.decline name in
            cond && not d
          in
          let cond = cond && data.call_count > data.pos_count in

          if cond then (name, data.time.sum) :: acc else acc)
    in

    let len = List.length list in
    if len = 0 then None
    else
      let idx = Random.int len in
      List.nth list idx

let file_single_run input agg_runs chance_to_stop =
  let inex = Hashtbl.copy inex_forced in
  let rec try_candidate root_name (module P0 : PARSER) s0 =
    let c =
      memo_candidate P0.Comb.name_of_id s0 P0.Comb.Conf.config chance_to_stop
    in
    match c with
    | None -> Hashtbl.find_exn s0 (Hashtbl.find_exn P0.Comb.name2id root_name)
    | Some (candidate, time) ->
        Hashtbl.add_exn inex ~key:candidate ~data:1.;
        let conf = mk_conf inex in
        let p1 = mk_parse conf in
        let _, s1 = stats input p1 agg_runs in
        let (module P1) = p1 in
        let id = Hashtbl.find_exn P1.Comb.name2id candidate in
        let candidate_new_stats = Hashtbl.find_exn s1 id in
        let div = candidate_new_stats.time.sum /. time in
        if div <. 0.99 then
          let _ =
            Hashtbl.set inex ~key:candidate
              ~data:(time -. candidate_new_stats.time.sum)
          in
          try_candidate root_name p1 s1
        else
          let _ =
            if div <=. 1.01 then Hashtbl.set inex ~key:candidate ~data:0.
            else
              Hashtbl.set inex ~key:candidate
                ~data:(time -. candidate_new_stats.time.sum)
          in

          let conf = mk_conf inex in
          let p2 = mk_parse conf in
          try_candidate root_name p2 s0
  in

  let conf = mk_conf inex in
  let p0 = mk_parse conf in
  let root_name, s0 = stats input p0 agg_runs in
  let (module P0) = p0 in
  let s0_root =
    Hashtbl.find_exn s0 (Hashtbl.find_exn P0.Comb.name2id root_name)
  in
  let sN_root = try_candidate root_name p0 s0 in
  let div = sN_root.time.sum /. s0_root.time.sum in
  if div <. 1. then (
    Printf.printf "  %.2f: %10.2f -> %10.2f" div
      (s0_root.time.sum *. 1_000_000. /. Float.of_int agg_runs)
      (sN_root.time.sum *. 1_000_000. /. Float.of_int agg_runs);
    print_endline "";
    Hashtbl.mapi_inplace inex ~f:(fun ~key:_ ~data -> data /. s0_root.time.sum);
    inex)
  else
    let _ =
      Printf.printf "  %.2f: %10.2f -> %10.2f (skip)" div
        (s0_root.time.sum *. 1_000_000. /. Float.of_int agg_runs)
        (sN_root.time.sum *. 1_000_000. /. Float.of_int agg_runs)
    in
    let _ = print_endline "" in
    Hashtbl.create (module String)

let file_run input agg_runs file_runs chance_to_stop =
  let runs = file_runs in
  let inex = Hashtbl.copy inex_forced in
  for i = 1 to runs do
    Printf.printf "%d of %s" i input.filename;
    print_endline "";
    let inex_ = file_single_run input agg_runs chance_to_stop in
    Hashtbl.merge_into ~src:inex_ ~dst:inex ~f:(fun ~key:_ a -> function
      | None -> Set_to (a /. Float.of_int runs)
      | Some b -> Set_to (b +. (a /. Float.of_int runs)))
  done;
  inex

let run inputs output agg_runs file_runs chance_to_stop =
  Random.self_init ();
  print_endline "start";
  let inex = Hashtbl.copy inex_forced in
  let rec loop = function
    | [] -> ()
    | (input, weight) :: xs ->
        let input = mk_input (Some input) in
        let inex_ = file_run input agg_runs file_runs chance_to_stop in
        Hashtbl.merge_into ~src:inex_ ~dst:inex ~f:(fun ~key:_ a -> function
          | None -> Set_to (a *. weight) | Some b -> Set_to (b +. (a *. weight)));
        loop xs
  in
  loop inputs;
  let out = mk_output output in
  fprintf out.channel "Include\n";
  let inc =
    Hashtbl.keys inex
    |> List.filter ~f:(fun x ->
           let d = Hashtbl.find_exn inex x in
           d >. 0.)
  in
  let inc =
    List.sort inc ~compare:(fun a b ->
        Float.compare (Hashtbl.find_exn inex b) (Hashtbl.find_exn inex a))
  in
  List.iter inc ~f:(fun x ->
      fprintf out.channel "    %5.2f: %s\n" (Hashtbl.find_exn inex x) x);

  fprintf out.channel "Exclude\n";
  let inc =
    Hashtbl.keys inex
    |> List.filter ~f:(fun x -> Hashtbl.find_exn inex x <=. 0.)
  in
  let inc =
    List.sort inc ~compare:(fun a b ->
        Float.compare (Hashtbl.find_exn inex a) (Hashtbl.find_exn inex b))
  in
  List.iter inc ~f:(fun x ->
      fprintf out.channel "    %5.2f: %s\n" (Hashtbl.find_exn inex x) x);
  out.close ()

let inputs =
  let doc = "Input file" in
  Arg.(
    non_empty
    & opt_all (pair ~sep:'*' string float) []
    & info ~doc ~docv:"FILE" [ "inputs" ])

let file_runs =
  let doc = "Input file" in
  Arg.(value & opt int 5 & info ~doc ~docv:"FILE" [ "file-runs" ])

let agg_runs =
  let doc = "Input file" in
  Arg.(value & opt int 10 & info ~doc ~docv:"FILE" [ "agg-runs" ])

let chance_to_stop =
  let doc = "Input file" in
  Arg.(value & opt int 5 & info ~doc ~docv:"FILE" [ "chance-to-stop" ])

let cmd =
  let open Args in
  Cmd.v (Cmd.info "exec")
    Term.(const run $ inputs $ output $ agg_runs $ file_runs $ chance_to_stop)

let () = Stdlib.exit @@ Cmd.eval cmd
