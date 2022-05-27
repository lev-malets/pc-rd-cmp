module IO = Res_io
open Res_driver

let parse ~src ~filename ~parse =
  Location.input_name := filename;
  let mode = Res_parser.ParseForTypeChecker in
  let engine = Res_parser.make ~mode src filename in

  let parsetree = parse engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in

  let parse_result =
    {
      filename = engine.scanner.filename;
      source = engine.scanner.src;
      parsetree;
      diagnostics;
      invalid;
      comments = List.rev engine.comments;
    }
  in

  if parse_result.invalid then (
    Res_diagnostics.printReport parse_result.diagnostics parse_result.source;
    exit 1);
  parse_result
  [@@raises exit]

let parse_implementation ~src ~filename =
  parse ~src ~filename ~parse:Res_core.parseImplementation

let parse_interface ~src ~filename =
  parse ~src ~filename ~parse:Res_core.parseSpecification
