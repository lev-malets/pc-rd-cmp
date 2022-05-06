val parse_implementation :
  src:string -> filename:string -> (Parsetree.structure, Res_diagnostics.t list) Res_driver.parseResult

val parse_interface :
  src:string -> filename:string -> (Parsetree.signature, Res_diagnostics.t list) Res_driver.parseResult
