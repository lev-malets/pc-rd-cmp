open Res_diagnostics
open Basic.Angstrom

let diagnostic startPos endPos category =
  let diagnostic = make ~startPos ~endPos category in
  map_state (fun s -> {s with diagnostics = diagnostic :: s.diagnostics})
