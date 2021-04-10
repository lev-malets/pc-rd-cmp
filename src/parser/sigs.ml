
module type TRACE = sig
  val point : string -> (unit -> 'a Basic.Angstrom.Parser.t) -> 'a Basic.Angstrom.Parser.t
end
