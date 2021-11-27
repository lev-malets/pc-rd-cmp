
type t

include module type of Types

val make : unit -> t
val add_entry : t -> name:string -> enter_pos:Lexing.position -> enter_time:float -> exit:exit_kind -> subs:t -> unit

val to_html_s : t -> string
val to_html : out_channel -> t -> unit
val to_stats : t -> stats_table

val last_pos : t -> string list * Lexing.position
