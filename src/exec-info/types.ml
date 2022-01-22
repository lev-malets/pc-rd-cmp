type exit_kind =
    | Exit of { pos: Lexing.position }
    | Error

type stats =
    { call_count : float
    ; pos_count : int
    }

type stats_table = (string, stats) Hashtbl.t
