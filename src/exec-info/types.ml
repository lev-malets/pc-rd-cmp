type exit_kind =
    | Exit of { pos: Lexing.position }
    | Error

type stats =
    { count : float
    ; time : float
    }

type stats_table = (string, stats) Hashtbl.t
