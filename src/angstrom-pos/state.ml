module Line = struct
    type t =
        {
            no    : int;
            start : int;
        }

    let default = { no = 1; start = 0 }

    let advance t l s = { no = t.no + l; start = s }
end

module Info = struct
    type t =
        {
            default_position : Lexing.position;
        }
end

type 'a t =
    {
        prev_line_start : int;
        line            : Line.t;
        token_end       : Lexing.position;
        ws_end          : int;
        info            : Info.t;
        custom          : 'a;
    }

let make file_name custom =
    let default_position = Lexing.{ pos_fname = file_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    {
        prev_line_start = 0;
        line = Line.default;
        token_end = default_position;
        ws_end = 0;
        info =
            {
                default_position;
            };
        custom;
    }
