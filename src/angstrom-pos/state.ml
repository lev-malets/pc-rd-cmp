module Line = struct
    type t =
        {
            no    : int;
            start : int;
        }

    let default = { no = 1; start = 0 }
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
        grammar_end     : Lexing.position;
        nongrammar_end  : int;
        info            : Info.t;
        custom          : 'a;
    }

let make file_name custom =
    let default_position = Lexing.{ pos_fname = file_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    {
        prev_line_start = 0;
        line = Line.default;
        grammar_end = default_position;
        nongrammar_end = 0;
        info =
            {
                default_position;
            };
        custom;
    }
