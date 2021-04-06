type t =
    {
        errors : (int * int * string) list;
        comments : Res_comment.t list;
        attrs : (Lexing.position * Parsetree.attributes) list;
        trace : Trace_ctx.t;
    }

let default =
    {
        errors = [];
        comments = [];
        attrs = [];
        trace = Trace_ctx.default
    }
