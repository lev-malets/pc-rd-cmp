type t =
    {
        diagnostics : Res_diagnostics.t list;
        comments : Res_comment.t list;
        scope : (Res_grammar.t * Lexing.position) list;
        attrs : (Lexing.position * Parsetree.attributes) list;
        trace : Trace_ctx.t;
    }

let default =
    {
        diagnostics = [];
        comments = [];
        scope = [];
        attrs = [];
        trace = Trace_ctx.default
    }
