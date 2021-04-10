type t =
    {
        (*
        diagnostics : Res_diagnostics.t list;
        scope : (Res_grammar.t * Lexing.position) list;
        attrs : (Lexing.position * Parsetree.attributes) list;
        *)
        comments : Res_comment.t list;
        trace : Trace_ctx.t;
    }

let default =
    {
        (*
        diagnostics = [];
        scope = [];
        attrs = [];
        *)
        comments = [];
        trace = Trace_ctx.default
    }
