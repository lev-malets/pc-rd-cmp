type s =
    {
        diagnostics : Res_diagnostics.t list;
        (*
        scope : (Res_grammar.t * Lexing.position) list;
        attrs : (Lexing.position * Parsetree.attributes) list;
        *)
        comments : Res_comment.t list;
        underscore_apply : bool;
    }

let default =
    {
        diagnostics = [];
        (*
        scope = [];
        attrs = [];
        *)
        comments = [];
        underscore_apply = false;
    }
