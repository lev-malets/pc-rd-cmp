
open State

module Make(T : sig type t end) : Sigs.POS with type state = T.t = struct
    module Angstrom = Angstrom_mod.Make(struct type t = T.t State.t end)

    include Angstrom
    open Let_syntax

    type state = T.t

    let (>>) = ( *> )
    let (<<) = ( <* )

    let parse_string p state ?(filename = "none") =
        parse_string ~consume:All p (State.make filename state)

    let make_position (state : Parser.pstate) ~prev =
        let open Angstrom_mod__Parser in
        let {line; prev_line_start; info; _} = state.custom in
        let (pos_lnum, pos_bol, pos_cnum) = match prev with
            | false -> (line.no, line.start, state.pos)
            | true ->
                match line.start = state.pos with
                | true -> (line.no - 1, prev_line_start, line.start - 1)
                | false -> (line.no, line.start, state.pos - 1)
        in

        let open Lexing in
        { info.default_position with
            pos_lnum;
            pos_bol;
            pos_cnum;
        }

    open Angstrom_mod.Parser

    let (>>$) p v =
        { run = fun input pos state more fail succ ->
            let succ input pos state more _ = succ input pos state more v in
            p.run input pos state more fail succ
        }

    let position =
        { run = fun input pos state more _fail succ ->
            succ input pos state more @@ make_position (pstate_exported pos state) ~prev:false
        }

    let end_position =
        { run = fun input pos state more _fail succ ->
            match pos + state.buffer_start - 1 = state.custom.ws_end with
            | true -> succ input pos state more state.custom.token_end
            | false -> succ input pos state more @@ make_position (pstate_exported pos state) ~prev:false
        }

    let whitespace =
        { run = fun input pos state more fail succ ->
            let module Input = Angstrom_mod__Input in
            let len = Input.length input in
            let char = Input.unsafe_get_char input in

            let succ lines start pos1 =
                let new_state = match pos1 = pos with
                    | true -> state
                    | false ->
                        let token_end = make_position (pstate_exported pos state) ~prev:false in
                        let ws_end = pos1 - 1 in

                        match lines with
                        | 0 ->
                            { state with custom = { state.custom with token_end; ws_end } }
                        | lines ->
                            { state with
                                custom =
                                    { state.custom with
                                        line = Line.advance state.custom.line lines start;
                                        token_end;
                                        ws_end;
                                    }
                            }
                in
                succ input pos1 new_state more ()
            in
            let rec loop lines start pos =
                match pos < len with
                | true ->
                    begin match char pos with
                    | ' ' -> loop lines start @@ pos + 1
                    | '\n' -> loop (lines + 1) (pos + 1) @@ pos + 1
                    | '\t' -> loop lines start @@ pos + 1
                    | '\r' ->
                        begin match pos + 1 < len && char (pos + 1) = '\n' with
                        | true -> loop (lines + 1) (pos + 2) @@ pos + 2
                        | false -> fail input pos state more [] "ws_pos"
                        end
                    | _ -> succ lines start pos
                    end
                | false -> succ lines start pos
            in
            loop 0 0 pos
        }

    let newline_skipped =
        { run = fun input pos state more fail succ ->
            match state.custom.line.no <> state.custom.token_end.pos_lnum with
            | true -> succ input pos state more ()
            | false -> fail input pos state more [] "newline_skipped"
        }

    let get_state = state_get >>| fun s -> s.custom.custom
    let map_state f = state_map (fun s -> {s.custom with custom = f s.custom.custom})
    let set_state custom = map_state (fun _ -> custom)

    module Alt = struct
        let position =
            let%map state = state_get in
            make_position state ~prev:false

        let end_position =
            let%map state = state_get in
            match state.pos - 1 = state.custom.ws_end with
            | true -> state.custom.token_end
            | false -> make_position state ~prev:true
    end
end
