let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()
let fmt = Fmt.string
let fmt_err = Fmt.styled `Red Fmt.string
let print = fmt Fmt.stderr
let eprint = fmt_err Fmt.stderr

let point str f =
    let open Basic.Angstrom in
    let open Let_syntax in
    let open State in
    let p = f () in

    let%bind old_state = get_state in
    let _ = print @@ Printf.sprintf "%s|%2d| %s\n" old_state.trace.prefix (old_state.trace.count + 1) str in

    let state_revert =
        map_state (fun s -> {s with trace = {old_state.trace with count = old_state.trace.count + 1}})
    in

    let fail =
        let%bind pos = position in
        eprint @@ Printf.sprintf "%s     err @ %s:%d:%d\n" old_state.trace.prefix pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
        fail str
    in

    map_state (fun s -> {s with trace = {prefix = "   | " ^ s.trace.prefix; count = 0}})
    >>
    ((p >>| fun x -> print @@ Printf.sprintf "%s     exit\n" old_state.trace.prefix; x) <|> fail)
    <<
    state_revert

module Stub = struct
    let point _str f = f ()
end
