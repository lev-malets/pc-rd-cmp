open Compilerlibs406
open Parsetree
open Ast_helper
open Base

module type APOS = Angstrom_pos.S with type s = Pc_syntax.Basic.LogElement.t

let identifier's_character = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

module Make (APos : APOS) : Pc_syntax.Sigs.PARSER = struct
  module Base = struct
    module Comb = APos
    open Pc
    open Comb

    let k x = s x >> failed (satisfy identifier's_character)
    let and' = k "and"
    let as' = k "as"
    let async = k "async"
    let await = k "await"
    let mutable' = k "mutable"
    let constraint' = k "constraint"
    let private' = k "private"
    let unpack = k "unpack"
    let external' = k "external"
    let import = k "import"
    let from = k "from"
    let let' = k "let"
    let module' = k "module"
    let with' = k "with"
    let open' = k "open"
    let exception' = k "exception"
    let switch = k "switch"
    let try' = k "try"
    let catch = k "catch"
    let else' = k "else"
    let to' = k "to"
    let downto' = k "downto"
    let for' = k "for"
    let of' = k "of"
    let in' = k "in"
    let if' = k "if"
    let json_tag = k "json"
    let while' = k "while"
    let assert' = k "assert"
    let lazy' = k "lazy"
    let true' = k "true"
    let type' = k "type"
    let false' = k "false"
    let sig' = k "sig"
    let include' = k "include"
    let rec' = k "rec"
    let nonrec' = k "nonrec"
    let when' = k "when"
    let _' = k "_"
    let ampersand = s "&"
    let ampersand_ampersand = s "&&"
    let arrow = s "=>"
    let asterisk = s "*"
    let asterisk_asterisk = s "**"
    let asterisk_dot = s "*."
    let at = s "@"
    let at_at = s "@@"
    let bang = s "!"
    let bang_eq = s "!="
    let bang_eq_eq = s "!=="
    let colon = s ":"
    let colon_eq = s ":="
    let colon_gt = s ":>"
    let comma = s ","
    let dot = s "."
    let dot_dot = s ".."
    let ellipsis = s "..."
    let eq = s "="
    let eq_eq = s "=="
    let eq_eq_eq = s "==="
    let eq_op = failed arrow >> s "="
    let gt = s ">"
    let gt_eq = s ">="
    let hash = s "#"
    let hash_eq = s "#="
    let l_brace = s "{"
    let l_bracket = s "["
    let l_paren = s "("
    let list = s "list{"
    let lt = s "<"
    let lt_eq = s "<="
    let minus = s "-"
    let minus_dot = s "-."
    let minus_gt = s "->"
    let percent = s "%"
    let percent_percent = s "%%"
    let pipe = s "|"
    let pipe_gt = s "|>"
    let pipe_pipe = s "||"
    let plus = s "+"
    let plus_dot = s "+."
    let plus_eq = s "+="
    let plus_plus = s "++"
    let question = s "?"
    let r_brace = s "}"
    let r_bracket = s "]"
    let r_paren = s ")"
    let slash = s "/"
    let slash_dot = s "/."
    let tilda = s "~"

    let exp_sign =
      skip
      @@ function
      | '-' | '+' -> true
      | _ -> false

    let value_part digit exp =
      let skip_digits =
        skip_while (fun c -> Caml.( || ) (digit c) (Char.equal c '_'))
      in
      let int = skip digit >> skip_digits in
      let float = int >> dot >> skip_digits in
      match exp with
      | None -> choice [float >>$ true; int >>$ false]
      | Some exp ->
          let exp = skip exp >> opt exp_sign >> skip digit >> skip_digits in
          choice
            [ float >> exp >>$ true
            ; float >>$ true
            ; int >> exp >>$ true
            ; int >>$ false ]

    let value_part_2 =
      value_part
        (function
          | '0' .. '1' -> true
          | _ -> false)
        None

    let value_part_8 =
      value_part
        (function
          | '0' .. '7' -> true
          | _ -> false)
        None

    let value_part_10 =
      value_part
        (function
          | '0' .. '9' -> true
          | _ -> false)
        (Some
           (function
           | 'e' | 'E' -> true
           | _ -> false))

    let value_part_16 =
      value_part
        (function
          | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
          | _ -> false)
        (Some
           (function
           | 'p' | 'P' -> true
           | _ -> false))

    let value_part =
      let nondec =
        s "0"
        >> choice ~name:"pa:value_part:0"
             [ s "b" >> value_part_2
             ; s "B" >> value_part_2
             ; s "o" >> value_part_8
             ; s "O" >> value_part_8
             ; s "x" >> value_part_16
             ; s "X" >> value_part_16
             ; value_part_8 ]
      in
      choice [nondec; value_part_10]

    let suffix_part =
      opt
      @@ satisfy (function
           | 'g' .. 'z' | 'G' .. 'Z' -> true
           | _ -> false)

    let number =
      named "pa:number"
      & mapping (fun (is_float, value) suffix ->
            match is_float with
            | true -> Pconst_float (value, suffix)
            | false -> Pconst_integer (value, suffix))
        <*> with_literal value_part <*> suffix_part

    let code offset basic c = Int.(Char.to_int c - Char.to_int basic + offset)

    let octal_code =
      satisfy (function
        | '0' .. '7' -> true
        | _ -> false)
      >>| code 0 '0'

    let decimal_code =
      satisfy (function
        | '0' .. '9' -> true
        | _ -> false)
      >>| code 0 '0'

    let oct_digit =
      satisfy (function
        | '0' .. '7' -> true
        | _ -> false)

    let dec_digit =
      satisfy (function
        | '0' .. '9' -> true
        | _ -> false)

    let hex_digit =
      satisfy (function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false)

    let hexadecimal_code =
      choice
        [ decimal_code
        ; satisfy (function
            | 'a' .. 'f' -> true
            | _ -> false)
          >>| code 10 'a'
        ; satisfy (function
            | 'A' .. 'F' -> true
            | _ -> false)
          >>| code 10 'A' ]

    let hex_cc =
      mapping (fun _1 _2 -> (_1 * 16) + _2)
      << s "x" <*> hexadecimal_code <*> hexadecimal_code

    let dec_cc =
      mapping (fun _1 _2 _3 -> (_1 * 100) + (_2 * 10) + _3)
      <*> (satisfy (function
             | '0' .. '2' -> true
             | _ -> false)
          >>| code 0 '0')
      <*> decimal_code <*> decimal_code

    let oct_cc =
      mapping (fun _1 _2 _3 -> (_1 * 64) + (_2 * 8) + _3)
      << s "o"
      <*> (satisfy (function
             | '0' .. '3' -> true
             | _ -> false)
          >>| code 0 '0')
      <*> octal_code <*> octal_code

    let escaped =
      choice ~name:"pa:char:escaped"
        [ s "\\" >>$ '\\'
        ; s "\'" >>$ '\''
        ; s " " >>$ ' '
        ; s "n" >>$ '\n'
        ; s "t" >>$ '\t'
        ; s "b" >>$ '\b'
        ; s "r" >>$ '\r'
        ; hex_cc >>| Char.of_int_exn
        ; dec_cc >>| Char.of_int_exn
        ; oct_cc >>| Char.of_int_exn ]

    let p =
      named "const:char"
        (s "\'"
        >> choice
             [ s "\\" >> escaped
             ; satisfy (function
                 | '\\' | '\'' | '\n' | '\t' | '\b' | '\r' -> false
                 | _ -> true) ]
        << s "\'")

    let character = p >>| Const.char

    let escaped_in_string =
      choice ~name:"pa:escaped_in_string"
        [ s "\\" >>$ "\\\\"
        ; s "\"" >>$ "\\\""
        ; s "n" >>$ "\\n"
        ; s "t" >>$ "\\t"
        ; s "b" >>$ "\\b"
        ; s "r" >>$ "\\r"
        ; s "0" >>$ "\\0"
        ; consumed (s "u" >> hex_digit >> hex_digit >> hex_digit >> hex_digit)
          >>| ( ^ ) "\\"
        ; consumed (s "x" >> hex_digit >> hex_digit) >>| ( ^ ) "\\"
        ; consumed (s "o" >> oct_digit >> oct_digit >> oct_digit) >>| ( ^ ) "\\"
        ; consumed (dec_digit >> dec_digit >> dec_digit) >>| ( ^ ) "\\" ]

    let string_raw =
      let parts =
        s "\""
        >> fix
           @@ fun loop ->
           let not_escaped = function
             | '\\' | '\"' | '\n' | '\t' | '\b' | '\r' -> false
             | _ -> true
           in
           cons <*> take_while not_escaped
           <*> choice
                 [s "\"" >>$ []; cons <*> (s "\\" >> escaped_in_string) <*> loop]
      in
      parts >>| String.concat

    let string_ml_helper ~q =
      let list =
        s q
        >> fix
           @@ fun loop ->
           cons
           <*> take_while (function
                 | '\n' | '\\' | '\r' -> false
                 | c -> not (Char.equal c q.[0]))
           <*> choice
                 [ s q >>$ []
                 ; cons
                   <*> choice
                         [ new_line
                         ; (s "\\" >> dec_cc
                           >>| fun c -> "\\" ^ Pc.Utils.to_hex_string c)
                         ; s ("\\" ^ q) >>$ "\\" ^ q
                         ; s "\\\\" >>$ "\\\\"
                         ; s "\\" >>$ "\\" ]
                   <*> loop ]
      in
      list
      >>| fun l -> Const.string ~quotation_delimiter:"js" @@ String.concat l

    let string_multiline = named "const:string:ml" & string_ml_helper ~q:"\""
    let template_no_template = string_ml_helper ~q:"`"

    let constant =
      choice ~name:"pa:constant" [number; character; string_multiline]

    let upper = function
      | 'A' .. 'Z' -> true
      | _ -> false

    let lower = function
      | 'a' .. 'z' | '_' -> true
      | _ -> false

    let c_ident first =
      consumed (skip first >> skip_while identifier's_character)

    let l_ident = named "l_ident" @@ failed @@ k "_" >> c_ident lower
    let u_ident = named "u_ident" @@ c_ident upper
    let ident = named "ident" & choice [l_ident; u_ident]
    let type_var = s "\'" >> ident

    let integer =
      run
      & mapping (fun n ->
            let open Simple in
            match n with
            | Pconst_integer (n, s) -> return (n, s)
            | _ -> fail)
        <*> number

    let single_line_comment =
      let p =
        s "//"
        >> loc
           @@ take_while (function
                | '\n' | '\r' -> false
                | _ -> true)
      in
      p >>| fun {txt; loc} -> Syntax.Res_comment.makeSingleLineComment ~loc txt

    let multi_line_comment =
      let parts =
        fix
        @@ fun parts ->
        cons
        <*> take_while (function
              | '*' | '\n' | '/' | '\r' -> false
              | _ -> true)
        <*> choice
              [ s "*/" >>$ []
              ; cons <*> new_line <*> parts
              ; mapping (fun l tail -> "/*" :: (l @ ("*/" :: tail)))
                << s "/*" <*> parts <*> parts
              ; cons <*> (any_char >>| String.make 1) <*> parts ]
      in
      let p = s "/*" >> parts in
      loc (p >>| String.concat)
      >>| fun {txt; loc} ->
      Syntax.Res_comment.makeMultiLineComment ~loc ~docComment:false
        ~standalone:false txt

    let comment =
      choice [single_line_comment; multi_line_comment]
      >>| fun x pos comments ->
      Pc_syntax.Basic.LogElement.Comment
        (Syntax.Res_comment.setPrevTokEndPos x pos;
         x)
      :: comments

    let comments =
      fold_left_0_n
        (t2 <*> comment <*> pos << whitespace)
        (t2 <*> comment <*> pos << whitespace)
        ~f:(fun (f1, p1) (f2, p2) ->
          ((fun pos comments -> f2 p1 @@ f1 pos comments), p2))

    let ng =
      named "pa:nongrammar"
      &
      let p =
        mapping (fun pos x ->
            match x with
            | Some (hlp, _) -> Simple.log_many (hlp pos [])
            | _ -> Simple.return ())
        <*> pos << whitespace <*> opt comments
      in
      let p2 = run p in
      {p2 with info = p.info}

    let ( <<. ) a b = a << ng << b
    let ( <*>. ) a b = a << ng <*> b

    let del_pos =
      run
      & mapping (fun p1 c ->
            let open Angstrom in
            let open Simple in
            match c with
            | Some '}' -> return p1
            | Some ')' -> return p1
            | Some ';' -> advance 1 >> APos.pos.p
            | None -> return p1
            | _ -> (
                APos.pos.p
                >>= fun p2 ->
                match p1.pos_lnum = p2.pos_lnum with
                | true -> fail
                | false -> return p1))
        <*> pos <*>. peek_char

    let del =
      run
      & mapping (fun p1 c ->
            let open Angstrom in
            let open Simple in
            match c with
            | Some '}' -> return ()
            | Some ')' -> return ()
            | Some ';' -> advance 1 >>$ ()
            | None -> return ()
            | _ -> (
                APos.pos.p
                >>= fun p2 ->
                match p1.Lexing.pos_lnum = p2.pos_lnum with
                | true -> fail
                | false -> return ()))
        <*> pos <*>. peek_char

    let template ~quote_tag ~expression =
      let open Pc_syntax.Basic in
      let open Parsetree in
      let open Ast_helper in
      let op = Hc.expr_id ["^"] in
      let cons = cons in
      let parts =
        fix
        @@ fun parts ->
        cons
        <*> take_while (function
              | '\n' | '$' | '`' | '\\' | '\r' -> false
              | _ -> true)
        <*> choice
              [ cons <*> new_line <*> parts
              ; cons <*> (s "\\`" >>$ "\\`") <*> parts
              ; cons <*> (s "\\$" >>$ "\\$") <*> parts
              ; cons <*> (s "\\\\" >>$ "\\\\") <*> parts
              ; cons <*> (s "$" >> failed l_brace >>$ "$") <*> parts
              ; return [] ]
      in
      let string =
        mapping (fun l p1 p2 ->
            let str = String.concat l in
            Exp.constant ~loc:(loc_mk p1 p2) ~attrs:[Hc.attr "res.template"]
            @@ Const.string ~quotation_delimiter:quote_tag str)
        <*> parts
      in
      let string_part =
        fix
        @@ fun string_part ->
        let tail =
          mapping (fun expr pos tail prev ->
              tail
                (Exp.apply
                   ~loc:(loc_mk prev.pexp_loc.loc_start pos)
                   ~attrs:[Hc.attr "res.template"]
                   op
                   [(Nolabel, prev); (Nolabel, expr)]))
          << s "${" <*>. expression <<. r_brace <*> pos <*> string_part
        in
        pos && string
        && choice
             [ mapping (fun p2 str p1 prev ->
                   Exp.apply
                     ~loc:{prev.pexp_loc with loc_end = p2}
                     ~attrs:[Hc.attr "res.template"]
                     op
                     [(Nolabel, prev); (Nolabel, str p1 p2)])
               << s "`" <*> pos
             ; mapping (fun p2 tail str p1 prev ->
                   tail
                   @@ Exp.apply
                        ~loc:{prev.pexp_loc with loc_end = p2}
                        ~attrs:[Hc.attr "res.template"]
                        op
                        [(Nolabel, prev); (Nolabel, str p1 p2)])
               <*> pos <*> tail ]
      in
      pos
      && s "`"
         >> (string
            && choice
                 [ mapping (fun p2 str p1 -> str p1 p2) << s "`" <*> pos
                 ; mapping (fun pos1 expr pos2 tail str p1 ->
                       let e0 = str p1 pos1 in
                       let e1 =
                         Exp.apply ~loc:(loc_mk p1 pos2)
                           ~attrs:[Hc.attr "res.template"]
                           op
                           [(Nolabel, e0); (Nolabel, expr)]
                       in
                       tail e1)
                   <*> pos << s "${" <*>. expression <<. r_brace <*> pos
                   <*> string_part ])

    let string_ident = s "\\" >> string_raw
  end

  include Pc_syntax.Parser.Make (Base)
end
