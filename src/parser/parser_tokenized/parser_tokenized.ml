module Lexer = Lexer
open Base
open Compilerlibs406
open Parsetree
open Ast_helper

module type TPC =
  Tokenized.Sigs.TPC
    with type tag = Token.t
     and type s = Pc_syntax.Basic.LogElement.t

let identifier's_character = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

module Make (Tpc : TPC) : Pc_syntax.Sigs.PARSER = struct
  module Base = struct
    module Comb = Tpc
    open Pc
    open Comb
    open Token

    let and' = named "tkn:and" & tkn_ And
    let as' = named "tkn:as" & tkn_ As
    let async = named "tkn:async" & tkn_ Async
    let await = named "tkn:await" & tkn_ Await
    let mutable' = named "tkn:mutable" & tkn_ Mutable
    let constraint' = named "tkn:constraint" & tkn_ Constraint
    let private' = named "tkn:private" & tkn_ Private
    let unpack = named "tkn:unpack" & tkn_ Unpack
    let external' = named "tkn:external" & tkn_ External
    let import = named "tkn:import" & tkn_ Import
    let from = named "tkn:from" & tkn_ From
    let let' = named "tkn:let" & tkn_ Let
    let module' = named "tkn:module" & tkn_ Module
    let with' = named "tkn:with" & tkn_ With
    let open' = named "tkn:open" & tkn_ Open
    let exception' = named "tkn:exception" & tkn_ Exception
    let switch = named "tkn:switch" & tkn_ Switch
    let try' = named "tkn:try" & tkn_ Try
    let catch = named "tkn:catch" & tkn_ Catch
    let else' = named "tkn:else" & tkn_ Else
    let to' = named "tkn:to" & tkn_ To
    let downto' = named "tkn:downto" & tkn_ Downto
    let for' = named "tkn:for" & tkn_ For
    let of' = named "tkn:of" & tkn_ Of
    let in' = named "tkn:in" & tkn_ In
    let if' = named "tkn:if" & tkn_ If
    let json_tag = named "tkn:json_tag" & tkn_ Json
    let while' = named "tkn:while" & tkn_ While
    let assert' = named "tkn:assert" & tkn_ Assert
    let lazy' = named "tkn:lazy" & tkn_ Lazy
    let true' = named "tkn:true" & tkn_ True
    let type' = named "tkn:type" & tkn_ Type
    let false' = named "tkn:false" & tkn_ False
    let sig' = named "tkn:sig" & tkn_ Sig
    let include' = named "tkn:include" & tkn_ Include
    let rec' = named "tkn:rec" & tkn_ Rec
    let nonrec' = named "tkn:nonrec" & tkn_ Nonrec
    let when' = named "tkn:when" & tkn_ When
    let _' = named "tkn:underscore" & tkn_ Underscore
    let ampersand_ampersand = tkn_ AmpersandAmptersand
    let ampersand = named "tkn:ampersand" & tkn_ Ampersand
    let arrow = named "tkn:arrow" & tkn_ Arrow
    let asterisk = named "tkn:asterisk" & tkn_ Asterisk

    let asterisk_asterisk =
      named "tkn:asterisk_asterisk" & tkn_ AsteriskAsterisk

    let asterisk_dot = named "tkn:asterisk_dot" & tkn_ AsteriskDot
    let at = named "tkn:at" & tkn_ At
    let at_at = named "tkn:at_at" & tkn_ AtAt
    let bang = named "tkn:bang" & tkn_ Bang
    let bang_eq = named "tkn:bang_eq" & tkn_ BangEq
    let bang_eq_eq = named "tkn:bang_eq_eq" & tkn_ BangEqEq
    let colon = named "tkn:colon" & tkn_ Colon
    let colon_eq = named "tkn:colon_eq" & tkn_ ColonEq
    let colon_gt = named "tkn:colon_gt" & tkn_ ColonGt
    let comma = named "tkn:comma" & tkn_ Comma
    let dot = named "tkn:dot" & tkn_ Dot
    let dot_dot = named "tkn:dot_dot" & tkn_ DotDot
    let ellipsis = named "tkn:ellipsis" & tkn_ Ellipsis
    let eq = named "tkn:eq" & tkn_ Eq
    let eq_eq = named "tkn:eq_eq" & tkn_ EqEq
    let eq_eq_eq = named "tkn:eq_eq_eq" & tkn_ EqEqEq
    let eq_op = named "tkn:eq_op" & failed arrow >> tkn_ Eq
    let gt = named "tkn:gt" & tkn_ Gt
    let gt_eq = named "tkn:gt_eq" & tkn_ Gt >> tkn_ Eq
    let hash = named "tkn:hash" & tkn_ Hash
    let hash_eq = named "tkn:hash_eq" & tkn_ HashEq
    let l_brace = named "tkn:l_brace" & tkn_ LBrace
    let l_bracket = named "tkn:l_bracket" & tkn_ LBracket
    let l_paren = named "tkn:l_paren" & tkn_ LParen
    let list = named "tkn:list" & tkn_ List
    let lt = named "tkn:lt" & tkn_ Lt
    let lt_eq = named "tkn:lt_eq" & tkn_ LtEq
    let minus = named "tkn:minus" & tkn_ Minus
    let minus_dot = named "tkn:minus_dot" & tkn_ MinusDot
    let minus_gt = named "tkn:minus_gt" & tkn_ MinusGt
    let percent = named "tkn:percent" & tkn_ Percent
    let percent_percent = named "tkn:percent_percent" & tkn_ PercentPercent
    let pipe = named "tkn:pipe" & tkn_ Pipe
    let pipe_gt = named "tkn:pipe_gt" & tkn_ PipeGt
    let pipe_pipe = named "tkn:pipe_pipe" & tkn_ PipePipe
    let plus = named "tkn:plus" & tkn_ Plus
    let plus_dot = named "tkn:plus_dot" & tkn_ PlusDot
    let plus_eq = named "tkn:plus_eq" & tkn_ PlusEq
    let plus_plus = named "tkn:plus_plus" & tkn_ PlusPlus
    let question = named "tkn:question" & tkn_ Question
    let r_brace = named "tkn:r_brace" & tkn_ RBrace
    let r_bracket = named "tkn:r_bracket" & tkn_ RBracket
    let r_paren = named "tkn:r_paren" & tkn_ RParen
    let slash = named "tkn:slash" & tkn_ Slash
    let slash_dot = named "tkn:slash_dot" & tkn_ SlashDot
    let tilda = named "tkn:tilda" & tkn_ Tilda

    let integer =
      named "tkn:integer"
      & tkn_payload Integer >>| fun {value; suffix} -> (value, suffix)

    let number =
      choice ~name:"tkn:number"
        [ (tkn_payload Integer
          >>| fun {value; suffix} -> Pconst_integer (value, suffix))
        ; (tkn_payload Float
          >>| fun {value; suffix} -> Pconst_float (value, suffix)) ]

    let character =
      named "tkn:character" & tkn_payload Character >>| fun c -> Pconst_char c

    let string_raw =
      named "tkn:string" & tkn_payload String >>| fun {value; raw = _} -> value

    let string_multiline =
      choice
        [ (tkn_payload String >>| fun {raw; value = _} -> raw)
        ; tkn_payload MultilineString ]
      >>| Const.string ~quotation_delimiter:"js"

    let template_no_template =
      tkn_payload TemplateTail >>| Const.string ~quotation_delimiter:"js"

    let constant =
      choice ~name:"pt:constant" [number; character; string_multiline]

    let l_ident =
      named "l_ident"
      & choice
          [ tkn_payload LIdent
          ; assert' >>$ "assert"
          ; catch >>$ "catch"
          ; downto' >>$ "downto"
          ; from >>$ "from"
          ; json_tag >>$ "json"
          ; to' >>$ "to"
          ; type' >>$ "type"
          ; unpack >>$ "unpack"
          ; with' >>$ "with"
          ; async >>$ "async"
          ; await >>$ "await" ]

    let u_ident = named "u_ident" & tkn_payload UIdent
    let ident = named "ident" & choice [l_ident; u_ident]
    let type_var = named "tkn:string:type_var" & tkn_payload TypeVar

    let single_line_comment =
      named "tkn:comment:s"
      & loc (tkn_payload Comment)
        >>| fun {txt; loc} -> Syntax.Res_comment.makeSingleLineComment ~loc txt

    let multi_line_comment =
      named "tkn:comment:m"
      & loc (tkn_payload MultilineComment)
        >>| fun {txt; loc} ->
        Syntax.Res_comment.makeMultiLineComment ~loc ~docComment:false
          ~standalone:false txt

    let comment =
      mapping (fun pt_pos x ->
          Pc_syntax.Basic.LogElement.Comment
            (Syntax.Res_comment.setPrevTokEndPos x pt_pos;
             x))
      <*> pos_end
      <*> choice [single_line_comment; multi_line_comment]

    let ng =
      named "pt:nongrammar"
      &
      let p = seq comment >>| Simple.log_many in
      let p1 = run p in
      {p1 with info = p.info}

    let ( <<. ) a b = a << ng << b
    let ( <*>. ) a b = a << ng <*> b

    let del_pos =
      choice
        [ pos_end <<. peek r_brace
        ; pos_end <<. peek r_paren
        ; pos_end <<. eof
        ; ng >> tkn_ Semicolon >> pos_end
        ; run
          & mapping (fun p1 p2 ->
                let open Simple in
                let open Lexing in
                match p1.pos_lnum = p2.pos_lnum with
                | true -> fail
                | false -> return p1)
            <*> pos_end <*>. pos ]

    let del =
      choice
        [ ng << peek r_brace
        ; ng << peek r_paren
        ; ng << eof
        ; ng << tkn_ Semicolon
        ; run
          & mapping (fun p1 p2 ->
                let open Simple in
                let open Lexing in
                match p1.pos_lnum = p2.pos_lnum with
                | true -> fail
                | false -> return ())
            <*> pos_end <*>. pos ]

    let template ~quote_tag ~expression =
      let open Pc_syntax.Basic in
      let open Parsetree in
      let open Ast_helper in
      let op = Hc.expr_id ["^"] in
      let template_part = tkn_payload TemplatePart in
      let template_tail = tkn_payload TemplateTail in
      let mk_const str p1 p2 =
        Exp.constant ~loc:(loc_mk p1 p2) ~attrs:[Hc.attr "res.template"]
        @@ Const.string ~quotation_delimiter:quote_tag str
      in
      let tail =
        fix
        @@ fun tail ->
        choice
          [ mapping (fun p1 str p2 prev ->
                let str = mk_const str p1 p2 in
                Exp.apply
                  ~loc:{prev.pexp_loc with loc_end = p2}
                  ~attrs:[Hc.attr "res.template"]
                  op
                  [(Nolabel, prev); (Nolabel, str)])
            <*> pos <*> template_tail <*> pos_end
          ; mapping (fun p1 str p2 expr tail prev ->
                let str = mk_const str p1 p2 in
                let e1 =
                  Exp.apply
                    ~loc:{prev.pexp_loc with loc_end = p2}
                    ~attrs:[Hc.attr "res.template"]
                    op
                    [(Nolabel, prev); (Nolabel, str)]
                in
                let e2 =
                  Exp.apply
                    ~loc:{prev.pexp_loc with loc_end = prev.pexp_loc.loc_start}
                    ~attrs:[Hc.attr "res.template"]
                    op
                    [(Nolabel, e1); (Nolabel, expr)]
                in
                tail e2)
            <*> pos <*> template_part <*> pos_end <*>. expression << ng <*> tail
          ]
      in
      choice
        [ mapping (fun p1 str p2 -> mk_const str p1 p2)
          <*> pos <*> template_tail <*> pos_end
        ; mapping (fun p1 str p2 expr tail ->
              let e0 = mk_const str p1 p2 in
              let e1 =
                Exp.apply ~loc:(loc_mk p1 p2)
                  ~attrs:[Hc.attr "res.template"]
                  op
                  [(Nolabel, e0); (Nolabel, expr)]
              in
              tail e1)
          <*> pos <*> template_part <*> pos_end <*>. expression << ng <*> tail
        ]

    let string_ident : string Comb.t =
      named "tkn:string_ident" & tkn_payload StringIdent
  end

  include Pc_syntax.Parser.Make (Base)
end
