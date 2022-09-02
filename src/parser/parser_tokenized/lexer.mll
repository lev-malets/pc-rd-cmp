{
open Token
open Tokenized.Parser
open Core

let char_code offset basic c = (Char.to_int c - Char.to_int basic + offset)
let cc16 c =
    let open Char in
    if c < 'A' then
        char_code 0 '0' c
    else if c < 'a' then
        char_code 10 'A' c
    else
        char_code 10 'a' c
let cc10 = char_code 0 '0'
let cc8  = char_code 0 '0'

module Make () = struct

module Tag = Token

let buf = Buffer.create 128
let buf2 = Buffer.create 128
let flag = ref false
let interpolation_cnt = ref 0
}

let upper = ['A' - 'Z']
let lower = ['a' - 'z']
let ident_char = upper | lower | ['0' - '9'] | '_' | '\''

let digit_2 = ['0'-'1']
let digits_2 = digit_2 (digit_2 | '_')*
let digit_8 = ['0'-'7']
let digits_8 = digit_8 (digit_8 | '_')*
let digit_10 = ['0'-'9']
let digits_10 = digit_10 (digit_10 | '_')*
let digit_16 = ['0'-'9'] | ['A'-'F'] | ['a'-'f']
let digits_16 = digit_16 (digit_16 | '_')*
let suffix = ['g'-'z'] | ['G' - 'Z']

let int_2 = ("0b" | "0B") digits_2
let int_8 = ("0o" | "0O" | "0") digits_8
let int_10 = digits_10
let int_16 = ("0x" | "0X") digits_16

let float_2 = int_2 '.' digits_2?
let float_8 = int_8 '.' digits_8?
let float_10 =
        (int_10 '.' digits_10?)
    |   (int_10 ('.' digits_10)? ('e' | 'E') ('+' | '-')? digits_10)
let float_16 =
        (int_16 '.' digits_16?)
    |   (int_16 ('.' digits_16)? ('p' | 'P') ('+' | '-')? digits_16)

rule scan_space =
    parse
    | [' ' '\t']+ { scan_space lexbuf }
    | '\n' | "\r\n" { Lexing.new_line lexbuf; scan_space lexbuf }
    | "" { () }

and scan_token =
    parse
    | "//"
        {
            Buffer.clear buf;
            parse_scomment lexbuf;
            let s = Buffer.contents buf in
            with_payload Comment s
        }
    | "/*"
        {
            Buffer.clear buf;
            parse_mcomment lexbuf;
            let s = Buffer.contents buf in
            with_payload MultilineComment s
        }
    | "&&" { Tag AmpersandAmptersand }
    | "&" { Tag Ampersand }
    | "=>" { Tag Arrow }
    | "**" { Tag AsteriskAsterisk }
    | "*." { Tag AsteriskDot }
    | "*" { Tag Asterisk }
    | "@@" { Tag AtAt }
    | "@" { Tag At }
    | "!==" { Tag BangEqEq }
    | "!=" { Tag BangEq }
    | "!" { Tag Bang }
    | ":=" { Tag ColonEq }
    | ":>" { Tag ColonGt }
    | ":" { Tag Colon }
    | "," { Tag Comma }
    | "..." { Tag Ellipsis }
    | ".." { Tag DotDot }
    | "." { Tag Dot }
    | "===" { Tag EqEqEq }
    | "==" { Tag EqEq }
    | "=" { Tag Eq }
    | ">" { Tag Gt }
    | "#=" { Tag HashEq }
    | "#" { Tag Hash }
    | "{" { Tag LBrace }
    | "[" { Tag LBracket }
    | "(" { Tag LParen }
    | "list{" { Tag List }
    | "<=" { Tag LtEq }
    | "<" { Tag Lt }
    | "-." { Tag MinusDot }
    | "->" { Tag MinusGt }
    | "-" { Tag Minus }
    | "%%" { Tag PercentPercent }
    | "%" { Tag Percent }
    | "|>" { Tag PipeGt }
    | "||" { Tag PipePipe }
    | "|" { Tag Pipe }
    | "+." { Tag PlusDot }
    | "+=" { Tag PlusEq }
    | "++" { Tag PlusPlus }
    | "+" { Tag Plus }
    | "?" { Tag Question }
    | "}"
        {
            if !interpolation_cnt > 0 then
                begin
                    interpolation_cnt := !interpolation_cnt - 1;
                    Buffer.clear buf;
                    parse_template_string lexbuf
                end
            else
                Tag RBrace
        }
    | "]" { Tag RBracket }
    | ")" { Tag RParen }
    | ";" { Tag Semicolon }
    | "/" { Tag Slash }
    | "/." { Tag SlashDot }
    | "~" { Tag Tilda }
    | ['A' - 'Z'] ident_char* { with_payload UIdent (Lexing.lexeme lexbuf) }
    | ['a' - 'z'] ident_char*
        {
            match Lexing.lexeme lexbuf with
            | "and" -> Tag And
            | "as" -> Tag As
            | "async" -> Tag Async
            | "assert" -> Tag Assert
            | "await" -> Tag Await
            | "catch" -> Tag Catch
            | "constraint" -> Tag Constraint
            | "downto" -> Tag Downto
            | "else" -> Tag Else
            | "exception" -> Tag Exception
            | "external" -> Tag External
            | "false" -> Tag False
            | "for" -> Tag For
            | "from" -> Tag From
            | "if" -> Tag If
            | "in" -> Tag In
            | "include" -> Tag Include
            | "import" -> Tag Import
            | "json" -> Tag Json
            | "lazy" -> Tag Lazy
            | "let" -> Tag Let
            | "module" -> Tag Module
            | "mutable" -> Tag Mutable
            | "nonrec" -> Tag Nonrec
            | "of" -> Tag Of
            | "open" -> Tag Open
            | "private" -> Tag Private
            | "rec" -> Tag Rec
            | "sig" -> Tag Sig
            | "switch" -> Tag Switch
            | "to" -> Tag To
            | "true" -> Tag True
            | "try" -> Tag Try
            | "type" -> Tag Type
            | "unpack" -> Tag Unpack
            | "when" -> Tag When
            | "while" -> Tag While
            | "with" -> Tag With
            | s -> with_payload LIdent s
        }
    | '_' ident_char+ { with_payload LIdent (Lexing.lexeme lexbuf) }
    | '_' { Tag Underscore }

    | (int_2 | int_8 | int_10 | int_16 as value) (suffix as suffix)
        { with_payload Integer { value; suffix = Some suffix } }
    | (int_2 | int_8 | int_10 | int_16 as value)
        { with_payload Integer { value; suffix = None } }
    | (float_2 | float_8 | float_10 | float_16 as value) (suffix as suffix)
        { with_payload Float { value; suffix = Some suffix } }
    | (float_2 | float_8 | float_10 | float_16 as value)
        { with_payload Float { value; suffix = None } }
    | "'" [^ '\\' '\n' '\t' '\b' '\r'] "'" as x { with_payload Character x.[1] }
    | "'\\"
        {
            let x = char_escaped lexbuf in
            char_end lexbuf;
            with_payload Character x
        }
    | '"'
        {
            Buffer.clear buf;
            Buffer.clear buf2;
            flag := false;
            parse_string lexbuf;
            if !flag then
                with_payload MultilineString (Buffer.contents buf2)
            else
                with_payload String
                    {
                        value = Buffer.contents buf;
                        raw = Buffer.contents buf2;
                    }
        }
    | '`'
        {
            Buffer.clear buf;
            parse_template_string lexbuf
        }
    | "'" ((upper | lower) ident_char* as x) { with_payload TypeVar x }
    | "\\\""
        {
            Buffer.clear buf;
            parse_string_ident lexbuf;
            let s = Buffer.contents buf in
            with_payload StringIdent s
        }
    | eof { Eof }
and char_end = parse | "'" { () }

and char_escaped =
    parse
    | "x" (digit_16 as x1) (digit_16 as x2)
        { Char.of_int_exn @@ cc16 x1 * 16 + cc16 x2 }
    | (digit_10 as x1) (digit_10 as x2) (digit_10 as x3)
        { Char.of_int_exn @@ cc10 x1 * 100 + cc10 x2 * 10 + cc10 x3 }
    | "o" (digit_8 as x1) (digit_8 as x2) (digit_8 as x3)
        { Char.of_int_exn @@ cc8 x1 * 64 + cc8 x2 * 8 + cc8 x3 }
    | '\\' { '\\' }
    | '\'' { '\'' }
    | ' ' { ' ' }
    | 'n' { '\n' }
    | 't' { '\t' }
    | 'b' { '\b' }
    | 'r' { '\r' }
and char_escaped_for_string =
    parse
    | "x" digit_16 digit_16 as x
        { Some (x, x) }
    | (digit_10 as x1) (digit_10 as x2) (digit_10 as x3) as x
        {
            let code = cc10 x1 * 100 + cc10 x2 * 10 + cc10 x3 in
            let hex = Pc.Utils.to_hex_string code in
            Some (x, hex)
        }
    | "o" digit_8 digit_8 digit_8 as x
        { Some (x, x) }
    | '\\' { Some ("\\", "\\") }
    | '\'' { Some ("\'", "\'") }
    | ' ' { Some (" ", " ") }
    | 'n' { Some ("n", "n") }
    | 't' { Some ("t", "t") }
    | 'b' { Some ("b", "b") }
    | 'r' { Some ("r", "r") }
    | 'u' digit_16 digit_16 digit_16 digit_16 as x { Some (x, x) }
    | '0' { Some ("0", "0") }
    | "" { None }
and parse_string_ident =
    parse
    | [^ '\\' '"' '\n' '\t' '\b' '\r']+ as x
        {
            Buffer.add_string buf x;
            parse_string_ident lexbuf
        }
    | "\\\"" as x
        {
            Buffer.add_string buf x;
            parse_string_ident lexbuf
        }
    | '"' { () }
and parse_template_string =
    parse
    | [^ '\\' '`' '\n' '\r' '$']+
    | "\\`" | "\\$" | "\\\\" as x
        {
            Buffer.add_string buf x;
            parse_template_string lexbuf
        }
    | '\n' | "\r\n" as x
        {
            Lexing.new_line lexbuf;
            Buffer.add_string buf x;
            parse_template_string lexbuf
        }
    | "${"
        {
            interpolation_cnt := !interpolation_cnt + 1;
            with_payload TemplatePart (Buffer.contents buf)
        }
    | "$"
        {
            Buffer.add_char buf '$';
            parse_template_string lexbuf
        }
    | '`' { with_payload TemplateTail (Buffer.contents buf) }
and parse_string =
    parse
    | [^ '\\' '"' '\n' '\t' '\b' '\r']+ as x
        {
            Buffer.add_string buf x;
            Buffer.add_string buf2 x;
            parse_string lexbuf
        }
    | '\n' | "\r\n" as x
        {
            Lexing.new_line lexbuf;
            flag := true;
            Buffer.add_string buf2 x;
            parse_string lexbuf
        }
    | ['\t' '\b']+ as x
        {
            flag := true;
            Buffer.add_string buf2 x;
            parse_string lexbuf
        }
    | "\\\"" as x
        {
            Buffer.add_string buf x;
            Buffer.add_string buf2 x;
            parse_string lexbuf
        }
    | "\\"
        {
            begin match char_escaped_for_string lexbuf with
            | Some (x, s) ->
                Buffer.add_char buf '\\';
                Buffer.add_string buf x;
                Buffer.add_char buf2 '\\';
                Buffer.add_string buf2 s
            | None ->
                flag := true;
                Buffer.add_char buf2 '\\'
            end;
            parse_string lexbuf
        }
    | '"' { () }

and parse_scomment =
    parse
    | [^ '\n' '\r']+ as x
        {
            Buffer.add_string buf x;
            parse_scomment lexbuf
        }
    | "\n" | "\r\n" { Lexing.new_line lexbuf }

and parse_mcomment =
    parse
    | [^ '\n' '\r' '*' '/']+ as x
        {
            Buffer.add_string buf x;
            parse_mcomment lexbuf
        }
    | '\n' | "\r\n" as x
        {
            Lexing.new_line lexbuf;
            Buffer.add_string buf x;
            parse_mcomment lexbuf
        }
    | "*/" { () }
    | "*" as x
        {
            Buffer.add_char buf x;
            parse_mcomment lexbuf
        }
    | "/*"
        {
            Buffer.add_string buf "/*";
            parse_mcomment lexbuf;
            Buffer.add_string buf "*/";
            parse_mcomment lexbuf;
        }
    | "/" as x
        {
            Buffer.add_char buf x;
            parse_mcomment lexbuf
        }

{ end }
