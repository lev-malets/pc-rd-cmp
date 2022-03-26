{
open Token

let char_code offset basic c = (Char.code c - Char.code basic + offset)
let cc16 c =
    if c < 'A' then
        char_code 0 '0' c
    else if c < 'a' then
        char_code 10 'A' c
    else
        char_code 10 'a' c
let cc10 = char_code 0 '0'
let cc8  = char_code 0 '0'

let buf = Buffer.create 128
let buf2 = Buffer.create 128
let flag = ref false
}

let digit_2 = ['0'-'1']
let digit_8 = ['0'-'7']
let digit_10 = ['0'-'9']
let digit_16 = ['0'-'9'] | ['A'-'F'] | ['a'-'f']
let suffix = ['g'-'z'] | ['G' - 'Z']

let int_2 = digit_2+
let int_8 = digit_8+
let int_10 = digit_10+
let int_16 = digit_16+

let float_2 = int_2 '.' digit_2*
let float_8 = int_8 '.' digit_8*
let float_10 = int_10 '.' digit_10* (('e' | 'E') ('+' | '-')? digit_10+)?
let float_16 = int_16 '.' digit_16* (('p' | 'P') ('+' | '-')? digit_16+)?

rule scan_space =
    parse
    | [' ' '\t']+ { scan_space lexbuf }
    | '\n' | "\r\n" { Lexing.new_line lexbuf; scan_space lexbuf }
    | "" { () }

and scan_token =
    parse
    | (int_2 | int_8 | int_10 | int_16 as value) (suffix? as suffix)
        { Integer { value; suffix = suffix.[0] } }
    | (float_2 | float_8 | float_10 | float_16 as value) (suffix? as suffix)
        { Float { value; suffix = suffix.[0] } }
    | "'" [^ '\\' '\n' '\t' '\b' '\r'] "'" as x { Character x.[1] }
    | "'\\"
        {
            let x = char_escaped lexbuf in
            char_end lexbuf;
            x
        }
    | "let"        { Let }
    | "type"       { Type }
    | "and"        { And }
    | "as"         { As }
    | "assert"     { Assert }
    | "constraint" { Constraint }
    | "else"       { Else }
    | "exception"  { Exception }
    | "export"     { Export }
    | "external"   { External }
    | "["          { LBracket }
    | "]"          { RBracket }
    | "{"          { LBrace }
    | "}"          { RBrace }
    | "("          { LParen }
    | ")"          { RParen }
    | "<"          { Lt }
    | ">"          { Gt }
    | '"'
        {
            Buffer.clear buf;
            Buffer.clear buf2;
            flag := false;
            parse_string lexbuf;
            if !flag then
                MultilineString (Buffer.contents buf2)
            else
                String {
                    value = Buffer.contents buf;
                    raw = Buffer.contents buf2;
                }
        }
and char_end = parse | "'" { () }

and char_escaped =
    parse
    | "x" digit_16 digit_16 as x
        {
            let x1 = cc16 x.[1] in
            let x2 = cc16 x.[2] in
            CharacterCode (x1 * 16 + x2)
        }
    | digit_10 digit_10 digit_10 as x
        { CharacterCode (cc10 x.[0] * 100 + cc10 x.[1] * 10 + cc10 x.[2]) }
    | "o" digit_8 digit_8 digit_8 as x
        { CharacterCode (cc8 x.[1] * 64 + cc8 x.[2] * 8 + cc8 x.[3]) }
    | '\\' { Character '\\' }
    | '\'' { Character '\'' }
    | ' ' { Character ' ' }
    | 'n' { Character '\n' }
    | 't' { Character '\t' }
    | 'b' { Character '\b' }
    | 'r' { Character '\r' }
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
    | '"' { () }

and parse_scomment =
    parse
    | [^ '\n' '\r']+ as x { Comment x }

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
