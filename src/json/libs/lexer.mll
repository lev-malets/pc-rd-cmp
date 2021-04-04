{
open Tokens
open Opal
open Lexing

let string_buf = Buffer.create 128

}

let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit*
let white = ([' ' '\n' '\t'] | "\r\n")+

rule read = parse
    | white    { read lexbuf }
    | int      { let lexeme = lexeme lexbuf in LazyStream.Cons (Int (int_of_string lexeme), lazy (read lexbuf)) }
    | float    { let lexeme = lexeme lexbuf in LazyStream.Cons (Float (float_of_string lexeme), lazy (read lexbuf)) }
    | "true"   { LazyStream.Cons (Bool true, lazy (read lexbuf)) }
    | "false"  { LazyStream.Cons (Bool false, lazy (read lexbuf)) }
    | "null"   { LazyStream.Cons (Null, lazy (read lexbuf)) }
    | '"'      { let str = Buffer.clear string_buf; read_string lexbuf in LazyStream.Cons (String str, lazy (read lexbuf)) }
    | '{'      { LazyStream.Cons (ObjectBegin, lazy (read lexbuf)) }
    | '}'      { LazyStream.Cons (ObjectEnd, lazy (read lexbuf)) }
    | '['      { LazyStream.Cons (ArrayBegin, lazy (read lexbuf)) }
    | ']'      { LazyStream.Cons (ArrayEnd, lazy (read lexbuf)) }
    | ':'      { LazyStream.Cons (Colon, lazy (read lexbuf)) }
    | ','      { LazyStream.Cons (Comma, lazy (read lexbuf)) }
    | eof      { LazyStream.Nil }
and read_string = parse
    | '"'      { Buffer.contents string_buf }
    | [^ '"' '\\']+ {
        let lexeme = lexeme lexbuf in
        Buffer.add_string string_buf lexeme;
        read_string lexbuf
    }
