
type string_info = { value : string; raw : string }
type number_info = { value : string; suffix : char option }
type e_cc_info = { code : int; raw : string }

type t =
    | LIdent (* of string *)
    | UIdent (* of string *)
    | Character (* of char *)
    | String (* of string_info *)
    | MultilineString (* of string *)
    | Integer (* of number_info *)
    | Float (* of number_info *)
    | Comment (* of string *)
    | MultilineComment (* of string *)
    | TypeVar (* of string *)
    | StringIdent (* of string *)

    | TemplatePart (* of string *)
    | TemplateTail (* of string *)

    | ErrorCharacterCode (* e_cc_info *)

    | And
    | As
    | Assert
    | Catch
    | Constraint
    | Downto
    | Else
    | Exception
    | Export
    | External
    | False
    | For
    | From
    | If
    | In
    | Include
    | Import
    | Json
    | Lazy
    | Let
    | Module
    | Mutable
    | Nonrec
    | Of
    | Open
    | Private
    | Rec
    | Sig
    | Switch
    | To
    | True
    | Try
    | Type
    | Unpack
    | When
    | While
    | With

    | Underscore

    | Ampersand
    | AmpersandAmptersand
    | Arrow
    | Asterisk
    | AsteriskAsterisk
    | AsteriskDot
    | At
    | AtAt
    | Bang
    | BangEq
    | BangEqEq
    | Colon
    | ColonEq
    | ColonGt
    | Comma
    | Dot
    | DotDot
    | Ellipsis
    | Eq
    | EqEq
    | EqEqEq
    | Gt
    | Hash
    | HashEq
    | LBrace
    | LBracket
    | LParen
    | List
    | Lt
    | LtEq
    | Minus
    | MinusDot
    | MinusGt
    | Percent
    | PercentPercent
    | Pipe
    | PipeGt
    | PipePipe
    | Plus
    | PlusDot
    | PlusEq
    | PlusPlus
    | Question
    | RBrace
    | RBracket
    | RParen
    | Semicolon
    | Slash
    | SlashDot
    | Tilda
    [@@deriving eq, show]
