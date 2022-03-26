
type t =
    | LIdent of string
    | UIdent of string
    | Character of char
    | CharacterCode of int
    | String of { value : string; raw : string }
    | MultilineString of string
    | Integer of { value : string; suffix : char }
    | Float of { value : string; suffix : char }
    | Comment of string
    | MultilineComment of string

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
    | GtEq
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
    | Slash
    | SlashDot
    | Tilda
    [@deriving eq]
