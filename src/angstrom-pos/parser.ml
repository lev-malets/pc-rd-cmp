type _ typ =
    | Parser : _ typ
    | Return : 'a -> 'a typ
    | Value :
        { v: 'a
        ; p: _ Angstrom.t
        } -> 'a typ
    | Lift :
        { f: 'a -> 'b
        ; a: 'a Angstrom.t
        } -> 'b typ
    | Lift2 :
        { f: 'a -> 'b -> 'c
        ; a: 'a Angstrom.t
        ; b: 'b Angstrom.t
        } -> 'c typ
    | Lift3 :
        { f: 'a -> 'b -> 'c -> 'd
        ; a: 'a Angstrom.t
        ; b: 'b Angstrom.t
        ; c: 'c Angstrom.t
        } -> 'd typ

type info =
    | Unknown
    | Empty
    | Consume of
        { empty: bool
        ; first: Charset.t
        }

type 'a t =
    { p: 'a Angstrom.t
    ; info: info
    ; typ: 'a typ
    ; id : int
    }
