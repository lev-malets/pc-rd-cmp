open Base
open Compilerlibs406
open Parsetree

module Make (Base : Sigs.BASIC_BASE) : Sigs.BASIC = struct
  include Base

  module type CORE = Sigs.CORE with module Comb = Comb
  module type MODEXPR = Sigs.MODEXPR with module Comb = Comb
  module type EXPRESSION = Sigs.EXPRESSION with module Comb = Comb
  module type PATTERN = Sigs.PATTERN with module Comb = Comb
  module type TYPE = Sigs.TYPE with module Comb = Comb
  module type MODTYPE = Sigs.MODTYPE with module Comb = Comb

  open Pc
  open Comb

  let ng_not_empty =
    run
    & mapping (fun p1 p2 ->
          let open Simple in
          let open Lexing in
          match p1.pos_cnum = p2.pos_cnum with
          | true -> fail
          | false -> return ())
      <*> pos_end <*>. pos

  let ng_no_new_line =
    run
    & mapping (fun p1 p2 ->
          let open Simple in
          let open Lexing in
          match p1.pos_lnum = p2.pos_lnum with
          | true -> return ()
          | false -> fail)
      <*> pos_end <*>. pos

  let ng_new_line =
    run
    & mapping (fun p1 p2 ->
          let open Simple in
          let open Lexing in
          match p1.pos_lnum = p2.pos_lnum with
          | true -> fail
          | false -> return ())
      <*> pos_end <*>. pos

  let with_del p =
    mapping (fun p1 f p2 -> f (loc_mk p1 p2)) <*> pos <*> p <*> del_pos

  let sep = ng >> comma >> ng

  let u_longident =
    named "u_longident"
    & fold_left_0_n
        ~f:(fun lid str -> Longident.Ldot (lid, str))
        (u_ident >>| fun str -> Longident.Lident str)
        (ng >> dot >> ng >> u_ident)

  let l_longident =
    choice ~name:"l_longident"
      [ mapping (fun a b -> Longident.Ldot (a, b))
        <*> u_longident <<. dot <*>. l_ident
      ; (l_ident >>| fun s -> Longident.Lident s) ]

  let longident =
    choice ~name:"longident"
      [ mapping (fun a b ->
            match b with
            | None -> a
            | Some b -> Longident.Ldot (a, b))
        <*> u_longident
        <*> opt (ng >> dot >> ng >> l_ident)
      ; (l_ident >>| fun s -> Longident.Lident s) ]

  let attribute_id =
    fold_left_0_n
      ~f:(fun buf x -> Buffer.add_char buf '.'; Buffer.add_string buf x; buf)
      (ident
      >>| fun x ->
      let buf = Buffer.create (String.length x) in
      Buffer.add_string buf x; buf)
      (ng >> dot >> ng >> ident)
    >>| Buffer.contents

  let variant_tag =
    hash >> ng
    >> choice ~name:"variant_tag:tag"
         [ident; string_raw; (integer >>| fun (x, _) -> x)]

  let parens p = l_paren >> ng >> p <<. r_paren
  let brackets p = l_bracket >> ng >> p <<. r_bracket
  let braces p = l_brace >> ng >> p <<. r_brace
  let chevrons p = lt >> ng >> p <<. gt

  let na_hlp (f : ?loc:Warnings.loc -> 'a -> 'b) =
    return @@ fun a loc -> f ~loc a

  let hlp (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) =
    return @@ fun a loc -> f ~loc a

  let hlp2 (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) =
    return @@ fun a b loc -> f ~loc a b

  let hlp3 (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd)
      =
    return @@ fun a b c loc -> f ~loc a b c

  let hlp4
      (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e)
      =
    return @@ fun a b c d loc -> f ~loc a b c d

  let hlp_a (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b) =
    return @@ fun attrs a loc -> f ~loc ~attrs a

  let hlp2_a (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c) =
    return @@ fun attrs a b loc -> f ~attrs ~loc a b

  let hlp3_a
      (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd) =
    return @@ fun attrs a b c loc -> f ~attrs ~loc a b c

  let hlp4_a
      (f : ?loc:Warnings.loc -> ?attrs:attributes -> 'a -> 'b -> 'c -> 'd -> 'e)
      =
    return @@ fun attrs a b c d loc -> f ~attrs ~loc a b c d

  module Sugar = struct
    open Basic

    let hlp ~get ~set ~update sym attr p =
      let attr = Basic.Hc.attr attr in
      mapping (fun loc_start f x ->
          if Option.is_none f then x
          else
            let e = get x in
            set x (update ~loc_start ~attr e))
      <*> pos <*> opt sym <*>. p

    let exp_hlp = hlp ~update:exp_prepend_attr
    let pat_hlp = hlp ~update:pat_prepend_attr
    let get x = x
    let set _ x = x
    let async = exp_hlp ~get ~set async "res.async"
    let await = exp_hlp ~get ~set await "res.await"
    let optional = exp_hlp ~get ~set question "ns.optional"

    let optional1 p =
      exp_hlp
        ~get:(fun (_, x) -> x)
        ~set:(fun (lid, _) x -> (lid, x))
        question "ns.optional" p

    let optional_pat = pat_hlp ~get ~set question "ns.optional"

    let optional1_pat p =
      pat_hlp
        ~get:(fun (_, x) -> x)
        ~set:(fun (lid, _) x -> (lid, x))
        question "ns.optional" p
  end
end
