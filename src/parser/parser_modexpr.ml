open Base
open Sigs
open Parsetree
open Ast_helper
open Basic

module Make
    (Basic : BASIC)
    (Core : CORE with module Comb = Basic.Comb)
    (Type : TYPE with module Comb = Basic.Comb)
    (Expression : EXPRESSION with module Comb = Basic.Comb)
    (Modtype : MODTYPE with module Comb = Basic.Comb) =
struct
  open Basic
  open Core
  open Type
  open Expression
  open Modtype
  open Pc
  open Comb
  module Comb = Comb

  module type THIS = MODEXPR with module Comb = Basic.Comb

  let x =
    fix_gen @@ fun getter : (module THIS) ->
    (module struct
      module Comb = Comb

      let modexpr = getter.get @@ fun (module M : THIS) -> M.modexpr

      let modexpr_constrainted =
        named "modexpr:constrainted"
        @@ fold_left_cont_0_1 modexpr
             (mapping (fun typ prev ->
                  Mod.constraint_
                    ~loc:(loc_comb prev.pmod_loc typ.pmty_loc)
                    prev typ)
             - ng - colon - ng + modtype)

      let atom =
        let unpack_constr =
          fold_left_cont_0_1 expression
            (mapping (fun typ prev ->
                 Exp.constraint_
                   ~loc:(loc_comb prev.pexp_loc typ.ptyp_loc)
                   prev typ)
            - ng - colon - ng + core_type_package)
        in

        choice ~name:"modexpr:atom"
          [
            with_loc
            & hlp Mod.unpack - unpack - ng - l_paren - ng + unpack_constr - ng
              - r_paren;
            with_loc & (hlp Mod.ident + loc u_longident);
            with_loc & parens
            & (mapping mod_loc + mod_attrs modexpr_constrainted);
            with_loc & (hlp Mod.extension + extension);
          ]

      let apply =
        named "modexpr:apply"
          (let genarg =
             with_loc
             & mapping (fun loc -> Mod.structure ~loc [])
               - l_paren - ng - r_paren
           in

           let arg = choice [ genarg; modexpr_constrainted ] in

           let args =
             fold_left_cont_0_n
               (mapping (fun arg prev ->
                    Mod.apply
                      ~loc:(loc_comb prev.pmod_loc arg.pmod_loc)
                      prev arg)
               + arg)
               (mapping (fun arg cont prev ->
                    let prev = cont prev in
                    Mod.apply
                      ~loc:(loc_comb prev.pmod_loc arg.pmod_loc)
                      prev arg)
               - ng - comma - ng + arg)
           in

           let args =
             choice ~name:"modexpr:apply:args"
               [
                 l_paren >> args << opt sep << ng << r_paren;
                 mapping (fun loc_end prev ->
                     Mod.apply
                       ~loc:{ prev.pmod_loc with loc_end }
                       prev (Mod.structure []))
                 - l_paren - ng - r_paren + pos;
               ]
           in

           mod_attrs & fold_left_cont_0_n atom args)

      let functor_ =
        let tail =
          choice ~name:"modexpr:functor:tail"
            [
              mapping (fun mt me -> Mod.constraint_ ~loc:me.pmod_loc me mt)
              - colon - ng + modtype - ng - arrow - ng + modexpr;
              arrow >> ng >> modexpr;
            ]
        in

        let arg_loop =
          fix @@ fun arg_loop ->
          let tail =
            choice ~name:"modexpr:functor:arg_loop:tail"
              [
                opt comma >> ng >> r_paren >> ng >> tail;
                comma >> ng >> arg_loop;
              ]
          in

          choice ~name:"modexpr:functor:arg_loop"
            [
              mod_attrs & with_loc
              & hlp3 Mod.functor_ + loc u_ident
                + opt (ng >> colon >> ng >> modtype)
                - ng + tail;
              mod_attrs & with_loc
              & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
                + loc (l_paren >> ng >> r_paren >>$ "*")
                - ng + tail;
            ]
        in

        choice ~name:"modexpr:functor"
          [
            mod_attrs & with_loc
            & mapping (fun a b loc -> Mod.functor_ ~loc a None b)
              + loc (l_paren >> ng >> r_paren >>$ "*")
              - ng + tail;
            mod_attrs & l_paren >> ng >> arg_loop;
          ]

      let structure =
        named "modexpr:structure"
          (with_loc
          & (hlp Mod.structure - l_brace - ng + structure - ng - r_brace))

      let modexpr = choice ~name:"modexpr" [ functor_; apply; structure ]
    end)

  let modexpr =
    let (module M) = x in
    M.modexpr

  let modexpr_constrainted =
    let (module M) = x in
    M.modexpr_constrainted
end
