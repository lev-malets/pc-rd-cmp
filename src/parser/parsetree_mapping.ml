open Parsetree
open Base

type t = {
  signature : signature -> signature;
  signature_item : signature_item -> signature_item;
  structure : structure -> structure;
  structure_item : structure_item -> structure_item;
  pattern : pattern -> pattern;
  expression : expression -> expression;
  core_type : core_type -> core_type;
  module_expr : module_expr -> module_expr;
  module_type : module_type -> module_type;
  class_expr : class_expr -> class_expr;
  class_field : class_field -> class_field;
  class_type : class_type -> class_type;
  class_type_field : class_type_field -> class_type_field;
  attribute : attribute -> attribute;
  extension : extension -> extension;
  class_structure : class_structure -> class_structure;
  class_description : class_description -> class_description;
  class_type_declaration : class_type_declaration -> class_type_declaration;
  extension_constructor : extension_constructor -> extension_constructor;
  module_binding : module_binding -> module_binding;
  module_type_declaration : module_type_declaration -> module_type_declaration;
  module_declaration : module_declaration -> module_declaration;
  open_description : open_description -> open_description;
  type_extension : type_extension -> type_extension;
  type_declaration : type_declaration -> type_declaration;
  value_description : value_description -> value_description;
  constructor_declaration : constructor_declaration -> constructor_declaration;
  value_binding : value_binding -> value_binding;
  label_declaration : label_declaration -> label_declaration;
  include_description : include_description -> include_description;
  include_declaration : include_declaration -> include_declaration;
}

let rec signature f x =
  let x = f.signature x in
  list signature_item f x

and structure f x =
  let x = f.structure x in
  list structure_item f x

and signature_item f x =
  f.signature_item
    {
      x with
      psig_desc =
        (match x.psig_desc with
        | Psig_attribute a -> Psig_attribute (attribute f a)
        | Psig_class l -> Psig_class (list class_description f l)
        | Psig_class_type l -> Psig_class_type (list class_type_declaration f l)
        | Psig_exception e -> Psig_exception (extension_constructor f e)
        | Psig_extension (e, a) -> Psig_extension (extension f e, attributes f a)
        | Psig_include x -> Psig_include (include_description f x)
        | Psig_modtype x -> Psig_modtype (module_type_declaration f x)
        | Psig_module x -> Psig_module (module_declaration f x)
        | Psig_open x -> Psig_open (open_description f x)
        | Psig_recmodule x -> Psig_recmodule (list module_declaration f x)
        | Psig_type (fl, x) -> Psig_type (fl, list type_declaration f x)
        | Psig_typext x -> Psig_typext (type_extension f x)
        | Psig_value x -> Psig_value (value_description f x));
    }

and structure_item f x =
  f.structure_item
    {
      x with
      pstr_desc =
        (match x.pstr_desc with
        | Pstr_eval (e, a) -> Pstr_eval (expression f e, attributes f a)
        | Pstr_value (fl, l) -> Pstr_value (fl, list value_binding f l)
        | Pstr_primitive x -> Pstr_primitive (value_description f x)
        | Pstr_type (fl, l) -> Pstr_type (fl, list type_declaration f l)
        | Pstr_typext x -> Pstr_typext (type_extension f x)
        | Pstr_exception x -> Pstr_exception (extension_constructor f x)
        | Pstr_module x -> Pstr_module (module_binding f x)
        | Pstr_recmodule x -> Pstr_recmodule (list module_binding f x)
        | Pstr_modtype x -> Pstr_modtype (module_type_declaration f x)
        | Pstr_open x -> Pstr_open (open_description f x)
        | Pstr_class x -> Pstr_class (list class_declaration f x)
        | Pstr_class_type x -> Pstr_class_type (list class_type_declaration f x)
        | Pstr_include x -> Pstr_include (include_declaration f x)
        | Pstr_attribute x -> Pstr_attribute (attribute f x)
        | Pstr_extension (e, a) -> Pstr_extension (extension f e, attributes f a));
    }

and class_description f x = f.class_description @@ class_infos class_type f x

and class_type_declaration f x = f.class_type_declaration @@ class_infos class_type f x

and attribute f x = f.attribute @@ id__payload f x

and extension f x = f.extension @@ id__payload f x

and id__payload f (s, p) =
  ( s,
    match p with
    | PPat (p, e) -> PPat (pattern f p, opt expression f e)
    | PSig s -> PSig (signature f s)
    | PStr s -> PStr (structure f s)
    | PTyp t -> PTyp (core_type f t) )

and attributes f l = list attribute f l

and module_binding f x =
  f.module_binding { x with pmb_expr = module_expr f x.pmb_expr; pmb_attributes = attributes f x.pmb_attributes }

and module_type_declaration f x =
  f.module_type_declaration
    { x with pmtd_type = opt module_type f x.pmtd_type; pmtd_attributes = attributes f x.pmtd_attributes }

and module_declaration f x =
  f.module_declaration { x with pmd_type = module_type f x.pmd_type; pmd_attributes = attributes f x.pmd_attributes }

and open_description f x = f.open_description { x with popen_attributes = attributes f x.popen_attributes }

and type_extension f x =
  f.type_extension
    {
      x with
      ptyext_params = list (t2 core_type id) f x.ptyext_params;
      ptyext_constructors = list extension_constructor f x.ptyext_constructors;
      ptyext_private = x.ptyext_private;
      ptyext_attributes = attributes f x.ptyext_attributes;
    }

and value_description f x =
  f.value_description { x with pval_type = core_type f x.pval_type; pval_attributes = attributes f x.pval_attributes }

and class_declaration f x = class_infos class_expr f x

and class_infos : 'a. (t -> 'a -> 'a) -> t -> 'a class_infos -> 'a class_infos =
 fun e f x ->
  {
    x with
    pci_params = list (t2 core_type id) f x.pci_params;
    pci_expr = e f x.pci_expr;
    pci_attributes = attributes f x.pci_attributes;
  }

and pattern f x =
  f.pattern
    {
      x with
      ppat_attributes = attributes f x.ppat_attributes;
      ppat_desc =
        (match x.ppat_desc with
        | Ppat_any -> Ppat_any
        | Ppat_alias (p, n) -> Ppat_alias (pattern f p, n)
        | Ppat_array l -> Ppat_array (list pattern f l)
        | Ppat_constant c -> Ppat_constant c
        | Ppat_constraint (p, t) -> Ppat_constraint (pattern f p, core_type f t)
        | Ppat_construct (lid, p) -> Ppat_construct (lid, opt pattern f p)
        | Ppat_exception p -> Ppat_exception (pattern f p)
        | Ppat_extension e -> Ppat_extension (extension f e)
        | Ppat_interval (c1, c2) -> Ppat_interval (c1, c2)
        | Ppat_lazy p -> Ppat_lazy (pattern f p)
        | Ppat_open (lid, p) -> Ppat_open (lid, pattern f p)
        | Ppat_or (p1, p2) -> Ppat_or (pattern f p1, pattern f p2)
        | Ppat_record (l, fl) -> Ppat_record (list (t2 id pattern) f l, fl)
        | Ppat_tuple l -> Ppat_tuple (list pattern f l)
        | Ppat_type lid -> Ppat_type lid
        | Ppat_unpack s -> Ppat_unpack s
        | Ppat_var s -> Ppat_var s
        | Ppat_variant (s, p) -> Ppat_variant (s, opt pattern f p));
    }

and expression f x =
  f.expression
    {
      x with
      pexp_attributes = attributes f x.pexp_attributes;
      pexp_desc =
        (match x.pexp_desc with
        | Pexp_apply (e, l) -> Pexp_apply (expression f e, list (t2 id expression) f l)
        | Pexp_array l -> Pexp_array (list expression f l)
        | Pexp_assert e -> Pexp_assert (expression f e)
        | Pexp_coerce (e, t1, t2) -> Pexp_coerce (expression f e, opt core_type f t1, core_type f t2)
        | Pexp_constant x -> Pexp_constant x
        | Pexp_constraint (e, t) -> Pexp_constraint (expression f e, core_type f t)
        | Pexp_construct (lid, e) -> Pexp_construct (lid, opt expression f e)
        | Pexp_extension e -> Pexp_extension (extension f e)
        | Pexp_field (e, lid) -> Pexp_field (expression f e, lid)
        | Pexp_for (p, e1, e2, fl, e3) -> Pexp_for (pattern f p, expression f e1, expression f e2, fl, expression f e3)
        | Pexp_fun (l, e1, p, e2) -> Pexp_fun (l, opt expression f e1, pattern f p, expression f e2)
        | Pexp_function l -> Pexp_function (list case f l)
        | Pexp_ident lid -> Pexp_ident lid
        | Pexp_ifthenelse (e1, e2, e3) -> Pexp_ifthenelse (expression f e1, expression f e2, opt expression f e3)
        | Pexp_lazy e -> Pexp_lazy (expression f e)
        | Pexp_let (fl, vb, e) -> Pexp_let (fl, list value_binding f vb, expression f e)
        | Pexp_letexception (ec, e) -> Pexp_letexception (extension_constructor f ec, expression f e)
        | Pexp_letmodule (s, m, e) -> Pexp_letmodule (s, module_expr f m, expression f e)
        | Pexp_match (e, l) -> Pexp_match (expression f e, list case f l)
        | Pexp_new lid -> Pexp_new lid
        | Pexp_newtype (s, e) -> Pexp_newtype (s, expression f e)
        | Pexp_object c -> Pexp_object (class_stucture f c)
        | Pexp_open (fl, lid, e) -> Pexp_open (fl, lid, expression f e)
        | Pexp_override l -> Pexp_override (list (t2 id expression) f l)
        | Pexp_pack m -> Pexp_pack (module_expr f m)
        | Pexp_poly (e, l) -> Pexp_poly (expression f e, opt core_type f l)
        | Pexp_record (l, e) -> Pexp_record (list (t2 id expression) f l, opt expression f e)
        | Pexp_send (e, s) -> Pexp_send (expression f e, s)
        | Pexp_sequence (e1, e2) -> Pexp_sequence (expression f e1, expression f e2)
        | Pexp_setfield (e1, lid, e2) -> Pexp_setfield (expression f e1, lid, expression f e2)
        | Pexp_setinstvar (s, e) -> Pexp_setinstvar (s, expression f e)
        | Pexp_try (e, l) -> Pexp_try (expression f e, list case f l)
        | Pexp_tuple e -> Pexp_tuple (list expression f e)
        | Pexp_unreachable -> Pexp_unreachable
        | Pexp_variant (s, e) -> Pexp_variant (s, opt expression f e)
        | Pexp_while (e1, e2) -> Pexp_while (expression f e1, expression f e2));
    }

and core_type f x =
  f.core_type
    {
      x with
      ptyp_attributes = attributes f x.ptyp_attributes;
      ptyp_desc =
        (match x.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_alias (c, s) -> Ptyp_alias (core_type f c, s)
        | Ptyp_arrow (l, t1, t2) -> Ptyp_arrow (l, core_type f t1, core_type f t2)
        | Ptyp_class (lid, l) -> Ptyp_class (lid, list core_type f l)
        | Ptyp_constr (lid, l) -> Ptyp_constr (lid, list core_type f l)
        | Ptyp_extension x -> Ptyp_extension (extension f x)
        | Ptyp_object (l, fl) -> Ptyp_object (list object_field f l, fl)
        | Ptyp_package x -> Ptyp_package (package_type f x)
        | Ptyp_poly (l, t) -> Ptyp_poly (l, core_type f t)
        | Ptyp_tuple l -> Ptyp_tuple (list core_type f l)
        | Ptyp_variant (l, fl, o) -> Ptyp_variant (list row_field f l, fl, o)
        | Ptyp_var s -> Ptyp_var s);
    }

and object_field f = function
  | Otag (l, a, t) -> Otag (l, attributes f a, core_type f t)
  | Oinherit t -> Oinherit (core_type f t)

and package_type f x = t2 id (list @@ t2 id core_type) f x

and row_field f = function
  | Rtag (lid, a, fl, l) -> Rtag (lid, attributes f a, fl, list core_type f l)
  | Rinherit x -> Rinherit (core_type f x)

and class_stucture f x =
  f.class_structure { pcstr_self = pattern f x.pcstr_self; pcstr_fields = list class_field f x.pcstr_fields }

and module_expr f x =
  f.module_expr
    {
      x with
      pmod_attributes = attributes f x.pmod_attributes;
      pmod_desc =
        (match x.pmod_desc with
        | Pmod_apply (m1, m2) -> Pmod_apply (module_expr f m1, module_expr f m2)
        | Pmod_constraint (m, t) -> Pmod_constraint (module_expr f m, module_type f t)
        | Pmod_extension x -> Pmod_extension (extension f x)
        | Pmod_functor (s, t, m) -> Pmod_functor (s, opt module_type f t, module_expr f m)
        | Pmod_ident x -> Pmod_ident x
        | Pmod_structure x -> Pmod_structure (structure f x)
        | Pmod_unpack x -> Pmod_unpack (expression f x));
    }

and module_type f x =
  f.module_type
    {
      x with
      pmty_attributes = attributes f x.pmty_attributes;
      pmty_desc =
        (match x.pmty_desc with
        | Pmty_alias x -> Pmty_alias x
        | Pmty_extension x -> Pmty_extension (extension f x)
        | Pmty_functor (s, o, x) -> Pmty_functor (s, opt module_type f o, module_type f x)
        | Pmty_ident x -> Pmty_ident x
        | Pmty_signature x -> Pmty_signature (signature f x)
        | Pmty_typeof x -> Pmty_typeof (module_expr f x)
        | Pmty_with (x, l) -> Pmty_with (module_type f x, list with_constraint f l));
    }

and with_constraint f = function
  | Pwith_type (lid, x) -> Pwith_type (lid, type_declaration f x)
  | Pwith_module (l1, l2) -> Pwith_module (l1, l2)
  | Pwith_typesubst (lid, x) -> Pwith_typesubst (lid, type_declaration f x)
  | Pwith_modsubst (l1, l2) -> Pwith_modsubst (l1, l2)

and type_declaration f x =
  f.type_declaration
    {
      x with
      ptype_params = list (t2 core_type id) f x.ptype_params;
      ptype_cstrs = list (t3 core_type core_type id) f x.ptype_cstrs;
      ptype_manifest = opt core_type f x.ptype_manifest;
      ptype_attributes = attributes f x.ptype_attributes;
      ptype_kind =
        (match x.ptype_kind with
        | Ptype_variant l -> Ptype_variant (list constructor_declaration f l)
        | Ptype_record l -> Ptype_record (list label_declaration f l)
        | Ptype_abstract -> Ptype_abstract
        | Ptype_open -> Ptype_open);
    }

and constructor_declaration f x =
  f.constructor_declaration
    {
      x with
      pcd_args = constructor_arguments f x.pcd_args;
      pcd_res = opt core_type f x.pcd_res;
      pcd_attributes = attributes f x.pcd_attributes;
    }

and class_structure f x = { pcstr_self = pattern f x.pcstr_self; pcstr_fields = list class_field f x.pcstr_fields }

and class_signature f x =
  { pcsig_self = core_type f x.pcsig_self; pcsig_fields = list class_type_field f x.pcsig_fields }

and class_type f x =
  f.class_type
    {
      x with
      pcty_attributes = attributes f x.pcty_attributes;
      pcty_desc =
        (match x.pcty_desc with
        | Pcty_arrow (l, t1, t2) -> Pcty_arrow (l, core_type f t1, class_type f t2)
        | Pcty_constr (lid, l) -> Pcty_constr (lid, list core_type f l)
        | Pcty_extension x -> Pcty_extension (extension f x)
        | Pcty_open (fl, lid, t) -> Pcty_open (fl, lid, class_type f t)
        | Pcty_signature s -> Pcty_signature (class_signature f s));
    }

and class_type_field f x =
  f.class_type_field
    {
      x with
      pctf_attributes = attributes f x.pctf_attributes;
      pctf_desc =
        (match x.pctf_desc with
        | Pctf_attribute x -> Pctf_attribute (attribute f x)
        | Pctf_constraint (t1, t2) -> Pctf_constraint (core_type f t1, core_type f t2)
        | Pctf_extension x -> Pctf_extension (extension f x)
        | Pctf_inherit x -> Pctf_inherit (class_type f x)
        | Pctf_method (s, f1, f2, t) -> Pctf_method (s, f1, f2, core_type f t)
        | Pctf_val (s, f1, f2, t) -> Pctf_val (s, f1, f2, core_type f t));
    }

and class_expr f x =
  f.class_expr
    {
      x with
      pcl_attributes = attributes f x.pcl_attributes;
      pcl_desc =
        (match x.pcl_desc with
        | Pcl_apply (x, l) -> Pcl_apply (class_expr f x, list (t2 id expression) f l)
        | Pcl_constr (lid, l) -> Pcl_constr (lid, list core_type f l)
        | Pcl_constraint (x, t) -> Pcl_constraint (class_expr f x, class_type f t)
        | Pcl_extension x -> Pcl_extension (extension f x)
        | Pcl_fun (l, e, p, x) -> Pcl_fun (l, opt expression f e, pattern f p, class_expr f x)
        | Pcl_let (fl, l, x) -> Pcl_let (fl, list value_binding f l, class_expr f x)
        | Pcl_open (fl, lid, x) -> Pcl_open (fl, lid, class_expr f x)
        | Pcl_structure x -> Pcl_structure (class_structure f x));
    }

and class_field f x =
  f.class_field
    {
      x with
      pcf_attributes = attributes f x.pcf_attributes;
      pcf_desc =
        (match x.pcf_desc with
        | Pcf_attribute x -> Pcf_attribute (attribute f x)
        | Pcf_constraint (t1, t2) -> Pcf_constraint (core_type f t1, core_type f t2)
        | Pcf_extension x -> Pcf_extension (extension f x)
        | Pcf_inherit (fl, c, s) -> Pcf_inherit (fl, class_expr f c, s)
        | Pcf_initializer x -> Pcf_initializer (expression f x)
        | Pcf_method (s, fl, x) -> Pcf_method (s, fl, class_field_kind f x)
        | Pcf_val (s, fl, x) -> Pcf_val (s, fl, class_field_kind f x));
    }

and class_field_kind f = function
  | Cfk_virtual t -> Cfk_virtual (core_type f t)
  | Cfk_concrete (fl, x) -> Cfk_concrete (fl, expression f x)

and value_binding f x =
  f.value_binding
    {
      x with
      pvb_pat = pattern f x.pvb_pat;
      pvb_expr = expression f x.pvb_expr;
      pvb_attributes = attributes f x.pvb_attributes;
    }

and extension_constructor f x =
  f.extension_constructor
    { x with pext_kind = extension_constructor_kind f x.pext_kind; pext_attributes = attributes f x.pext_attributes }

and extension_constructor_kind f = function
  | Pext_decl (ca, ct) -> Pext_decl (constructor_arguments f ca, opt core_type f ct)
  | Pext_rebind lid -> Pext_rebind lid

and constructor_arguments f = function
  | Pcstr_tuple l -> Pcstr_tuple (list core_type f l)
  | Pcstr_record l -> Pcstr_record (list label_declaration f l)

and label_declaration f x =
  f.label_declaration { x with pld_type = core_type f x.pld_type; pld_attributes = attributes f x.pld_attributes }

and case f x = { pc_lhs = pattern f x.pc_lhs; pc_guard = opt expression f x.pc_guard; pc_rhs = expression f x.pc_rhs }

and include_description f x = f.include_description @@ include_infos module_type f x

and include_declaration f x = f.include_declaration @@ include_infos module_expr f x

and include_infos : 'a. (t -> 'a -> 'a) -> t -> 'a include_infos -> 'a include_infos =
 fun e f x -> { x with pincl_mod = e f x.pincl_mod; pincl_attributes = attributes f x.pincl_attributes }

and opt : 'a. (t -> 'a -> 'a) -> t -> 'a option -> 'a option = fun f m x -> Option.map ~f:(f m) x

and list : 'a. (t -> 'a -> 'a) -> t -> 'a list -> 'a list = fun f m x -> List.map ~f:(f m) x

and t2 : 'a 'b. (t -> 'a -> 'a) -> (t -> 'b -> 'b) -> t -> 'a * 'b -> 'a * 'b = fun f1 f2 m (a, b) -> (f1 m a, f2 m b)

and t3 : 'a 'b 'c. (t -> 'a -> 'a) -> (t -> 'b -> 'b) -> (t -> 'c -> 'c) -> t -> 'a * 'b * 'c -> 'a * 'b * 'c =
 fun f1 f2 f3 m (a, b, c) -> (f1 m a, f2 m b, f3 m c)

and id : 'a. t -> 'a -> 'a = fun _ x -> x

let default =
  let id : 'a. 'a -> 'a = fun x -> x in

  {
    signature = id;
    signature_item = id;
    structure = id;
    structure_item = id;
    pattern = id;
    expression = id;
    core_type = id;
    module_expr = id;
    module_type = id;
    class_expr = id;
    class_field = id;
    class_type = id;
    class_type_field = id;
    attribute = id;
    extension = id;
    class_structure = id;
    class_description = id;
    class_type_declaration = id;
    extension_constructor = id;
    module_binding = id;
    module_type_declaration = id;
    module_declaration = id;
    open_description = id;
    type_extension = id;
    type_declaration = id;
    value_description = id;
    constructor_declaration = id;
    value_binding = id;
    label_declaration = id;
    include_description = id;
    include_declaration = id;
  }
