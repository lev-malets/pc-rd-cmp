
open Asttypes
open Parsetree
open Base

type t_signature_item_desc = {
    attribute  : attribute -> signature_item_desc;
    class_     : class_description list -> signature_item_desc;
    class_type : class_type_declaration list -> signature_item_desc;
    exception_ : extension_constructor -> signature_item_desc;
    extension  : extension -> attributes -> signature_item_desc;
    include_   : include_description -> signature_item_desc;
    modtype    : module_type_declaration -> signature_item_desc;
    module_    : module_declaration -> signature_item_desc;
    open_      : open_description -> signature_item_desc;
    recmodule  : module_declaration list -> signature_item_desc;
    type_      : rec_flag -> type_declaration list -> signature_item_desc;
    typext     : type_extension -> signature_item_desc;
    value      : value_description -> signature_item_desc;
}

type t_structure_item_desc = {
    eval       : expression -> attributes -> structure_item_desc;
    value      : rec_flag ->  value_binding list -> structure_item_desc;
    primitive  : value_description -> structure_item_desc;
    type_      : rec_flag -> type_declaration list -> structure_item_desc;
    typext     : type_extension -> structure_item_desc;
    exception_ : extension_constructor -> structure_item_desc;
    module_    : module_binding -> structure_item_desc;
    recmodule  : module_binding list -> structure_item_desc;
    modtype    : module_type_declaration -> structure_item_desc;
    open_      : open_description -> structure_item_desc;
    class_     : class_declaration list -> structure_item_desc;
    class_type : class_type_declaration list -> structure_item_desc;
    include_   : include_declaration -> structure_item_desc;
    attribute  : attribute -> structure_item_desc;
    extension  : extension -> attributes -> structure_item_desc;
}

type t_pattern_desc = {
    any         : unit -> pattern_desc;
    var         : string loc -> pattern_desc;
    alias       : pattern -> string loc -> pattern_desc;
    constant    : constant -> pattern_desc;
    interval    : constant -> constant -> pattern_desc;
    tuple       : pattern list -> pattern_desc;
    construct   : Longident.t loc -> pattern option -> pattern_desc;
    variant     : label -> pattern option -> pattern_desc;
    record      : (Longident.t loc * pattern) list -> closed_flag -> pattern_desc;
    array       : pattern list -> pattern_desc;
    or_         : pattern -> pattern -> pattern_desc;
    constraint_ : pattern -> core_type -> pattern_desc;
    type_       : Longident.t loc -> pattern_desc;
    lazy_       : pattern -> pattern_desc;
    unpack      : string loc -> pattern_desc;
    exception_  : pattern -> pattern_desc;
    extension   : extension -> pattern_desc;
    open_       : Longident.t loc -> pattern -> pattern_desc;
}

type t_expression_desc = {
    apply        : expression -> (arg_label * expression) list -> expression_desc;
    array        : expression list -> expression_desc;
    assert_      : expression -> expression_desc;
    coerce       : expression -> core_type option -> core_type -> expression_desc;
    constant     : constant -> expression_desc;
    constraint_  : expression -> core_type -> expression_desc;
    construct    : Longident.t loc -> expression option -> expression_desc;
    extension    : extension -> expression_desc;
    field        : expression -> Longident.t loc -> expression_desc;
    for_         : pattern -> expression -> expression -> direction_flag -> expression -> expression_desc;
    fun_         : arg_label -> expression option -> pattern -> expression -> expression_desc;
    function_    : case list -> expression_desc;
    ident        : Longident.t loc -> expression_desc;
    ifthenelse   : expression -> expression -> expression option -> expression_desc;
    lazy_        : expression -> expression_desc;
    let_         : rec_flag -> value_binding list -> expression -> expression_desc;
    letexception : extension_constructor -> expression -> expression_desc;
    letmodule    : label loc -> module_expr -> expression -> expression_desc;
    match_       : expression -> case list -> expression_desc;
    new_         : Longident.t loc -> expression_desc;
    newtype      : label loc -> expression -> expression_desc;
    object_      : class_structure -> expression_desc;
    open_        : override_flag -> Longident.t loc -> expression -> expression_desc;
    override     : (label loc * expression) list -> expression_desc;
    pack         : module_expr -> expression_desc;
    poly         : expression -> core_type option -> expression_desc;
    record       : (Longident.t loc * expression) list -> expression option -> expression_desc;
    send         : expression -> label loc -> expression_desc;
    sequence     : expression -> expression -> expression_desc;
    setfield     : expression -> Longident.t loc -> expression -> expression_desc;
    setinstvar   : label loc -> expression -> expression_desc;
    try_         : expression -> case list -> expression_desc;
    tuple        : expression list -> expression_desc;
    unreachable  : unit -> expression_desc;
    variant      : label -> expression option -> expression_desc;
    while_       : expression -> expression -> expression_desc;
}

type t_core_type_desc = {
    any       : unit -> core_type_desc;
    alias     : core_type -> label -> core_type_desc;
    arrow     : arg_label -> core_type -> core_type -> core_type_desc;
    class_    : Longident.t loc -> core_type list -> core_type_desc;
    constr    : Longident.t loc -> core_type list -> core_type_desc;
    extension : extension -> core_type_desc;
    object_   : object_field list -> closed_flag -> core_type_desc;
    package   : package_type -> core_type_desc;
    poly      : label loc list -> core_type -> core_type_desc;
    tuple     : core_type list -> core_type_desc;
    variant   : row_field list -> closed_flag -> string list option -> core_type_desc;
    var       : label -> core_type_desc;
}

type t_module_expr_desc = {
    apply       : module_expr -> module_expr -> module_expr_desc;
    constraint_ : module_expr -> module_type -> module_expr_desc;
    extension   : extension -> module_expr_desc;
    functor_    : label loc -> module_type option -> module_expr -> module_expr_desc;
    ident       : Longident.t loc -> module_expr_desc;
    structure   : structure -> module_expr_desc;
    unpack      : expression -> module_expr_desc;
}

type t_module_type_desc = {
    alias     : Longident.t loc -> module_type_desc;
    extension : extension -> module_type_desc;
    functor_  : label loc -> module_type option -> module_type -> module_type_desc;
    ident     : Longident.t loc -> module_type_desc;
    signature : signature -> module_type_desc;
    typeof    : module_expr -> module_type_desc;
    with_     : module_type -> with_constraint list -> module_type_desc;
}

type t_class_expr_desc = {
    apply       : class_expr -> (arg_label * expression) list -> class_expr_desc;
    constr      : Longident.t loc -> core_type list -> class_expr_desc;
    constraint_ : class_expr -> class_type -> class_expr_desc;
    extension   : extension -> class_expr_desc;
    fun_        : arg_label -> expression option -> pattern -> class_expr -> class_expr_desc;
    let_        : rec_flag -> value_binding list -> class_expr -> class_expr_desc;
    open_       : override_flag -> Longident.t loc -> class_expr -> class_expr_desc;
    structure   : class_structure -> class_expr_desc;
}

type t_class_field_desc = {
    attribute    : attribute -> class_field_desc;
    constraint_  : core_type -> core_type -> class_field_desc;
    extension    : extension -> class_field_desc;
    inherit_     : override_flag -> class_expr -> label loc option -> class_field_desc;
    initializer_ : expression -> class_field_desc;
    method_      : label loc -> private_flag -> class_field_kind -> class_field_desc;
    val_         : label loc -> mutable_flag -> class_field_kind -> class_field_desc;
}

type t_class_field_kind = {
    virtual_ : core_type -> class_field_kind;
    concrete : override_flag -> expression -> class_field_kind;
}

type t_class_type_desc = {
    arrow     : arg_label -> core_type -> class_type -> class_type_desc;
    constr    : Longident.t loc -> core_type list -> class_type_desc;
    extension : extension -> class_type_desc;
    open_     : override_flag -> Longident.t loc -> class_type -> class_type_desc;
    signature : class_signature -> class_type_desc;
}

type t_class_type_field_desc = {
    attribute   : attribute -> class_type_field_desc;
    constraint_ : core_type -> core_type -> class_type_field_desc;
    extension   : extension -> class_type_field_desc;
    inherit_    : class_type -> class_type_field_desc;
    method_     : label loc -> private_flag -> virtual_flag -> core_type -> class_type_field_desc;
    val_        : label loc -> mutable_flag -> virtual_flag -> core_type -> class_type_field_desc;
}

type t_type_kind = {
    variant  : constructor_declaration list -> type_kind;
    record   : label_declaration list -> type_kind;
    abstract : unit -> type_kind;
    open_    : unit -> type_kind;
}

type t_with_constraint = {
    type_     : Longident.t loc -> type_declaration -> with_constraint;
    module_   : Longident.t loc -> Longident.t loc -> with_constraint;
    typesubst : Longident.t loc -> type_declaration -> with_constraint;
    modsubst  : Longident.t loc -> Longident.t loc -> with_constraint;
}

type t_object_field = {
    tag      : label loc -> attributes -> core_type -> object_field;
    inherit_ : core_type -> object_field;
}

type t_row_field = {
    tag      : label loc -> attributes -> bool -> core_type list -> row_field;
    inherit_ : core_type -> row_field;
}

type t_constructor_arguments = {
    tuple  : core_type list -> constructor_arguments;
    record : label_declaration list -> constructor_arguments;
}

type t_extension_constructor_kind = {
    decl   : constructor_arguments -> core_type option -> extension_constructor_kind;
    rebind : Longident.t loc -> extension_constructor_kind;
}

type t = {
    signature           : signature -> signature;
    signature_item      : signature_item -> signature_item;
    signature_item_desc : t_signature_item_desc;
    structure           : structure -> structure;
    structure_item      : structure_item -> structure_item;
    structure_item_desc : t_structure_item_desc;

    pattern          : pattern -> pattern;
    pattern_desc     : t_pattern_desc;
    expression       : expression -> expression;
    expression_desc  : t_expression_desc;
    core_type        : core_type -> core_type;
    core_type_desc   : t_core_type_desc;
    module_expr      : module_expr -> module_expr;
    module_expr_desc : t_module_expr_desc;
    module_type      : module_type -> module_type;
    module_type_desc : t_module_type_desc;

    class_expr            : class_expr -> class_expr;
    class_expr_desc       : t_class_expr_desc;
    class_field           : class_field -> class_field;
    class_field_desc      : t_class_field_desc;
    class_field_kind      : t_class_field_kind;
    class_type            : class_type -> class_type;
    class_type_desc       : t_class_type_desc;
    class_type_field      : class_type_field -> class_type_field;
    class_type_field_desc : t_class_type_field_desc;

    with_constraint            : t_with_constraint;
    object_field               : t_object_field;
    row_field                  : t_row_field;
    type_kind                  : t_type_kind;
    constructor_arguments      : t_constructor_arguments;
    extension_constructor_kind : t_extension_constructor_kind;

    attribute               : attribute -> attribute;
    extension               : extension -> extension;
    class_structure         : class_structure -> class_structure;
    class_description       : class_description -> class_description;
    class_type_declaration  : class_type_declaration -> class_type_declaration;
    extension_constructor   : extension_constructor -> extension_constructor;
    module_binding          : module_binding -> module_binding;
    module_type_declaration : module_type_declaration -> module_type_declaration;
    module_declaration      : module_declaration -> module_declaration;
    open_description        : open_description -> open_description;
    type_extension          : type_extension -> type_extension;
    type_declaration        : type_declaration -> type_declaration;
    value_description       : value_description -> value_description;
    constructor_declaration : constructor_declaration -> constructor_declaration;
    value_binding           : value_binding -> value_binding;
    label_declaration       : label_declaration -> label_declaration;
    include_description     : include_description -> include_description;
    include_declaration     : include_declaration -> include_declaration;
}


let rec signature f x =
    let x = f.signature x in
    list signature_item f x

and structure f x =
    let x = f.structure x in
    list structure_item f x

and signature_item f x =
    let fdesc = f.signature_item_desc in
    f.signature_item
    { x with
        psig_desc =
            match x.psig_desc with
            | Psig_attribute a      -> fdesc.attribute @@ attribute f a
            | Psig_class l          -> fdesc.class_ @@ list class_description f l
            | Psig_class_type l     -> fdesc.class_type @@ list class_type_declaration f l
            | Psig_exception e      -> fdesc.exception_ @@ extension_constructor f e
            | Psig_extension (e, a) -> fdesc.extension (extension f e) @@ attributes f a
            | Psig_include x        -> fdesc.include_ @@ include_description f x
            | Psig_modtype x        -> fdesc.modtype @@ module_type_declaration f x
            | Psig_module x         -> fdesc.module_ @@ module_declaration f x
            | Psig_open x           -> fdesc.open_ @@ open_description f x
            | Psig_recmodule x      -> fdesc.recmodule @@ list module_declaration f x
            | Psig_type (fl, x)     -> fdesc.type_ fl @@ list type_declaration f x
            | Psig_typext x         -> fdesc.typext (type_extension f x)
            | Psig_value x          -> fdesc.value (value_description f x)
    }

and structure_item f x =
    let fdesc = f.structure_item_desc in
    f.structure_item
    { x with
        pstr_desc =
            match x.pstr_desc with
            | Pstr_eval (e, a)      -> fdesc.eval (expression f e) @@ attributes f a
            | Pstr_value (fl, l)    -> fdesc.value fl @@ list value_binding f l
            | Pstr_primitive x      -> fdesc.primitive @@ value_description f x
            | Pstr_type (fl, l)     -> fdesc.type_ fl @@ list type_declaration f l
            | Pstr_typext x         -> fdesc.typext @@ type_extension f x
            | Pstr_exception x      -> fdesc.exception_ @@ extension_constructor f x
            | Pstr_module x         -> fdesc.module_ @@ module_binding f x
            | Pstr_recmodule x      -> fdesc.recmodule @@ list module_binding f x
            | Pstr_modtype x        -> fdesc.modtype @@ module_type_declaration f x
            | Pstr_open x           -> fdesc.open_ @@ open_description f x
            | Pstr_class x          -> fdesc.class_ @@ list class_declaration f x
            | Pstr_class_type x     -> fdesc.class_type @@ list class_type_declaration f x
            | Pstr_include x        -> fdesc.include_ @@ include_declaration f x
            | Pstr_attribute x      -> fdesc.attribute @@ attribute f x
            | Pstr_extension (e, a) -> fdesc.extension (extension f e) @@ attributes f a
    }

and class_description f x = f.class_description @@ class_infos class_type f x

and class_type_declaration f x = f.class_type_declaration @@ class_infos class_type f x

and attribute f x = f.attribute @@ id__payload f x

and extension f x = f.extension @@ id__payload f x

and id__payload f (s, p) =
    (
        s,
        match p with
        | PPat (p, e) -> PPat (pattern f p, opt expression f e)
        | PSig s      -> PSig (signature f s)
        | PStr s      -> PStr (structure f s)
        | PTyp t      -> PTyp (core_type f t)
    )

and attributes f l = list attribute f l

and module_binding f x =
    f.module_binding
    { x with
        pmb_expr = module_expr f x.pmb_expr;
        pmb_attributes = attributes f x.pmb_attributes;
    }

and module_type_declaration f x =
    f.module_type_declaration
    { x with
        pmtd_type = opt module_type f x.pmtd_type;
        pmtd_attributes = attributes f x.pmtd_attributes;
    }

and module_declaration f x =
    f.module_declaration
    { x with
        pmd_type = module_type f x.pmd_type;
        pmd_attributes = attributes f x.pmd_attributes;
    }

and open_description f x =
    f.open_description
    { x with
        popen_attributes = attributes f x.popen_attributes;
    }

and type_extension f x =
    f.type_extension
    { x with
        ptyext_params = list (t2 core_type id) f x.ptyext_params;
        ptyext_constructors = list extension_constructor f x.ptyext_constructors;
        ptyext_private = x.ptyext_private;
        ptyext_attributes = attributes f x.ptyext_attributes;
    }

and value_description f x =
    f.value_description
    { x with
        pval_type = core_type f x.pval_type;
        pval_attributes = attributes f x.pval_attributes;
    }

and class_declaration f x = class_infos class_expr f x

and class_infos : 'a. (t -> 'a -> 'a) -> t -> 'a class_infos -> 'a class_infos =
    fun e f x ->
    { x with
        pci_params = list (t2 core_type id) f x.pci_params;
        pci_expr = e f x.pci_expr;
        pci_attributes = attributes f x.pci_attributes;
    }

and pattern f x =
    let fdesc = f.pattern_desc in
    f.pattern
    { x with
        ppat_attributes = attributes f x.ppat_attributes;
        ppat_desc =
            match x.ppat_desc with
            | Ppat_any                -> fdesc.any ()
            | Ppat_alias (p, n)       -> fdesc.alias (pattern f p) n
            | Ppat_array l            -> fdesc.array (list pattern f l)
            | Ppat_constant c         -> fdesc.constant c
            | Ppat_constraint (p, t)  -> fdesc.constraint_ (pattern f p) (core_type f t)
            | Ppat_construct (lid, p) -> fdesc.construct lid (opt pattern f p)
            | Ppat_exception p        -> fdesc.exception_ (pattern f p)
            | Ppat_extension e        -> fdesc.extension (extension f e)
            | Ppat_interval (c1, c2)  -> fdesc.interval c1 c2
            | Ppat_lazy p             -> fdesc.lazy_ (pattern f p)
            | Ppat_open (lid, p)      -> fdesc.open_ lid (pattern f p)
            | Ppat_or (p1, p2)        -> fdesc.or_ (pattern f p1) (pattern f p2)
            | Ppat_record (l, fl)     -> fdesc.record (list (t2 id pattern) f l) fl
            | Ppat_tuple l            -> fdesc.tuple (list pattern f l)
            | Ppat_type lid           -> fdesc.type_ lid
            | Ppat_unpack s           -> fdesc.unpack s
            | Ppat_var s              -> fdesc.var s
            | Ppat_variant (s, p)     -> fdesc.variant s (opt pattern f p)
    }

and expression f x =
    let fdesc = f.expression_desc in
    f.expression
    { x with
        pexp_attributes = attributes f x.pexp_attributes;
        pexp_desc =
            match x.pexp_desc with
            | Pexp_apply (e, l)            -> fdesc.apply (expression f e) (list (t2 id expression) f l)
            | Pexp_array l                 -> fdesc.array (list expression f l)
            | Pexp_assert e                -> fdesc.assert_ (expression f e)
            | Pexp_coerce (e, t1, t2)      -> fdesc.coerce (expression f e) (opt core_type f t1) (core_type f t2)
            | Pexp_constant x              -> fdesc.constant x
            | Pexp_constraint (e, t)       -> fdesc.constraint_ (expression f e) (core_type f t)
            | Pexp_construct (lid, e)      -> fdesc.construct lid (opt expression f e)
            | Pexp_extension e             -> fdesc.extension (extension f e)
            | Pexp_field (e, lid)          -> fdesc.field (expression f e) lid
            | Pexp_for (p, e1, e2, fl, e3) -> fdesc.for_ (pattern f p) (expression f e1) (expression f e2) fl (expression f e3)
            | Pexp_fun (l, e1, p, e2)      -> fdesc.fun_ l (opt expression f e1) (pattern f p) (expression f e2)
            | Pexp_function l              -> fdesc.function_ (list case f l)
            | Pexp_ident lid               -> fdesc.ident lid
            | Pexp_ifthenelse (e1, e2, e3) -> fdesc.ifthenelse (expression f e1) (expression f e2)  (opt expression f e3)
            | Pexp_lazy e                  -> fdesc.lazy_ (expression f e)
            | Pexp_let (fl, vb, e)         -> fdesc.let_ fl (list value_binding f vb) (expression f e)
            | Pexp_letexception (ec, e)    -> fdesc.letexception (extension_constructor f ec) (expression f e)
            | Pexp_letmodule (s, m, e)     -> fdesc.letmodule s (module_expr f m) (expression f e)
            | Pexp_match (e, l)            -> fdesc.match_ (expression f e) (list case f l)
            | Pexp_new lid                 -> fdesc.new_ lid
            | Pexp_newtype (s, e)          -> fdesc.newtype s (expression f e)
            | Pexp_object c                -> fdesc.object_ (class_stucture f c)
            | Pexp_open (fl, lid, e)       -> fdesc.open_  fl lid (expression f e)
            | Pexp_override l              -> fdesc.override (list (t2 id expression) f l)
            | Pexp_pack m                  -> fdesc.pack (module_expr f m)
            | Pexp_poly (e, l)             -> fdesc.poly (expression f e) (opt core_type f l)
            | Pexp_record (l, e)           -> fdesc.record (list (t2 id expression) f l) (opt expression f e)
            | Pexp_send (e, s)             -> fdesc.send (expression f e) s
            | Pexp_sequence (e1, e2)       -> fdesc.sequence (expression f e1) (expression f e2)
            | Pexp_setfield (e1, lid, e2)  -> fdesc.setfield (expression f e1) lid (expression f e2)
            | Pexp_setinstvar (s, e)       -> fdesc.setinstvar s (expression f e)
            | Pexp_try (e, l)              -> fdesc.try_ (expression f e) (list case f l)
            | Pexp_tuple e                 -> fdesc.tuple (list expression f e)
            | Pexp_unreachable             -> fdesc.unreachable ()
            | Pexp_variant (s, e)          -> fdesc.variant s (opt expression f e)
            | Pexp_while (e1, e2)          -> fdesc.while_ (expression f e1) (expression f e2)
    }

and core_type f x =
    let fdesc = f.core_type_desc in
    f.core_type
    { x with
        ptyp_attributes = attributes f x.ptyp_attributes;
        ptyp_desc =
            match x.ptyp_desc with
            | Ptyp_any -> fdesc.any ()
            | Ptyp_alias (c, s) -> fdesc.alias (core_type f c) s
            | Ptyp_arrow (l, t1, t2) -> fdesc.arrow l (core_type f t1) (core_type f t2)
            | Ptyp_class (lid, l) -> fdesc.class_ lid (list core_type f l)
            | Ptyp_constr (lid, l) -> fdesc.constr lid (list core_type f l)
            | Ptyp_extension x -> fdesc.extension (extension f x)
            | Ptyp_object (l, fl) -> fdesc.object_ (list object_field f l) fl
            | Ptyp_package x -> fdesc.package (package_type f x)
            | Ptyp_poly (l, t) -> fdesc.poly l (core_type f t)
            | Ptyp_tuple l -> fdesc.tuple (list core_type f l)
            | Ptyp_variant (l, fl, o) -> fdesc.variant (list row_field f l) fl o
            | Ptyp_var s -> fdesc.var s
    }

and object_field f =
    function
    | Otag (l, a, t) -> f.object_field.tag l (attributes f a) (core_type f t)
    | Oinherit t     -> f.object_field.inherit_ (core_type f t)

and package_type f x = t2 id (list @@ t2 id core_type) f x

and row_field f =
    function
    | Rtag (lid, a, fl, l) -> f.row_field.tag lid (attributes f a) fl (list core_type f l)
    | Rinherit x           -> f.row_field.inherit_ (core_type f x)

and class_stucture f x =
    f.class_structure
    {
        pcstr_self = pattern f x.pcstr_self;
        pcstr_fields = list class_field f x.pcstr_fields;
    }

and module_expr f x =
    let fdesc = f.module_expr_desc in
    f.module_expr
    { x with
        pmod_attributes = attributes f x.pmod_attributes;
        pmod_desc =
            match x.pmod_desc with
            | Pmod_apply (m1, m2)    -> fdesc.apply (module_expr f m1) (module_expr f m2)
            | Pmod_constraint (m, t) -> fdesc.constraint_ (module_expr f m) (module_type f t)
            | Pmod_extension x       -> fdesc.extension (extension f x)
            | Pmod_functor (s, t, m) -> fdesc.functor_ s (opt module_type f t) (module_expr f m)
            | Pmod_ident x           -> fdesc.ident x
            | Pmod_structure x       -> fdesc.structure (structure f x)
            | Pmod_unpack x          -> fdesc.unpack (expression f x)
    }

and module_type f x =
    let fdesc = f.module_type_desc in
    f.module_type
    { x with
        pmty_attributes = attributes f x.pmty_attributes;
        pmty_desc =
            match x.pmty_desc with
            | Pmty_alias x           -> fdesc.alias x
            | Pmty_extension x       -> fdesc.extension (extension f x)
            | Pmty_functor (s, o, x) -> fdesc.functor_ s (opt module_type f o) (module_type f x)
            | Pmty_ident x           -> fdesc.ident x
            | Pmty_signature x       -> fdesc.signature (signature f x)
            | Pmty_typeof x          -> fdesc.typeof (module_expr f x)
            | Pmty_with (x, l)       -> fdesc.with_ (module_type f x) (list with_constraint f l)
    }

and with_constraint f =
    function
    | Pwith_type (lid, x)      -> f.with_constraint.type_ lid (type_declaration f x)
    | Pwith_module (l1, l2)    -> f.with_constraint.module_  l1 l2
    | Pwith_typesubst (lid, x) -> f.with_constraint.typesubst lid (type_declaration f x)
    | Pwith_modsubst (l1, l2)  -> f.with_constraint.modsubst l1 l2

and type_declaration f x =
    let fdesc = f.type_kind in
    f.type_declaration
    { x with
        ptype_params = list (t2 core_type id) f x.ptype_params;
        ptype_cstrs = list (t3 core_type core_type id) f x.ptype_cstrs;
        ptype_manifest = opt core_type f x.ptype_manifest;
        ptype_attributes = attributes f x.ptype_attributes;
        ptype_kind =
            match x.ptype_kind with
            | Ptype_variant l -> fdesc.variant (list constructor_declaration f l)
            | Ptype_record l  -> fdesc.record (list label_declaration f l)
            | Ptype_abstract  -> fdesc.abstract ()
            | Ptype_open      -> fdesc.open_ ()
    }

and constructor_declaration f x =
    f.constructor_declaration
    { x with
        pcd_args = constructor_arguments f x.pcd_args;
        pcd_res = opt core_type f x.pcd_res;
        pcd_attributes = attributes f x.pcd_attributes;
    }

and class_structure f x =
    {
        pcstr_self = pattern f x.pcstr_self;
        pcstr_fields = list class_field f x.pcstr_fields;
    }

and class_signature f x =
    {
        pcsig_self = core_type f x.pcsig_self;
        pcsig_fields = list class_type_field f x.pcsig_fields;
    }

and class_type f x =
    let fdesc = f.class_type_desc in

    f.class_type
    { x with
        pcty_attributes = attributes f x.pcty_attributes;
        pcty_desc =
            match x.pcty_desc with
            | Pcty_arrow (l, t1, t2) -> fdesc.arrow  l (core_type f t1) (class_type f t2)
            | Pcty_constr (lid, l)   -> fdesc.constr lid (list core_type f l)
            | Pcty_extension x       -> fdesc.extension (extension f x)
            | Pcty_open (fl, lid, t) -> fdesc.open_ fl lid (class_type f t)
            | Pcty_signature s       -> fdesc.signature (class_signature f s)
    }

and class_type_field f x =
    let fdesc = f.class_type_field_desc in
    f.class_type_field
    { x with
        pctf_attributes = attributes f x.pctf_attributes;
        pctf_desc =
            match x.pctf_desc with
            | Pctf_attribute x -> fdesc.attribute (attribute f x)
            | Pctf_constraint (t1, t2) -> fdesc.constraint_ (core_type f t1) (core_type f t2)
            | Pctf_extension x -> fdesc.extension (extension f x)
            | Pctf_inherit x -> fdesc.inherit_ (class_type f x)
            | Pctf_method (s, f1, f2, t) -> fdesc.method_ s f1 f2 (core_type f t)
            | Pctf_val (s, f1, f2, t) -> fdesc.val_ s f1  f2 (core_type f t)
    }

and class_expr f x =
    let fdesc = f.class_expr_desc in
    f.class_expr
    { x with
        pcl_attributes = attributes f x.pcl_attributes;
        pcl_desc =
            match x.pcl_desc with
            | Pcl_apply (x, l) -> fdesc.apply (class_expr f x) (list (t2 id expression) f l)
            | Pcl_constr (lid, l) -> fdesc.constr lid (list core_type f l)
            | Pcl_constraint (x, t) -> fdesc.constraint_ (class_expr f x) (class_type f t)
            | Pcl_extension x -> fdesc.extension (extension f x)
            | Pcl_fun (l, e, p, x) -> fdesc.fun_ l (opt expression f e) (pattern f p) (class_expr f x)
            | Pcl_let (fl, l, x) -> fdesc.let_ fl (list value_binding f l) (class_expr f x)
            | Pcl_open (fl, lid, x) -> fdesc.open_ fl lid (class_expr f x)
            | Pcl_structure x -> fdesc.structure (class_structure f x)
    }

and class_field f x =
    let fdesc = f.class_field_desc in
    f.class_field
    { x with
        pcf_attributes = attributes f x.pcf_attributes;
        pcf_desc =
            match x.pcf_desc with
            | Pcf_attribute x -> fdesc.attribute (attribute f x)
            | Pcf_constraint (t1, t2) -> fdesc.constraint_ (core_type f t1) (core_type f t2)
            | Pcf_extension x -> fdesc.extension (extension f x)
            | Pcf_inherit (fl, c, s) -> fdesc.inherit_ fl (class_expr f c) s
            | Pcf_initializer x -> fdesc.initializer_ (expression f x)
            | Pcf_method (s, fl, x) -> fdesc.method_ s fl (class_field_kind f x)
            | Pcf_val (s, fl, x) -> fdesc.val_ s fl (class_field_kind f x)
    }

and class_field_kind f =
    function
    | Cfk_virtual t -> f.class_field_kind.virtual_ (core_type f t)
    | Cfk_concrete (fl, x) -> f.class_field_kind.concrete fl (expression f x)

and value_binding f x =
    f.value_binding
    { x with
        pvb_pat = pattern f x.pvb_pat;
        pvb_expr = expression f x.pvb_expr;
        pvb_attributes = attributes f x.pvb_attributes;
    }

and extension_constructor f x =
    f.extension_constructor
    { x with
        pext_kind = extension_constructor_kind f x.pext_kind;
        pext_attributes = attributes f x.pext_attributes;
    }

and extension_constructor_kind f =
    function
    | Pext_decl (ca, ct) -> f.extension_constructor_kind.decl (constructor_arguments f ca) (opt core_type f ct)
    | Pext_rebind lid -> f.extension_constructor_kind.rebind lid

and constructor_arguments f =
    function
    | Pcstr_tuple l -> f.constructor_arguments.tuple (list core_type f l)
    | Pcstr_record l -> f.constructor_arguments.record (list label_declaration f l)

and label_declaration f x =
    f.label_declaration
    { x with
        pld_type = core_type f x.pld_type;
        pld_attributes = attributes f x.pld_attributes;
    }

and case f x =
    {
        pc_lhs = pattern f x.pc_lhs;
        pc_guard = opt expression f x.pc_guard;
        pc_rhs = expression f x.pc_rhs;
    }

and include_description f x =
    f.include_description @@
    include_infos module_type f x
and include_declaration f x =
    f.include_declaration @@
    include_infos module_expr f x

and include_infos: 'a. (t -> 'a -> 'a) -> t -> 'a include_infos -> 'a include_infos  =
    fun e f x ->
    { x with
        pincl_mod = e f x.pincl_mod;
        pincl_attributes = attributes f x.pincl_attributes;
    }

and opt: 'a. (t -> 'a -> 'a) -> t -> 'a option -> 'a option =
    fun f m x -> Option.map ~f:(f m) x

and list: 'a. (t -> 'a -> 'a) -> t -> 'a list -> 'a list =
    fun f m x -> List.map ~f:(f m) x

and t2: 'a 'b. (t -> 'a -> 'a) -> (t -> 'b -> 'b) -> t -> 'a * 'b -> 'a * 'b =
    fun f1 f2 m (a, b) -> f1 m a, f2 m b

and t3: 'a 'b 'c. (t -> 'a -> 'a) -> (t -> 'b -> 'b) -> (t -> 'c -> 'c) -> t -> 'a * 'b * 'c -> 'a * 'b * 'c =
    fun f1 f2 f3 m (a, b, c) -> f1 m a, f2 m b, f3 m c

and id: 'a. t -> 'a -> 'a = fun _ x -> x


let default =
    let id: 'a. 'a -> 'a = fun x -> x in

    {
        signature           = id;
        signature_item      = id;
        signature_item_desc = {
            attribute  = (fun x -> Psig_attribute x);
            class_     = (fun x -> Psig_class x);
            class_type = (fun x -> Psig_class_type x);
            exception_ = (fun x -> Psig_exception x);
            extension  = (fun e a -> Psig_extension (e, a));
            include_   = (fun x -> Psig_include x);
            modtype    = (fun x -> Psig_modtype x);
            module_    = (fun x -> Psig_module x);
            open_      = (fun x -> Psig_open x);
            recmodule  = (fun x -> Psig_recmodule x);
            type_      = (fun f x -> Psig_type (f, x));
            typext     = (fun x -> Psig_typext x);
            value      = (fun x -> Psig_value x);
        };
        structure           = id;
        structure_item      = id;
        structure_item_desc = {
            eval       = (fun e a -> Pstr_eval (e, a));
            value      = (fun f x -> Pstr_value (f, x));
            primitive  = (fun x -> Pstr_primitive x);
            type_      = (fun f x -> Pstr_type (f, x));
            typext     = (fun x -> Pstr_typext x);
            exception_ = (fun x -> Pstr_exception x);
            module_    = (fun x -> Pstr_module x);
            recmodule  = (fun x -> Pstr_recmodule x);
            modtype    = (fun x -> Pstr_modtype x);
            open_      = (fun x -> Pstr_open x);
            class_     = (fun x -> Pstr_class x);
            class_type = (fun x -> Pstr_class_type x);
            include_   = (fun x -> Pstr_include x);
            attribute  = (fun x -> Pstr_attribute x);
            extension  = (fun e a -> Pstr_extension (e, a));
        };

        pattern          = id;
        pattern_desc     = {
            any         = (fun () -> Ppat_any);
            var         = (fun x -> Ppat_var x);
            alias       = (fun x n -> Ppat_alias (x, n));
            constant    = (fun x -> Ppat_constant x);
            interval    = (fun x y -> Ppat_interval (x, y));
            tuple       = (fun x -> Ppat_tuple x);
            construct   = (fun x a -> Ppat_construct (x, a));
            variant     = (fun x a -> Ppat_variant (x, a));
            record      = (fun x f -> Ppat_record (x, f));
            array       = (fun x -> Ppat_array x);
            or_         = (fun x y -> Ppat_or (x, y));
            constraint_ = (fun x t -> Ppat_constraint (x, t));
            type_       = (fun x -> Ppat_type x);
            lazy_       = (fun x -> Ppat_lazy x);
            unpack      = (fun x -> Ppat_unpack x);
            exception_  = (fun x -> Ppat_exception x);
            extension   = (fun x -> Ppat_extension x);
            open_       = (fun x p -> Ppat_open (x, p));
        };
        expression       = id;
        expression_desc  = {
            apply        = (fun x a -> Pexp_apply (x, a));
            array        = (fun x -> Pexp_array x);
            assert_      = (fun x -> Pexp_assert x);
            coerce       = (fun x t1 t2 -> Pexp_coerce (x, t1, t2));
            constant     = (fun x -> Pexp_constant x);
            constraint_  = (fun x t -> Pexp_constraint (x, t));
            construct    = (fun x a -> Pexp_construct (x, a));
            extension    = (fun x -> Pexp_extension x);
            field        = (fun x f -> Pexp_field (x, f));
            for_         = (fun p e1 e2 f e3 -> Pexp_for (p, e1, e2, f, e3));
            fun_         = (fun l d p e -> Pexp_fun (l, d, p, e));
            function_    = (fun x -> Pexp_function x);
            ident        = (fun x -> Pexp_ident x);
            ifthenelse   = (fun e1 e2 e3 -> Pexp_ifthenelse (e1, e2, e3));
            lazy_        = (fun x -> Pexp_lazy x);
            let_         = (fun f x e -> Pexp_let (f, x, e));
            letexception = (fun x e -> Pexp_letexception (x, e));
            letmodule    = (fun l x e -> Pexp_letmodule (l, x, e));
            match_       = (fun e x -> Pexp_match (e, x));
            new_         = (fun x -> Pexp_new x);
            newtype      = (fun l x -> Pexp_newtype (l, x));
            object_      = (fun x -> Pexp_object x);
            open_        = (fun f n x -> Pexp_open (f, n, x));
            override     = (fun x -> Pexp_override x);
            pack         = (fun x -> Pexp_pack x);
            poly         = (fun x t -> Pexp_poly (x, t));
            record       = (fun x e -> Pexp_record (x, e));
            send         = (fun x n -> Pexp_send (x, n));
            sequence     = (fun x y -> Pexp_sequence (x, y));
            setfield     = (fun x n y -> Pexp_setfield (x, n, y));
            setinstvar   = (fun n x -> Pexp_setinstvar (n, x));
            try_         = (fun x c -> Pexp_try (x, c));
            tuple        = (fun x -> Pexp_tuple x);
            unreachable  = (fun _ -> Pexp_unreachable);
            variant      = (fun n x -> Pexp_variant (n, x));
            while_       = (fun x y -> Pexp_while (x, y));
        };
        core_type        = id;
        core_type_desc   = {
            any       = (fun _ -> Ptyp_any);
            alias     = (fun x n -> Ptyp_alias (x, n));
            arrow     = (fun l x y -> Ptyp_arrow (l, x, y));
            class_    = (fun n x -> Ptyp_class (n, x));
            constr    = (fun n x -> Ptyp_constr (n, x));
            extension = (fun x -> Ptyp_extension x);
            object_   = (fun x f -> Ptyp_object (x, f));
            package   = (fun x -> Ptyp_package x);
            poly      = (fun n x -> Ptyp_poly (n, x));
            tuple     = (fun x -> Ptyp_tuple x);
            variant   = (fun r f x -> Ptyp_variant (r, f, x));
            var       = (fun x -> Ptyp_var x);
        };
        module_expr      = id;
        module_expr_desc = {
            apply       = (fun x y -> Pmod_apply (x, y));
            constraint_ = (fun x t -> Pmod_constraint (x, t));
            extension   = (fun x -> Pmod_extension x);
            functor_    = (fun n t x -> Pmod_functor (n, t, x));
            ident       = (fun x -> Pmod_ident x);
            structure   = (fun x -> Pmod_structure x);
            unpack      = (fun x -> Pmod_unpack x);
        };
        module_type      = id;
        module_type_desc = {
            alias     = (fun x -> Pmty_alias x);
            extension = (fun x -> Pmty_extension x);
            functor_  = (fun n x t -> Pmty_functor (n, x, t));
            ident     = (fun x -> Pmty_ident x);
            signature = (fun x -> Pmty_signature x);
            typeof    = (fun x -> Pmty_typeof x);
            with_     = (fun x l -> Pmty_with (x, l));
        };

        class_expr            = id;
        class_expr_desc       = {
            apply       = (fun x a -> Pcl_apply (x, a));
            constr      = (fun n x -> Pcl_constr (n, x));
            constraint_ = (fun x t -> Pcl_constraint (x, t));
            extension   = (fun x -> Pcl_extension x);
            fun_        = (fun l e p c -> Pcl_fun (l, e, p, c));
            let_        = (fun f v x -> Pcl_let (f, v, x));
            open_       = (fun f n x -> Pcl_open (f, n, x));
            structure   = (fun x -> Pcl_structure x);
        };
        class_field           = id;
        class_field_desc      = {
            attribute    = (fun x -> Pcf_attribute x);
            constraint_  = (fun x y -> Pcf_constraint (x, y));
            extension    = (fun x -> Pcf_extension x);
            inherit_     = (fun f x n -> Pcf_inherit (f, x, n));
            initializer_ = (fun x -> Pcf_initializer x);
            method_      = (fun n f x -> Pcf_method (n, f, x));
            val_         = (fun n f x -> Pcf_val (n, f, x));
        };
        class_field_kind      = {
            virtual_ = (fun x -> Cfk_virtual x);
            concrete = (fun f x -> Cfk_concrete (f, x));
        };
        class_type            = id;
        class_type_desc       = {
            arrow     = (fun l x c -> Pcty_arrow (l, x, c));
            constr    = (fun n x -> Pcty_constr (n, x));
            extension = (fun x -> Pcty_extension x);
            open_     = (fun f n x -> Pcty_open (f, n, x));
            signature = (fun x -> Pcty_signature x);
        };
        class_type_field      = id;
        class_type_field_desc = {
            attribute   = (fun x -> Pctf_attribute x);
            constraint_ = (fun x y -> Pctf_constraint (x, y));
            extension   = (fun x -> Pctf_extension x);
            inherit_    = (fun x -> Pctf_inherit x);
            method_     = (fun n f1 f2 x -> Pctf_method (n, f1, f2, x));
            val_        = (fun n f1 f2 x -> Pctf_val (n, f1, f2, x));
        };

        with_constraint            = {
            type_     = (fun x y -> Pwith_type (x, y));
            module_   = (fun x y -> Pwith_module (x, y));
            typesubst = (fun x y -> Pwith_typesubst (x, y));
            modsubst  = (fun x y -> Pwith_modsubst (x, y));
        };
        object_field               = {
            tag      = (fun n a t -> Otag (n, a, t));
            inherit_ = (fun x -> Oinherit x);
        };
        row_field                  = {
            tag      = (fun n a f l -> Rtag (n, a, f, l));
            inherit_ = (fun x -> Rinherit x);
        };
        type_kind                  = {
            variant  = (fun x -> Ptype_variant x);
            record   = (fun x -> Ptype_record x);
            abstract = (fun _ -> Ptype_abstract);
            open_    = (fun _ -> Ptype_open);
        };
        constructor_arguments      = {
            tuple  = (fun x -> Pcstr_tuple x);
            record = (fun x -> Pcstr_record x);
        };
        extension_constructor_kind = {
            decl   = (fun x t -> Pext_decl (x, t));
            rebind = (fun x -> Pext_rebind x);
        };

        attribute               = id;
        extension               = id;
        class_structure         = id;
        class_description       = id;
        class_type_declaration  = id;
        extension_constructor   = id;
        module_binding          = id;
        module_type_declaration = id;
        module_declaration      = id;
        open_description        = id;
        type_extension          = id;
        type_declaration        = id;
        value_description       = id;
        constructor_declaration = id;
        value_binding           = id;
        label_declaration       = id;
        include_description     = id;
        include_declaration     = id;
    }
