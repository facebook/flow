(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Anno = Type_annotation
module Flow = Flow_js

open Reason

type field = Type.polarity * field'
and field' = Annot of Type.t | Infer of Func_sig.t

type signature = {
  reason: reason;
  super: Type.t;
  fields: field SMap.t;
  private_fields: field SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: Func_sig.t Nel.t SMap.t;
  getters: Func_sig.t SMap.t;
  setters: Func_sig.t SMap.t;
}

type t = {
  id: int;
  structural: bool;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  implements: Type.t list;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: Func_sig.t list;
  static: signature;
  instance: signature;
}

type super =
  | Interface of {
      extends: Type.t list;
      callable: bool;
      static_callable: bool;
    }
  | Class of {
      extends: extends;
      mixins: Type.t list; (* declare class only *)
      implements: Type.t list
    }

and extends =
  | Explicit of Type.t
  | Implicit of { null: bool }

let empty id reason tparams tparams_map super =
  let empty_sig reason super = {
    reason; super;
    fields = SMap.empty;
    private_fields = SMap.empty;
    methods = SMap.empty;
    getters = SMap.empty;
    setters = SMap.empty;
  } in
  let structural, super, ssuper, implements =
    let open Type in
    let super_reason = replace_reason (fun d -> RSuperOf d) reason in
    match super with
    | Interface {extends; callable; static_callable} ->
      let super, ssuper = match extends with
      | [] ->
        ObjProtoT super_reason, ObjProtoT super_reason
      | t0::ts ->
        (* Interfaces support multiple inheritance. *)
        let super = match ts with
        | [] -> t0
        | t1::ts -> DefT (super_reason, IntersectionT (InterRep.make t0 t1 ts))
        in
        super, class_type super
      in
      (* If the interface definition includes a callable property, add the
         function prototype to the super type. *)
      let super =
        if callable
        then
          let rep = InterRep.make super (FunProtoT super_reason) [] in
          DefT (super_reason, IntersectionT rep)
        else super
      in
      (* This case exists to support the constructor_annots case (see
         tests/constructor_annots), which allows users to case a function F to a
         class-like type described by an interface with a static callable
         property with (F: Class<I>). We should remove this feature and make
         static callable properties on interfaces an error. *)
      let ssuper =
        if static_callable
        then
          let rep = InterRep.make ssuper (FunProtoT super_reason) [] in
          DefT (super_reason, IntersectionT rep)
        else ssuper
      in
      true, super, ssuper, []
    | Class {extends; mixins; implements} ->
      (* Class statics inherit properties from the function prototype, like
         bind, call, and apply. Despite this, classes are generally not callable
         without new (exceptions include cast functions like Number). *)
      let super, ssuper = match extends with
      | Explicit t -> t, class_type t
      | Implicit { null } ->
        (* The builtin Object class represents the top of the prototype chain,
           so it's super type should be `null` to signify the end. *)
        let super =
          if null
          then NullT.make super_reason
          else ObjProtoT super_reason
        in
        super, FunProtoT super_reason
      in
      (* Classes also support multiple inheritance through mixins. Note that
         mixins override explicit extends. *)
      let super = match mixins with
      | [] -> super
      | t0::ts ->
        let t1, ts = match ts with
        | [] -> super, []
        | t1::ts -> t1, ts@[super]
        in
        DefT (super_reason, IntersectionT (InterRep.make t0 t1 ts))
      in
      false, super, ssuper, implements
  in
  let constructor = [] in
  let static =
    let reason = replace_reason (fun desc -> RStatics desc) reason in
    empty_sig reason ssuper
  in
  let instance = empty_sig reason super in
  { id; structural; tparams; tparams_map; constructor; static; instance;
    implements }

let map_sig ~static f s =
  if static
  then {s with static = f s.static}
  else {s with instance = f s.instance}

let with_sig ~static f s =
  if static then f s.static else f s.instance

let add_private_field name fld = map_sig (fun s -> {
  s with
  private_fields = SMap.add name fld s.private_fields;
})

let add_constructor fsig s =
  {s with constructor = [fsig]}

let add_default_constructor reason s =
  let fsig = Func_sig.default_constructor reason in
  add_constructor fsig s

let append_constructor fsig s =
  {s with constructor = fsig::s.constructor}

let add_field ~static name fld = map_sig ~static (fun s -> {
  s with
  fields = SMap.add name fld s.fields;
  methods = if static then SMap.remove name s.methods else s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_method ~static name fsig = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.add name (Nel.one fsig) s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method ~static name fsig = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = (
    match SMap.get name s.methods with
    | Some fsigs -> SMap.add name (Nel.cons fsig fsigs) s.methods
    | None -> SMap.add name (Nel.one fsig) s.methods
  );
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_getter ~static name fsig = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.remove name s.methods;
  getters = SMap.add name fsig s.getters;
})

let add_setter ~static name fsig = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.remove name s.methods;
  setters = SMap.add name fsig s.setters;
})

let mk_method cx ~expr x loc func =
  Func_sig.mk cx x.tparams_map ~expr loc func

let mk_field cx ~polarity x reason typeAnnotation init =
  polarity, match init with
  | None -> Annot (Anno.mk_type_annotation cx x.tparams_map reason typeAnnotation)
  | Some expr -> Infer (
    Func_sig.field_initializer cx x.tparams_map reason expr typeAnnotation)

let mem_constructor {constructor; _} = constructor <> []

let mem_field x = with_sig (fun s -> SMap.mem x s.fields)

let iter_methods f s =
  SMap.iter (fun _ -> Nel.iter f) s.methods;
  SMap.iter (fun _ -> f) s.getters;
  SMap.iter (fun _ -> f) s.setters

(* TODO? *)
let subst_field cx map (polarity, field) =
  polarity, match field with
  | Annot t -> Annot (Flow.subst cx map t)
  | Infer fsig -> Infer (Func_sig.subst cx map fsig)

let subst_sig cx map s = {
  reason = s.reason;
  super = Flow.subst cx map s.super;
  fields = SMap.map (subst_field cx map) s.fields;
  private_fields = SMap.map (subst_field cx map) s.private_fields;
  methods = SMap.map (Nel.map (Func_sig.subst cx map)) s.methods;
  getters = SMap.map (Func_sig.subst cx map) s.getters;
  setters = SMap.map (Func_sig.subst cx map) s.setters;
}

let generate_tests cx f x =
  Flow.generate_tests cx x.instance.reason x.tparams (fun map -> f {
    id = x.id;
    structural = x.structural;
    tparams = x.tparams;
    tparams_map = SMap.map (Flow.subst cx map) x.tparams_map;
    implements = List.map (Flow.subst cx map) x.implements;
    constructor = List.map (Func_sig.subst cx map) x.constructor;
    static = subst_sig cx map x.static;
    instance = subst_sig cx map x.instance;
  })

let to_field (polarity, field) =
  let t = match field with
  | Annot t -> t
  | Infer fsig -> Func_sig.gettertype fsig
  in
  Type.Field (t, polarity)

let elements cx ?constructor s =
  let methods =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.map Type.(fun xs ->
      match Nel.rev_map (Func_sig.methodtype cx) xs with
      | t, [] -> t
      | t0, t1::ts -> DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts))
    ) s.methods
  in

  (* Re-add the constructor as a method. *)
  let methods = match constructor with
  | Some t -> SMap.add "constructor" t methods
  | None -> methods
  in

  (* If there is a both a getter and a setter, then flow the setter type to
     the getter. Otherwise just use the getter type or the setter type *)
  let getters = SMap.map Func_sig.gettertype s.getters in
  let setters = SMap.map Func_sig.settertype s.setters in
  let getters_and_setters = SMap.merge (fun _ getter setter ->
    match getter, setter with
    | Some t1, Some t2 -> Some (Type.GetSet (t1, t2))
    | Some t, None -> Some (Type.Get t)
    | None, Some t -> Some (Type.Set t)
    | _ -> None
  ) getters setters in

  let fields = SMap.map to_field s.fields in

  (* Treat getters and setters as fields *)
  let fields = SMap.union getters_and_setters fields in

  let methods = SMap.map (fun t -> Type.Method t) methods in

  (* Only un-initialized fields require annotations, so determine now
   * (syntactically) which fields have initializers *)
  let initialized_field_names = SMap.fold (fun x (_, field) acc ->
    match field with
    | Annot _ -> acc
    | Infer _ -> SSet.add x acc
  ) s.fields SSet.empty in

  initialized_field_names, fields, methods

let arg_polarities x =
  List.fold_left Type.(fun acc tp ->
    SMap.add tp.name tp.polarity acc
  ) SMap.empty x.tparams

let statictype cx s =
  let _, fields, methods = elements cx s in
  let props = SMap.union fields methods
    ~combine:(fun _ _ ->
      Utils_js.assert_false (Utils_js.spf
        "static fields and methods must be disjoint: %s"
        (Debug_js.dump_reason cx s.reason)))
  in
  (* Statics are not exact, because we allow width subtyping between them.
     Specifically, given class A and class B extends A, Class<B> <: Class<A>. *)
  let static =
    Flow.mk_object_with_map_proto cx s.reason props s.super
      ~sealed:true ~exact:false
  in
  let open Type in
  match static with
  | DefT (_, ObjT o) -> o
  | _ -> failwith "statics must be an ObjT"

let insttype cx s =
  let constructor =
    let ts = List.rev_map (Func_sig.methodtype cx) s.constructor in
    match ts with
    | [] -> None
    | [t] -> Some t
    | t0::t1::ts ->
      let open Type in
      let t = DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts)) in
      Some t
  in
  let inited_fields, fields, methods = elements cx ?constructor s.instance in
  { Type.
    class_id = s.id;
    type_args = s.tparams_map;
    arg_polarities = arg_polarities s;
    fields_tmap = Context.make_property_map cx fields;
    initialized_field_names = inited_fields;
    methods_tmap = Context.make_property_map cx methods;
    mixins = false;
    structural = s.structural;
  }

let add_this self cx reason tparams tparams_map =
  (* We haven't computed the instance type yet, but we can still capture a
     reference to it using the class name (as long as the class has a name).
     We need this reference to constrain the `this` in the class. *)
  let rec_instance_type =
    match tparams with
    | [] ->
      Flow.mk_instance cx reason self
    | _ ->
      let tparams = List.map (fun tp -> Type.BoundT tp) tparams in
      Type.typeapp self tparams
  in
  let this_tp = { Type.
    name = "this";
    reason = replace_reason_const RThisType reason;
    bound = rec_instance_type;
    polarity = Type.Positive;
    default = None;
  } in
  (* Add the type of `this` to the end of the list of type
     parameters. Remember, order is important, since we don't have recursive
     bounds (aka F-bounds): the bound of This refers to all the other type
     parameters! *)
  tparams@[this_tp],
  SMap.add "this" (Type.BoundT this_tp) tparams_map

let remove_this x =
  if x.structural then x else {
    x with
    tparams = List.rev (List.tl (List.rev x.tparams));
    tparams_map = SMap.remove "this" x.tparams_map;
  }

let thistype cx x =
  let x = remove_this x in
  let {
    implements;
    static = {reason = sreason; _};
    instance = {reason; super; _};
    _;
  } = x in
  let open Type in
  let static = DefT (sreason, ObjT (statictype cx x.static)) in
  DefT (reason, InstanceT (static, super, implements, insttype cx x))

let check_implements cx x =
  let this = thistype cx x in
  List.iter (fun i ->
    Flow.flow cx (i, Type.(ImplementsT (UnknownUse, this)))
  ) x.implements

let check_super cx x =
  let x = remove_this x in
  let reason = x.instance.reason in
  let open Type in
  Flow.flow cx (x.static.super,
    SuperT (reason, DerivedStatics (statictype cx x.static)));
  Flow.flow cx (x.instance.super,
    SuperT (reason, DerivedInstance (insttype cx x)))

(* TODO: Ideally we should check polarity for all class types, but this flag is
   flipped off for interface/declare class currently. *)
let classtype cx ?(check_polarity=true) x =
  let this = thistype cx x in
  let { structural; tparams; _ } = remove_this x in
  let open Type in
  (if check_polarity then Flow.check_polarity cx Positive this);
  let t = if structural then class_type this else this_class_type this in
  poly_type (Flow.mk_nominal cx) tparams t

let mk_super cx tparams_map c targs = Type.(
  (* A super class must be parameterized by This, so that it can be
     specialized to this class and its subclasses when properties are looked
     up on their instances. *)
  let params = Anno.extract_type_param_instantiations targs in
  let this = SMap.find_unsafe "this" tparams_map in
  match params with
  | None ->
      (* No type params, but `c` could still be a polymorphic class that must
         be implicitly instantiated. We need to do this before we try to
         this-specialize `c`. *)
      let reason = reason_of_t c in
      let c = Flow.mk_tvar_derivable_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (reason, reason, None, None, tvar))
      ) in
      this_typeapp c this None
  | Some params ->
      let tparams = List.map (Anno.convert cx tparams_map) params in
      this_typeapp c this (Some tparams)
)

let mk_interface_super cx structural tparams_map (r, id, targs) =
  let desc, lookup_mode =
    if structural then "extends", Env.LookupMode.ForType
    else "mixins", Env.LookupMode.ForValue in
  let i = Anno.convert_qualification ~lookup_mode cx desc id in
  if structural then
    let params = Anno.extract_type_param_instantiations targs in
    Anno.mk_nominal_type cx r tparams_map (i, params)
  else mk_super cx tparams_map i targs

let mk_extends cx tparams_map ~expr = function
  | None, None -> Implicit { null = false }
  | None, _ -> assert false (* type args with no head expr *)
  | Some e, targs ->
    let c = expr cx e in
    Explicit (mk_super cx tparams_map c targs)

let mk_mixins cx tparams_map (r, id, targs) =
  let i =
    let lookup_mode = Env.LookupMode.ForValue in
    Anno.convert_qualification ~lookup_mode cx "mixins" id
  in
  let props_bag = Flow.mk_tvar_derivable_where cx r (fun tvar ->
    Flow.flow cx (i, Type.MixinT (r, tvar))
  ) in
  mk_super cx tparams_map props_bag targs

let warn_or_ignore_decorators cx = function
| [] -> ()
| (start_loc, _)::ds ->
  let loc = List.fold_left (fun start_loc (end_loc, _) ->
    Loc.btwn start_loc end_loc
  ) start_loc ds in
  match Context.esproposal_decorators cx with
  | Options.ESPROPOSAL_ENABLE -> failwith "Decorators cannot be enabled!"
  | Options.ESPROPOSAL_IGNORE -> ()
  | Options.ESPROPOSAL_WARN ->
    Flow_js.add_output cx (Flow_error.EExperimentalDecorators loc)

let warn_or_ignore_class_properties cx ~static loc =
  let config_setting =
    if static
    then Context.esproposal_class_static_fields cx
    else Context.esproposal_class_instance_fields cx
  in
  match config_setting with
  | Options.ESPROPOSAL_ENABLE
  | Options.ESPROPOSAL_IGNORE -> ()
  | Options.ESPROPOSAL_WARN ->
    Flow_js.add_output cx
      (Flow_error.EExperimentalClassProperties (loc, static))

let warn_unsafe_getters_setters cx loc =
  if not (Context.enable_unsafe_getters_and_setters cx)
  then Flow_js.add_output cx (Flow_error.EUnsafeGetSet loc)

(* Process a class definition, returning a (polymorphic) class type. A class
   type is a wrapper around an instance type, which contains types of instance
   members, a pointer to the super instance type, and a container for types of
   static members. The static members can be thought of as instance members of a
   "metaclass": thus, the static type is itself implemented as an instance
   type. *)
let mk cx _loc reason self ~expr =
  fun { Ast.Class.
    body = (_, { Ast.Class.Body.body = elements });
    superClass;
    typeParameters;
    superTypeParameters;
    implements;
    classDecorators;
    _;
  } ->

  warn_or_ignore_decorators cx classDecorators;

  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx typeParameters
  in

  let tparams, tparams_map =
    add_this self cx reason tparams tparams_map
  in

  let class_sig =
    let id = Flow.mk_nominal cx in
    let extends =
      mk_extends cx tparams_map ~expr (superClass, superTypeParameters)
    in
    let implements = List.map (fun (_, i) ->
      let { Ast.Class.Implements.id = (loc, name); typeParameters } = i in
      let reason = mk_reason (RCustom "implements") loc in
      let c = Env.get_var ~lookup_mode:Env.LookupMode.ForType cx name loc in
      let params = Anno.extract_type_param_instantiations typeParameters in
      Anno.mk_nominal_type cx reason tparams_map (c, params)
    ) implements in
    let super = Class { extends; mixins = []; implements } in
    empty id reason tparams tparams_map super
  in

  (* In case there is no constructor, pick up a default one. *)
  let class_sig =
    if superClass <> None
    then
      (* Subclass default constructors are technically of the form (...args) =>
         { super(...args) }, but we can approximate that using flow's existing
         inheritance machinery. *)
      (* TODO: Does this distinction matter for the type checker? *)
      class_sig
    else
      let reason = replace_reason_const RDefaultConstructor reason in
      add_default_constructor reason class_sig
  in

  (* All classes have a static "name" property. *)
  let class_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (Type.Neutral, Annot t) class_sig
  in

  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  List.fold_left Ast.Class.(fun c -> function
    (* instance and static methods *)
    | Body.Property (_, {
        Property.key = Ast.Expression.Object.Property.PrivateName _;
        _
      }) -> failwith "Internal Error: Found non-private field with private name"

    | Body.Method (_, {
        Method.key = Ast.Expression.Object.Property.PrivateName _;
        _
      }) -> failwith "Internal Error: Found method with private name"

    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_, name);
        value = (_, func);
        kind;
        static;
        decorators;
      }) ->

      warn_or_ignore_decorators cx decorators;

      Ast.Class.Method.(match kind with
      | Get | Set -> warn_unsafe_getters_setters cx loc
      | _ -> ());

      let add = match kind with
      | Method.Constructor -> add_constructor
      | Method.Method -> add_method ~static name
      | Method.Get -> add_getter ~static name
      | Method.Set -> add_setter ~static name
      in
      let method_sig = mk_method cx ~expr c loc func in
      add method_sig c

    (* fields *)
    | Body.PrivateField(loc, {
        PrivateField.key = (_, (_, name));
        typeAnnotation;
        value;
        static;
        variance;
        _;
      }) ->
        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let reason = mk_reason (RProperty (Some name)) loc in
        let polarity = Anno.polarity variance in
        let field = mk_field cx ~polarity c reason typeAnnotation value in
        add_private_field ~static name field c

    | Body.Property (loc, {
      Property.key = Ast.Expression.Object.Property.Identifier (_, name);
        typeAnnotation;
        value;
        static;
        variance;
        _;
      }) ->
        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let reason = mk_reason (RProperty (Some name)) loc in
        let polarity = Anno.polarity variance in
        let field = mk_field cx ~polarity c reason typeAnnotation value in
        add_field ~static name field c

    (* literal LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Literal _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Literal _;
        _
      }) ->
        Flow_js.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, ClassPropertyLiteral));
        c

    (* computed LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Computed _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Computed _;
        _
      }) ->
        Flow_js.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, ClassPropertyComputed));
        c
  ) class_sig elements

let extract_extends cx structural =
  let rec loop acc first = function
    | [] -> List.rev acc
    | (loc, {Ast.Type.Generic.id; typeParameters})::rest ->
      if structural || first then
        let name = Anno.qualified_name id in
        let r = mk_reason (RCustom name) loc in
        loop ((r, id, typeParameters)::acc) false rest
      else (
        Flow_js.add_output cx Flow_error.(EUnsupportedSyntax
          (loc, ClassExtendsMultiple));
        loop acc false []
      )
  in
  loop [] true

let extract_mixins _cx =
  List.map (fun (loc, {Ast.Type.Generic.id; typeParameters}) ->
    let name = Anno.qualified_name id in
    let r = mk_reason (RCustom name) loc in
    r, id, typeParameters
  )

let is_object_builtin_libdef (loc, name) =
  name = "Object" &&
  match Loc.source loc with
  | None -> false
  | Some source -> File_key.is_lib_file source

let mk_interface cx loc reason structural self = Ast.Statement.(
  fun { Interface.
    id = ident;
    typeParameters;
    body = (_, { Ast.Type.Object.properties; _ });
    extends;
    mixins;
    _;
  } ->

  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx typeParameters in

  let tparams, tparams_map =
    if not structural
    then add_this self cx reason tparams tparams_map
    else tparams, tparams_map in

  let iface_sig =
    let id = Flow.mk_nominal cx in
    let extends = List.map
      (mk_interface_super cx structural tparams_map)
      (extract_extends cx structural extends)
    in
    let mixins = List.map
      (mk_mixins cx tparams_map)
      (extract_mixins cx mixins)
    in
    let super =
      if structural then
        let callable = List.exists Ast.Type.Object.(function
          | CallProperty (_, { CallProperty.static; _ }) -> not static
          | _ -> false
        ) properties in
        let static_callable = List.exists Ast.Type.Object.(function
          | CallProperty (_, { CallProperty.static; _ }) -> static
          | _ -> false
        ) properties in
        Interface { extends; callable; static_callable }
      else
        let extends = match extends with
        | [] -> Implicit { null = is_object_builtin_libdef ident }
        | [t] -> Explicit t
        | _ -> failwith "declare class with multiple extends"
        in
        Class { extends; mixins; implements = [] }
    in
    empty id reason tparams tparams_map super
  in

  let iface_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (Type.Neutral, Annot t) iface_sig
  in

  let iface_sig = List.fold_left Ast.Type.Object.(fun x -> function
    | CallProperty (loc, { CallProperty.value = (_, func); static }) ->
      let fsig = Func_sig.convert cx tparams_map loc func in
      append_method ~static "$call" fsig x
    | Indexer (loc, { Indexer.static; _ }) when mem_field ~static "$key" x ->
      Flow_js.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, MultipleIndexers));
      x
    | Indexer (_, { Indexer.key; value; static; variance; _ }) ->
      let k = Anno.convert cx tparams_map key in
      let v = Anno.convert cx tparams_map value in
      let polarity = Anno.polarity variance in
      x
        |> add_field ~static "$key" (polarity, Annot k)
        |> add_field ~static "$value" (polarity, Annot v)
    | Property (loc, { Property.key; value; static; _method; optional; variance; }) ->
      if optional && _method
      then Flow_js.add_output cx Flow_error.(EInternal (loc, OptionalMethod));
      let polarity = Anno.polarity variance in
      Ast.Expression.Object.(match _method, key, value with
      | _, Property.Literal (loc, _), _
      | _, Property.PrivateName (loc, _), _
      | _, Property.Computed (loc, _), _ ->
          Flow_js.add_output cx (Flow_error.EIllegalName loc);
          x
      | true, Property.Identifier (_, name),
          Ast.Type.Object.Property.Init (_, Ast.Type.Function func) ->
          let fsig = Func_sig.convert cx tparams_map loc func in
          let append_method = match static, name with
          | false, "constructor" -> append_constructor
          | _ -> append_method ~static name
          in
          append_method fsig x

      | true, Property.Identifier _, _ ->
          Flow_js.add_output cx
            Flow_error.(EInternal (loc, MethodNotAFunction));
          x

      | false, Property.Identifier (_, name),
          Ast.Type.Object.Property.Init value ->
          let t = Anno.convert cx tparams_map value in
          let t = if optional then Type.optional t else t in
          add_field ~static name (polarity, Annot t) x

      (* unsafe getter property *)
      | _, Property.Identifier (_, name),
          Ast.Type.Object.Property.Get (_, func)
            when Context.enable_unsafe_getters_and_setters cx ->
          let fsig = Func_sig.convert cx tparams_map loc func in
          add_getter ~static name fsig x

      (* unsafe setter property *)
      | _, Property.Identifier (_, name),
          Ast.Type.Object.Property.Set (_, func)
            when Context.enable_unsafe_getters_and_setters cx ->
          let fsig = Func_sig.convert cx tparams_map loc func in
          add_setter ~static name fsig x

      | _, _, Ast.Type.Object.Property.Get _
      | _, _, Ast.Type.Object.Property.Set _ ->
          Flow_js.add_output cx
            Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyGetSet));
          x
      )

    | SpreadProperty (loc, _) ->
      Flow_js.add_output cx Flow_error.(EInternal (loc, InterfaceTypeSpread));
      x
  ) iface_sig properties in

  (* Structural interfaces never get a default constructor. Non-structural
   * (aka declare class) get a default constructor if they don't have a
   * constructor and won't inherit one from a super *)
  let inherits_constructor = extends <> [] || mixins <> [] in
  if structural || mem_constructor iface_sig || inherits_constructor
  then iface_sig
  else
    let reason = mk_reason RDefaultConstructor loc in
    add_default_constructor reason iface_sig
)

(* Processes the bodies of instance and static class members. *)
let toplevels cx ~decls ~stmts ~expr x =
  Env.in_lex_scope cx (fun () ->
    let new_entry t = Scope.Entry.new_var ~loc:(Type.loc_of_t t) t in

    let method_ this super f =
      let save_return = Abnormal.clear_saved Abnormal.Return in
      let save_throw = Abnormal.clear_saved Abnormal.Throw in
      f |> Func_sig.generate_tests cx (
        Func_sig.toplevels None cx this super ~decls ~stmts ~expr
      );
      ignore (Abnormal.swap_saved Abnormal.Return save_return);
      ignore (Abnormal.swap_saved Abnormal.Throw save_throw)
    in

    let field config this super _name (_, value) =
      match config, value with
      | Options.ESPROPOSAL_IGNORE, _ -> ()
      | _, Annot _ -> ()
      | _, Infer fsig -> method_ this super fsig
    in

    let this = SMap.find_unsafe "this" x.tparams_map in
    let static = Type.class_type this in

    (* Bind private fields to the environment *)
    let to_prop_map = fun x -> Context.make_property_map cx (SMap.map to_field x) in
    Env.bind_class cx x.id (to_prop_map x.instance.private_fields)
    (to_prop_map x.static.private_fields);

    x |> with_sig ~static:true (fun s ->
      (* process static methods and fields *)
      let this, super = new_entry static, new_entry s.super in
      iter_methods (method_ this super) s;
      let config = Context.esproposal_class_static_fields cx in
      SMap.iter (field config this super) s.fields;
      SMap.iter (field config this super) s.private_fields
    );

    x |> with_sig ~static:false (fun s ->
      (* process constructor *)
      begin
        (* When in a derived constructor, initialize this and super to undefined.
           For internal names, we can afford to initialize with undefined and
           fatten the declared type to allow undefined. This works because we
           always look up the flow-sensitive type of internals, and don't havoc
           them. However, the same trick wouldn't work for normal uninitialized
           locals, e.g., so it cannot be used in general to track definite
           assignments. *)
        let derived_ctor = Type.(match s.super with
          | DefT (_, ClassT (ObjProtoT _)) -> false
          | ObjProtoT _ -> false
          | _ -> true
        ) in
        let new_entry t =
          if derived_ctor then
            let open Type in
            let specific =
              DefT (replace_reason_const RUninitializedThis (reason_of_t this), VoidT)
            in
            Scope.Entry.new_var ~loc:(loc_of_t t) ~specific (Type.optional t)
          else
            new_entry t
        in
        let this, super = new_entry this, new_entry s.super in
        x.constructor |> List.iter (method_ this super)
      end;

      (* process instance methods and fields *)
      begin
        let this, super = new_entry this, new_entry s.super in
        iter_methods (method_ this super) s;
        let config = Context.esproposal_class_instance_fields cx in
        SMap.iter (field config this super) s.fields;
        SMap.iter (field config this super) s.private_fields;
      end
  ))

module This = struct
  let is_bound_to_empty x =
    let open Type in
    Flow.match_this_binding x.tparams_map
      (function DefT (_, EmptyT) -> true | _ -> false)

  exception FoundInClass
  class detector = object
    inherit Flow_ast_mapper.mapper as super

    method! generic_identifier_type (git: Loc.t Ast.Type.Generic.Identifier.t) =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified (_, "this") -> raise FoundInClass
      | _ -> super#generic_identifier_type git
  end

  let in_class c =
    try (new detector)#class_ c |> ignore; false
    with FoundInClass -> true
end
