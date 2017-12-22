(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Anno = Type_annotation
module Flow = Flow_js

open Reason

type field = fld * Type.polarity
and fld = Type.Property.kind * Loc.t option * field_t
and field_t = Annot of Type.t | Infer of Func_sig.t

type meth = Type.Property.kind * Loc.t option * Func_sig.t

type signature = {
  reason: reason;
  super: Type.t;
  fields: field SMap.t;
  private_fields: field SMap.t;
  abstracts: meth SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: meth Nel.t SMap.t;
  getters: meth SMap.t;
  setters: meth SMap.t;
}

type t = {
  id: int;
  structural: bool;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  implements: Type.t list;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: meth list;
  static: signature;
  instance: signature;
}

type super =
  | Interface of {
      extends: Type.t list;
      callable: bool;
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
    abstracts = SMap.empty;
    methods = SMap.empty;
    getters = SMap.empty;
    setters = SMap.empty;
  } in
  let structural, super, ssuper, implements =
    let open Type in
    let super_reason = replace_reason (fun d -> RSuperOf d) reason in
    match super with
    | Interface {extends; callable} ->
      let super, ssuper = match extends with
      | [] ->
        ObjProtoT super_reason, ObjProtoT super_reason
      | t0::ts ->
        (* Interfaces support multiple inheritance. *)
        let super = match ts with
        | [] -> t0
        | t1::ts -> DefT (super_reason, IntersectionT (InterRep.make t0 t1 ts))
        in
        super, nonabstract_class_type super
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
      true, super, ssuper, []
    | Class {extends; mixins; implements} ->
      (* Class statics inherit properties from the function prototype, like
         bind, call, and apply. Despite this, classes are generally not callable
         without new (exceptions include cast functions like Number). *)
      let super, ssuper = match extends with
      | Explicit t -> t, nonabstract_class_type t
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

let map_third f (e1, e2, e3) = (e1, e2, f e3)

let map_sig ~static f s =
  if static
  then {s with static = f s.static}
  else {s with instance = f s.instance}

let with_sig ~static f s =
  if static then f s.static else f s.instance

let add_private_field name fld polarity = map_sig (fun s -> {
  s with
  private_fields = SMap.add name (fld, polarity) s.private_fields;
})

let add_constructor meth s = {
  s with constructor = [map_third Func_sig.to_ctor_sig meth]
}

let add_default_constructor reason s =
  let kind = Type.Property.Member ("constructor", loc_of_reason reason) in
  let fsig = Func_sig.default_constructor reason in
  add_constructor (kind, None, fsig) s

let append_constructor meth s = {
  s with constructor = (map_third Func_sig.to_ctor_sig meth)::s.constructor
}

let add_field ~static name fld polarity = map_sig ~static (fun s -> {
  s with
  fields = SMap.add name (fld, polarity) s.fields;
  methods = if static then SMap.remove name s.methods else s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_method ~static name meth = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.add name (Nel.one meth) s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_abstract ~static name meth = map_sig ~static (fun s -> {
  s with
  abstracts = SMap.add name meth s.abstracts;
})

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method ~static name meth = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = (
    match SMap.get name s.methods with
    | Some fsigs -> SMap.add name (Nel.cons meth fsigs) s.methods
    | None -> SMap.add name (Nel.one meth) s.methods
  );
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_getter ~static name meth = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.remove name s.methods;
  getters = SMap.add name meth s.getters;
})

let add_setter ~static name meth = map_sig ~static (fun s -> {
  s with
  fields = if static then SMap.remove name s.fields else s.fields;
  methods = SMap.remove name s.methods;
  setters = SMap.add name meth s.setters;
})

let mask { fields; methods; getters; _ } =
  let add_keys map = SMap.fold (fun k _ acc -> SSet.add k acc) map in
  SSet.empty |> add_keys fields |> add_keys methods |> add_keys getters

let abstracts { abstracts; _ } =
  abstracts |> SMap.map (fun (_, _, fsig) -> Func_sig.reason_of_t fsig)

let mk_abstract cx x loc func =
  let fsig = Func_sig.convert cx x.tparams_map loc func in
  Func_sig.replace_reason_const (RPropertyDef RAbstractMethodDef) fsig

let mk_method cx ~expr x loc func =
  Func_sig.mk cx x.tparams_map ~expr loc func

let mk_field cx x reason typeAnnotation = function
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
let subst_field cx map (fld, polarity) =
  map_third (function
    | Annot t -> Annot (Flow.subst cx map t)
    | Infer fsig -> Infer (Func_sig.subst cx map fsig)
  ) fld, polarity

let subst_sig cx map s = {
  reason = s.reason;
  super = Flow.subst cx map s.super;
  fields = SMap.map (subst_field cx map) s.fields;
  private_fields = SMap.map (subst_field cx map) s.private_fields;
  abstracts = SMap.map (map_third (Func_sig.subst cx map)) s.abstracts;
  methods = SMap.map (Nel.map (map_third (Func_sig.subst cx map))) s.methods;
  getters = SMap.map (map_third (Func_sig.subst cx map)) s.getters;
  setters = SMap.map (map_third (Func_sig.subst cx map)) s.setters;
}

let generate_tests cx f x =
  Flow.generate_tests cx x.instance.reason x.tparams (fun map -> f {
    id = x.id;
    structural = x.structural;
    tparams = x.tparams;
    tparams_map = SMap.map (Flow.subst cx map) x.tparams_map;
    implements = List.map (Flow.subst cx map) x.implements;
    constructor = List.map (map_third (Func_sig.subst cx map)) x.constructor;
    static = subst_sig cx map x.static;
    instance = subst_sig cx map x.instance;
  })

let to_field (fld, polarity) =
  let triple = map_third (function
    | Annot t -> t
    | Infer fsig -> Func_sig.gettertype fsig
  ) fld in
  Type.Field (triple, polarity)

let elements cx ?constructor s =
  let open Type in

  let nonabstracts =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.map (fun xs ->
      match Nel.rev_map (map_third (Func_sig.methodtype cx)) xs with
      | x, [] -> x
      | (loc0, key_loc0, t0), (_loc1, _key_loc1, t1)::ts ->
          let ts = List.map (fun (_loc, _key_loc, t) -> t) ts in
          let reason = reason_of_t t0 in
          loc0, key_loc0, DefT (reason, IntersectionT (InterRep.make t0 t1 ts))
    ) s.methods
  in

  (* Re-add the constructor as a method. *)
  let nonabstracts = match constructor with
  | Some t -> SMap.add "constructor" t nonabstracts
  | None -> nonabstracts
  in

  (* Sort getters and setters into property types. *)
  let getters = SMap.map (map_third Func_sig.gettertype) s.getters in
  let setters = SMap.map (map_third Func_sig.settertype) s.setters in
  let getters_and_setters = SMap.merge (fun _ getter setter ->
    match getter, setter with
    | Some get, Some set ->Some (GetSet (get, set))
    | Some get, None -> Some (Get get)
    | None, Some set -> Some (Set set)
    | _ -> None
  ) getters setters in

  (* Treat getters and setters as fields *)
  let fields = SMap.union getters_and_setters (SMap.map to_field s.fields) in

  let methods =
    s.abstracts
      |> SMap.map (map_third (Func_sig.methodtype cx))
      |> SMap.map (fun abstract -> Abstract abstract)
    |> SMap.fold (fun x triple ms -> SMap.add x (Method triple) ms) nonabstracts
  in

  (* Only un-initialized fields require annotations, so determine now
   * (syntactically) which fields have initializers *)
  let initialized_field_names = SMap.fold (fun x ((_, _, field), _) acc ->
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
  let inited_fields, fields, methods = elements cx s in
  let props = SMap.union fields methods
    ~combine:(fun _ _ ->
      Utils_js.assert_false (Utils_js.spf
        "static fields and methods must be disjoint: %s"
        (Debug_js.dump_reason cx s.reason)))
  in
  (* Statics are not exact, because we allow width subtyping between them.
     Specifically, given class A and class B extends A, Class<B> <: Class<A>. *)
  let static =
    Obj_type.mk_with_proto cx s.reason ~props s.super
      ~sealed:true ~exact:false
  in
  let open Type in
  match static with
  | DefT (_, ObjT o) -> inited_fields, o
  | _ -> failwith "statics must be an ObjT"

let insttype cx ~initialized_static_field_names s =
  let constructor =
    let ts = List.rev_map (map_third (Func_sig.methodtype cx)) s.constructor in
    match ts with
    | [] -> None
    | [x] -> Some x
    | (loc0, key_loc0, t0)::(_loc1, _key_loc1, t1)::ts ->
      let ts = List.map (fun (_loc, _key_loc, t) -> t) ts in
      let open Type in
      let t = DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts)) in
      Some (loc0, key_loc0, t)
  in
  let inited_fields, fields, methods = elements cx ?constructor s.instance in
  { Type.
    class_id = s.id;
    type_args = s.tparams_map;
    arg_polarities = arg_polarities s;
    fields_tmap = Context.make_property_map cx fields;
    initialized_field_names = inited_fields;
    initialized_static_field_names;
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
  let initialized_static_field_names, static_objtype = statictype cx x.static in
  let insttype = insttype cx ~initialized_static_field_names x in
  let static = DefT (sreason, ObjT static_objtype) in
  DefT (reason, InstanceT (static, super, implements, insttype))

let check_implements cx def_reason x =
  let this = thistype cx x in
  let reason = x.instance.reason in
  let open Type in
  List.iter (fun i ->
    let use_op = Op (ClassImplementsCheck {
      def = def_reason;
      name = reason;
      implements = reason_of_t i;
    }) in
    Flow.flow cx (i, ImplementsT (use_op, this))
  ) x.implements

let check_super cx def_reason x =
  let x = remove_this x in
  let reason = x.instance.reason in
  let open Type in
  let initialized_static_field_names, static_objtype = statictype cx x.static in
  let insttype = insttype cx ~initialized_static_field_names x in
  let use_op = Op (ClassExtendsCheck {
    def = def_reason;
    name = reason;
    extends = reason_of_t x.instance.super;
  }) in
  Flow.flow cx (x.static.super, SuperT (use_op, reason, DerivedStatics static_objtype));
  Flow.flow cx (x.instance.super, SuperT (use_op, reason, DerivedInstance insttype))

(* TODO: Ideally we should check polarity for all class types, but this flag is
   flipped off for interface/declare class currently. *)
let classtype cx ?(check_polarity=true) x =
  let this = thistype cx x in
  let { structural; tparams;
        instance = { reason; _ };
        _
      } = remove_this x in
  let open Type in
  (if check_polarity then Flow.check_polarity cx Positive this);
  let t = if structural then nonabstract_class_type this else
    let super = x.instance.super in
    let masks = (mask x.static, mask x.instance) in
    let local_abstracts = (abstracts x.static, abstracts x.instance) in
    let reason = replace_reason (fun desc -> RAbstractsOf desc) reason in
    let abstracts = Tvar.mk_where cx reason (fun tvar ->
      Flow.flow cx (super, AccAbstractsT (reason, masks, local_abstracts, tvar))
    ) in
    this_class_type this abstracts
  in
  poly_type (Context.make_nominal cx) tparams t

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
      let c = Tvar.mk_derivable_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (unknown_use, reason, reason, None, None, tvar))
      ) in
      this_typeapp c this None
  | Some params ->
      let tparams = List.map (Anno.convert cx tparams_map) params in
      this_typeapp c this (Some tparams)
)

let mk_interface_super cx tparams_map (loc, {Ast.Type.Generic.id; typeParameters}) =
  let r = mk_reason (RCustom (Anno.qualified_name id)) loc in
  let lookup_mode = Env.LookupMode.ForType in
  let i = Anno.convert_qualification ~lookup_mode cx "extends" id in
  let params = Anno.extract_type_param_instantiations typeParameters in
  Anno.mk_nominal_type cx r tparams_map (i, params)

let mk_extends cx tparams_map ~expr = function
  | None, None -> Implicit { null = false }
  | None, _ -> assert false (* type args with no head expr *)
  | Some e, targs ->
    let c = expr cx e in
    Explicit (mk_super cx tparams_map c targs)

let mk_mixin cx tparams_map (r, id, targs) =
  let i =
    let lookup_mode = Env.LookupMode.ForValue in
    Anno.convert_qualification ~lookup_mode cx "mixins" id
  in
  let props_bag = Tvar.mk_derivable_where cx r Type.(fun tvar ->
    Flow.flow cx (i, MixinT (unknown_use, r, tvar))
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
    Flow.add_output cx (Flow_error.EExperimentalDecorators loc)

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
    Flow.add_output cx
      (Flow_error.EExperimentalClassProperties (loc, static))

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
    let id = Context.make_nominal cx in
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
    let name = "name" in
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let kind = Type.Property.Member (name, loc_of_reason reason) in
    let fld = (kind, None, Annot (Type.StrT.why reason)) in
    add_field ~static:true name fld Type.Neutral class_sig
  in

  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  List.fold_left Ast.Class.(fun c -> function
    (* abstract instance and abstract static methods *)
    | Body.AbstractMethod (loc, {
        AbstractMethod.key = (id_loc, name);
        value = (_, func);
        static;
      }) ->

      let fsig = mk_abstract cx c loc func in
      let meth = (Type.Property.Explicit loc, Some id_loc, fsig) in
      add_abstract ~static name meth c

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
        Method.key = Ast.Expression.Object.Property.Identifier (id_loc, name);
        value = (_, func);
        kind;
        static;
        decorators;
      }) ->

      Type_inference_hooks_js.dispatch_method_decl_hook cx name id_loc;
      warn_or_ignore_decorators cx decorators;

      Ast.Class.Method.(match kind with
      | Get | Set -> Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc)
      | _ -> ());

      let fsig = mk_method cx ~expr c loc func in
      let meth = (Type.Property.Explicit loc, Some id_loc, fsig) in
      begin match kind with
      | Method.Constructor -> add_constructor meth c
      | Method.Method -> add_method ~static name meth c
      | Method.Get -> add_getter ~static name meth c
      | Method.Set -> add_setter ~static name meth c
      end

    (* fields *)
    | Body.PrivateField(loc, {
        PrivateField.key = (_, (id_loc, name));
        typeAnnotation;
        value;
        static;
        variance;
        _;
      }) ->
        Type_inference_hooks_js.dispatch_prop_decl_hook cx name id_loc;

        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let fld =
          let reason = mk_reason (RProperty (Some name)) loc in
          let t = mk_field cx c reason typeAnnotation value in
          (Type.Property.Explicit loc, Some id_loc, t)
        in
        let polarity = Anno.polarity variance in
        add_private_field ~static name fld polarity c

    | Body.Property (loc, {
      Property.key = Ast.Expression.Object.Property.Identifier (id_loc, name);
        typeAnnotation;
        value;
        static;
        variance;
        _;
      }) ->
        Type_inference_hooks_js.dispatch_prop_decl_hook cx name id_loc;

        if value <> None
        then warn_or_ignore_class_properties cx ~static loc;

        let fld =
          let reason = mk_reason (RProperty (Some name)) loc in
          let t = mk_field cx c reason typeAnnotation value in
          (Type.Property.Explicit loc, Some id_loc, t)
        in
        let polarity = Anno.polarity variance in
        add_field ~static name fld polarity c

    (* literal LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Literal _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Literal _;
        _
      }) ->
        Flow.add_output cx
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
        Flow.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, ClassPropertyComputed));
        c
  ) class_sig elements

let extract_mixins _cx =
  List.map (fun (loc, {Ast.Type.Generic.id; typeParameters}) ->
    let name = Anno.qualified_name id in
    let r = mk_reason (RType name) loc in
    r, id, typeParameters
  )

let is_object_builtin_libdef (loc, name) =
  name = "Object" &&
  match Loc.source loc with
  | None -> false
  | Some source -> File_key.is_lib_file source

let add_interface_properties cx properties s =
  let { tparams_map; _ } = s in
  List.fold_left Ast.Type.Object.(fun x -> function
    | CallProperty (loc, { CallProperty.value = (_, func); static }) ->
      let fsig = Func_sig.convert cx tparams_map loc func in
      let meth = (Type.Property.Explicit loc, None, fsig) in
      append_method ~static "$call" meth x
    | Indexer (loc, { Indexer.static; _ }) when mem_field ~static "$key" x ->
      Flow.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, MultipleIndexers));
      x
    | Indexer (loc, { Indexer.key; value; static; variance; _ }) ->
      let k = Anno.convert cx tparams_map key in
      let v = Anno.convert cx tparams_map value in
      let polarity = Anno.polarity variance in
      let kind = Type.Property.Explicit loc in
      x
        |> add_field ~static "$key" (kind, None, Annot k) polarity
        |> add_field ~static "$value" (kind, None, Annot v) polarity
    | Property (loc, { Property.
        key; value; optional; abstract; static; _method; variance; }) ->
      if optional && _method
      then Flow.add_output cx Flow_error.(EInternal (loc, OptionalMethod));
      let polarity = Anno.polarity variance in
      Ast.Expression.Object.(match _method, key, value with
      | _, Property.Literal (loc, _), _
      | _, Property.PrivateName (loc, _), _
      | _, Property.Computed (loc, _), _ ->
          Flow.add_output cx (Flow_error.EIllegalName loc);
          x
      | true, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Init (_, Ast.Type.Function func) ->
          let fsig =
            if abstract
            then mk_abstract cx x loc func
            else Func_sig.convert cx tparams_map loc func
          in
          let meth = (Type.Property.Explicit loc, Some id_loc, fsig) in
          begin match abstract, static, name with
          | true, _, _ -> add_abstract ~static name meth x
          | false, false, "constructor" -> append_constructor meth x
          | _ -> append_method ~static name meth x
          end

      | true, Property.Identifier _, _ ->
          Flow.add_output cx
            Flow_error.(EInternal (loc, MethodNotAFunction));
          x

      | false, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Init value ->
          assert(not abstract);
          let t = Anno.convert cx tparams_map value in
          let t = if optional then Type.optional t else t in
          let fld = (Type.Property.Explicit loc, Some id_loc, Annot t) in
          add_field ~static name fld polarity x

      (* unsafe getter property *)
      | _, Property.Identifier (id_loc, name),
        Ast.Type.Object.Property.Get (_, func) ->
          assert(not abstract);
          Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
          let fsig = Func_sig.convert cx tparams_map loc func in
          let meth = (Type.Property.Explicit loc, Some id_loc, fsig) in
          add_getter ~static name meth x

      (* unsafe setter property *)
      | _, Property.Identifier (id_loc, name),
        Ast.Type.Object.Property.Set (_, func) ->
          assert(not abstract);
          Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
          let fsig = Func_sig.convert cx tparams_map loc func in
          let meth = (Type.Property.Explicit loc, Some id_loc, fsig) in
          add_setter ~static name meth x
      )

    | SpreadProperty (loc, _) ->
      Flow.add_output cx Flow_error.(EInternal (loc, InterfaceTypeSpread));
      x
  ) s properties

let of_interface cx reason { Ast.Statement.Interface.
  typeParameters;
  body = (_, { Ast.Type.Object.properties; _ });
  extends;
  _;
} =
  let self = Tvar.mk cx reason in

  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx typeParameters in

  let iface_sig =
    let id = Context.make_nominal cx in
    let extends = List.map (mk_interface_super cx tparams_map) extends in
    let super =
      let callable = List.exists Ast.Type.Object.(function
        | CallProperty (_, { CallProperty.static; _ }) -> not static
        | _ -> false
      ) properties in
      Interface { extends; callable }
    in
    empty id reason tparams tparams_map super
  in

  let iface_sig =
    let name = "name" in
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let kind = Type.Property.Member (name, loc_of_reason reason) in
    let fld = (kind, None, Annot (Type.StrT.why reason)) in
    add_field ~static:true name fld Type.Neutral iface_sig
  in

  let iface_sig = add_interface_properties cx properties iface_sig in

  iface_sig, self


let of_declare_class cx reason { Ast.Statement.DeclareClass.
  id = ident;
  typeParameters;
  body = (_, { Ast.Type.Object.properties; _ });
  extends;
  mixins;
  _;
} =
  let self = Tvar.mk cx reason in

  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx typeParameters in

  let tparams, tparams_map = add_this self cx reason tparams tparams_map in

  let iface_sig =
    let id = Context.make_nominal cx in
    let extends =
      match extends with
      | Some (loc, {Ast.Type.Generic.id; typeParameters}) ->
        let lookup_mode = Env.LookupMode.ForValue in
        let i = Anno.convert_qualification ~lookup_mode cx "mixins" id in
        let super = mk_super cx tparams_map i typeParameters in
        if mixins <> [] then Type.(
          let super = Flow.reposition cx loc super in
          let kind = NonabstractClass in
          Flow.flow cx (super, AssertNonabstractT (unknown_use, reason, kind))
        );
        Explicit super
      | None -> Implicit { null = is_object_builtin_libdef ident }
    in
    let mixins =
      mixins
      |> extract_mixins cx
      |> List.map (mk_mixin cx tparams_map)
    in
    mixins |> List.iter Type.(fun mixin ->
      let kind = NonabstractClass in
      Flow.flow cx (mixin, AssertNonabstractT (unknown_use, reason, kind))
    );
    let super = Class { extends; mixins; implements = [] } in
    empty id reason tparams tparams_map super
  in

  let iface_sig =
    let name = "name" in
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let kind = Type.Property.Member (name, loc_of_reason reason) in
    let fld = (kind, None, Annot (Type.StrT.why reason)) in
    add_field ~static:true name fld Type.Neutral iface_sig
  in

  let iface_sig = add_interface_properties cx properties iface_sig in

  (* Add a default constructor if we don't have a constructor and won't inherit one from a super *)
  let iface_sig =
    if mem_constructor iface_sig || extends <> None || mixins <> [] then
      iface_sig
    else
      let reason = replace_reason_const RDefaultConstructor reason in
      add_default_constructor reason iface_sig
  in
  iface_sig, self


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

    let field config this super _name ((_, _, value), _) =
      match config, value with
      | Options.ESPROPOSAL_IGNORE, _ -> ()
      | _, Annot _ -> ()
      | _, Infer fsig -> method_ this super fsig
    in

    let this = SMap.find_unsafe "this" x.tparams_map in
    let static = Type.nonabstract_class_type this in

    (* Bind private fields to the environment *)
    let to_prop_map = fun x -> Context.make_property_map cx (SMap.map to_field x) in
    Env.bind_class cx x.id (to_prop_map x.instance.private_fields)
    (to_prop_map x.static.private_fields);

    x |> with_sig ~static:true (fun s ->
      (* process static methods and fields *)
      let this, super = new_entry static, new_entry s.super in
      iter_methods (fun (_, _, fsig) -> method_ this super fsig) s;
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
          | DefT (_, ClassT (ObjProtoT _, _)) -> false
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
        x.constructor |> List.iter (fun (_, _, fsig) -> method_ this super fsig)
      end;

      (* process instance methods and fields *)
      begin
        let this, super = new_entry this, new_entry s.super in
        iter_methods (fun (_, _, fsig) -> method_ this super fsig) s;
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
