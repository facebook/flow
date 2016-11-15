(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Anno = Type_annotation
module Ast = Spider_monkey_ast
module Flow = Flow_js

open Reason

type field = Type.t * Type.polarity * Ast.Expression.t option

type signature = {
  reason: reason;
  super: Type.t;
  fields: field SMap.t;
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
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: Func_sig.t list;
  static: signature;
  instance: signature;
}

let empty ?(structural=false) id reason tparams tparams_map super =
  let empty_sig reason super = {
    reason; super;
    fields = SMap.empty;
    methods = SMap.empty;
    getters = SMap.empty;
    setters = SMap.empty;
  } in
  let constructor = [] in
  let static =
    let super = Type.ClassT super in
    let reason = replace_reason (fun desc -> RStatics desc) reason in
    empty_sig reason super
  in
  let instance = empty_sig reason super in
  { id; structural; tparams; tparams_map; constructor; static; instance }

let map_sig ~static f s =
  if static
  then {s with static = f s.static}
  else {s with instance = f s.instance}

let with_sig ~static f s =
  if static then f s.static else f s.instance

let mutually f = (f ~static:true, f ~static:false)

let add_field name fld = map_sig (fun s -> {
  s with
  fields = SMap.add name fld s.fields;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

let add_constructor fsig s =
  {s with constructor = [fsig]}

let add_default_constructor reason s =
  let fsig = Func_sig.default_constructor reason in
  add_constructor fsig s

let append_constructor fsig s =
  {s with constructor = fsig::s.constructor}

let add_method name fsig = map_sig (fun s -> {
  s with
  methods = SMap.add name (Nel.one fsig) s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method name fsig = map_sig (fun s ->
  let methods = match SMap.get name s.methods with
  | Some fsigs -> SMap.add name (Nel.cons fsig fsigs) s.methods
  | None -> SMap.add name (Nel.one fsig) s.methods
  in
  {s with methods}
)

let add_getter name fsig = map_sig (fun s -> {
  s with
  getters = SMap.add name fsig s.getters;
  methods = SMap.remove name s.methods;
})

let add_setter name fsig = map_sig (fun s -> {
  s with
  setters = SMap.add name fsig s.setters;
  methods = SMap.remove name s.methods;
})

let mk_method cx ~expr x reason func =
  Func_sig.mk cx x.tparams_map ~expr reason func

let mk_field cx ~polarity x reason typeAnnotation value =
  let t = Anno.mk_type_annotation cx x.tparams_map reason typeAnnotation in
  (t, polarity, value)

let mem_constructor {constructor; _} = constructor <> []

let iter_methods f s =
  SMap.iter (fun _ -> Nel.iter f) s.methods;
  SMap.iter (fun _ -> f) s.getters;
  SMap.iter (fun _ -> f) s.setters

let subst_field cx map (t, polarity, value) =
  Flow.subst cx map t, polarity, value

let subst_sig cx map s = {
  reason = s.reason;
  super = Flow.subst cx map s.super;
  fields = SMap.map (subst_field cx map) s.fields;
  methods = SMap.map (Nel.map (Func_sig.subst cx map)) s.methods;
  getters = SMap.map (Func_sig.subst cx map) s.getters;
  setters = SMap.map (Func_sig.subst cx map) s.setters;
}

let generate_tests cx f x =
  let {tparams_map; constructor; static; instance; _} = x in
  Flow.generate_tests cx x.instance.reason x.tparams (fun map -> f {
    x with
    tparams_map = SMap.map (Flow.subst cx map) tparams_map;
    constructor = List.map (Func_sig.subst cx map) constructor;
    static = subst_sig cx map static;
    instance = subst_sig cx map instance;
  })

let elements ?constructor = with_sig (fun s ->
  let methods =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.map Type.(fun xs ->
      match Nel.rev_map Func_sig.methodtype xs with
      | t, [] -> t
      | t0, t1::ts -> IntersectionT (reason_of_t t0, InterRep.make t0 t1 ts)
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

  let fields = SMap.map (fun (t, polarity, _) ->
    Type.Field (t, polarity)
  ) s.fields in

  (* Treat getters and setters as fields *)
  let fields = SMap.union getters_and_setters fields in

  let methods = SMap.map (fun t ->
    Type.Field (t, Type.Positive)
  ) methods in

  (* Only un-initialized fields require annotations, so determine now
   * (syntactically) which fields have initializers *)
  let initialized_field_names =
    s.fields
    |> SMap.filter (fun _ (_, _, init_expr) -> init_expr <> None)
    |> SMap.keys
    |> Utils_js.set_of_list
  in

  initialized_field_names, fields, methods
)

let arg_polarities x =
  List.fold_left Type.(fun acc tp ->
    SMap.add tp.name tp.polarity acc
  ) SMap.empty x.tparams

let insttype ~static cx s =
  let class_id = if static then 0 else s.id in
  let constructor = if static then None else
    let ts = List.rev_map Func_sig.methodtype s.constructor in
    match ts with
    | [] -> None
    | [t] -> Some t
    | t0::t1::ts ->
      let open Type in
      let t = IntersectionT (reason_of_t t0, InterRep.make t0 t1 ts) in
      Some t
  in
  let inited_fields, fields, methods = elements ?constructor ~static s in
  { Type.
    class_id;
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
      Type.TypeAppT (self, tparams)
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

let check_super cx x =
  let x = remove_this x in
  let reason = x.instance.reason in
  mutually (fun ~static ->
    let super = with_sig ~static (fun s -> s.super) x in
    let insttype = insttype ~static cx x in
    Flow.flow cx (super, Type.SuperT (reason, insttype))
  ) |> ignore

(* TODO: Ideally we should check polarity for all class types, but this flag is
   flipped off for interface/declare class currently. *)
let classtype cx ?(check_polarity=true) x =
  let x = remove_this x in
  let {
    structural;
    tparams;
    static = {reason = sreason; super = ssuper; _};
    instance = {reason; super; _};
    _;
  } = x in
  let open Type in
  let sinsttype, insttype = mutually (insttype cx x) in
  let static = InstanceT (sreason, ObjProtoT.t, ssuper, sinsttype) in
  let this = InstanceT (reason, static, super, insttype) in
  (if check_polarity then Flow.check_polarity cx Positive this);
  let t = if structural then ClassT this else ThisClassT this in
  if tparams = [] then t else PolyT (tparams, t)

let mk_super cx tparams_map c targs = Type.(
  (* A super class must be parameterized by This, so that it can be
     specialized to this class and its subclasses when properties are looked
     up on their instances. *)
  let params = Anno.extract_type_param_instantiations targs in
  let this = SMap.find_unsafe "this" tparams_map in
  match params with
  | None
  | Some [] ->
      (* No type params, but `c` could still be a polymorphic class that must
         be implicitly instantiated. We need to do this before we try to
         this-specialize `c`. *)
      let reason = reason_of_t c in
      let c = Flow.mk_tvar_derivable_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (reason, reason, false, [], tvar))
      ) in
      ThisTypeAppT (c, this, [])
  | Some params ->
      ThisTypeAppT (c, this, List.map (Anno.convert cx tparams_map) params)
)

let mk_interface_super cx structural reason tparams_map = Type.(function
  | (None, None) ->
      ObjProtoT (locationless_reason RObjectClassName)
  | (None, _) ->
      assert false (* type args with no head expr *)
  | (Some id, targs) ->
      let desc, lookup_mode =
        if structural then "extends", Env.LookupMode.ForType
        else "mixins", Env.LookupMode.ForValue in
      let i = Anno.convert_qualification ~lookup_mode cx desc id in
      if structural then
        let params = Anno.extract_type_param_instantiations targs in
        Anno.mk_nominal_type cx reason tparams_map (i, params)
      else mk_super cx tparams_map i targs
)

let mk_extends cx tparams_map ~expr = Type.(function
  | (None, None) ->
      ObjProtoT (locationless_reason RObjectClassName)
  | (None, _) ->
      assert false (* type args with no head expr *)
  | (Some e, targs) ->
      let c = expr cx e in
      mk_super cx tparams_map c targs
)

let mk_mixins cx reason tparams_map = Type.(function
  | (None, None) ->
      ObjProtoT (locationless_reason RObjectClassName)
  | (None, _) ->
      assert false (* type args with no head expr *)
  | (Some id, targs) ->
      let i =
        let lookup_mode = Env.LookupMode.ForValue in
        Anno.convert_qualification ~lookup_mode cx "mixins" id
      in
      let props_bag = Flow.mk_tvar_derivable_where cx reason (fun tvar ->
        Flow.flow cx (i, MixinT (reason, tvar))
      ) in
      mk_super cx tparams_map props_bag targs
)

(* Process a class definition, returning a (polymorphic) class type. A class
   type is a wrapper around an instance type, which contains types of instance
   members, a pointer to the super instance type, and a container for types of
   static members. The static members can be thought of as instance members of a
   "metaclass": thus, the static type is itself implemented as an instance
   type. *)
let mk cx loc reason self ~expr = Ast.Class.(
  let warn_or_ignore_decorators cx = function
  | [] -> ()
  | (start_loc, _)::ds ->
    let loc = List.fold_left (fun start_loc (end_loc, _) ->
      Loc.btwn start_loc end_loc
    ) start_loc ds in
    Flow_error.warn_or_ignore_decorators cx loc
  in

  fun {
    body = (_, { Body.body = elements });
    superClass;
    typeParameters;
    superTypeParameters;
    implements;
    classDecorators;
    _;
  } ->

  warn_or_ignore_decorators cx classDecorators;

  (* TODO *)
  if implements <> []
  then Flow_error.(add_output cx (EUnsupportedSyntax (loc, Implements)))
  else ();

  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx typeParameters
  in

  let tparams, tparams_map =
    add_this self cx reason tparams tparams_map
  in

  let class_sig =
    let super =
      mk_extends cx tparams_map ~expr (superClass, superTypeParameters)
    in
    let id = Flow.mk_nominal cx in
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
      let reason = replace_reason_const RConstructor reason in
      add_default_constructor reason class_sig
  in

  (* All classes have a static "name" property. *)
  let class_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (t, Type.Neutral, None) class_sig
  in

  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  List.fold_left (fun c -> function
    (* instance and static methods *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_, name);
        value = (_, func);
        kind;
        static;
        decorators;
      }) ->

      warn_or_ignore_decorators cx decorators;

      Ast.Class.Method.(match kind with
      | Get | Set -> Flow_error.warn_unsafe_getters_setters cx loc
      | _ -> ());

      let method_desc, add = match kind with
      | Method.Constructor ->
          RConstructor,
          add_constructor
      | Method.Method ->
          RProperty (Some name),
          add_method ~static name
      | Method.Get ->
          RProperty (Some name),
          add_getter ~static name
      | Method.Set ->
          RProperty (Some name),
          add_setter ~static name
      in
      let reason = mk_reason method_desc loc in
      let method_sig = mk_method cx ~expr c reason func in
      add method_sig c

    (* fields *)
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier (_, name);
        typeAnnotation;
        value;
        static;
        variance;
        _;
      }) ->
        if value <> None
        then Flow_error.warn_or_ignore_class_properties cx ~static loc;

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
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, ClassPropertyLiteral)));
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
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, ClassPropertyComputed)));
        c
  ) class_sig elements
)

let rec extract_extends cx structural = function
  | [] -> [None,None]
  | [_, {Ast.Type.Generic.id; typeParameters}] ->
      [Some id, typeParameters]
  | (loc, {Ast.Type.Generic.id; typeParameters})::others ->
      if structural
      then (Some id, typeParameters)::(extract_extends cx structural others)
      else (
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, ClassExtendsMultiple)));
        []
      )

let extract_mixins _cx =
  List.map (fun (_, {Ast.Type.Generic.id; typeParameters}) ->
    (Some id, typeParameters)
  )

let mk_interface cx loc reason structural self = Ast.Statement.(
  fun { Interface.
    typeParameters;
    body = (_, { Ast.Type.Object.properties; indexers; callProperties; _ });
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
    let extends = extract_extends cx structural extends in
    let mixins = extract_mixins cx mixins in
    let super_reason = replace_reason (fun desc -> RSuperOf desc) reason in
    (* mixins override extends *)
    let interface_supers =
      List.map (mk_mixins cx super_reason tparams_map) mixins @
      List.map (mk_interface_super cx structural super_reason tparams_map)
        extends
    in
    let super = Type.(match interface_supers with
      | [] -> AnyT.t
      | [t] -> t
      | t0::t1::ts -> IntersectionT (super_reason, InterRep.make t0 t1 ts)
    ) in
    empty ~structural id reason tparams tparams_map super
  in

  let iface_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (t, Type.Neutral, None) iface_sig
  in

  let iface_sig = List.fold_left (
    fun s (loc, {Ast.Type.Object.Property.
      key; value; static; _method; optional; variance; _
    }) ->
    if optional && _method
    then Flow_error.(add_output cx (EInternal (loc, OptionalMethod)));
    let polarity = Anno.polarity variance in
    Ast.Expression.Object.(match _method, key with
    | _, Property.Literal (loc, _)
    | _, Property.Computed (loc, _) ->
        Flow_error.(add_output cx (EIllegalName loc));
        s
    | true, Property.Identifier (_, name) ->
        (match value with
        | _, Ast.Type.Function func ->
          let fsig = Func_sig.convert cx tparams_map loc func in
          let append_method = match static, name with
          | false, "constructor" -> append_constructor
          | _ -> append_method ~static name
          in
          append_method fsig s
        | _ ->
          Flow_error.(add_output cx (EInternal (loc, MethodNotAFunction)));
          s)
    | false, Property.Identifier (_, name) ->
        let t = Anno.convert cx tparams_map value in
        let t = if optional then Type.OptionalT t else t in
        add_field ~static name (t, polarity, None) s)
  ) iface_sig properties in

  let iface_sig = match indexers with
    | [] -> iface_sig
    | (_, {Ast.Type.Object.Indexer.key; value; static; variance; _})::rest ->
      (* TODO? *)
      List.iter (fun (indexer_loc, _) ->
        Flow_error.(add_output cx
          (EUnsupportedSyntax (indexer_loc, MultipleIndexers)))
      ) rest;
      let k = Anno.convert cx tparams_map key in
      let v = Anno.convert cx tparams_map value in
      let polarity = Anno.polarity variance in
      iface_sig
        |> add_field ~static "$key" (k, polarity, None)
        |> add_field ~static "$value" (v, polarity, None)
  in

  let iface_sig = List.fold_left (
    fun s (loc, {Ast.Type.Object.CallProperty.value = (_, func); static}) ->
    let fsig = Func_sig.convert cx tparams_map loc func in
    append_method ~static "$call" fsig s
  ) iface_sig callProperties in

  if mem_constructor iface_sig
  then iface_sig
  else
    let reason = mk_reason RConstructor loc in
    add_default_constructor reason iface_sig
)

(* Processes the bodies of instance and static class members. *)
let toplevels cx ~decls ~stmts ~expr x =
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

  let field config this super name (field_t, _, value) =
    match config, value with
    | Options.ESPROPOSAL_IGNORE, _ -> ()
    | _, None -> ()
    | _, Some ((loc, _) as expr) ->
      let init =
        let reason = mk_reason (RFieldInitializer name) loc in
        Func_sig.field_initializer x.tparams_map reason expr field_t
      in
      method_ this super init
  in

  let this = SMap.find_unsafe "this" x.tparams_map in
  let static = Type.ClassT this in

  x |> with_sig ~static:true (fun s ->
    (* process static methods and fields *)
    let this, super = new_entry static, new_entry s.super in
    iter_methods (method_ this super) s;
    let config = Context.esproposal_class_static_fields cx in
    SMap.iter (field config this super) s.fields
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
        | ClassT (ObjProtoT _) -> false
        | ObjProtoT _ -> false
        | _ -> true
      ) in
      let new_entry t =
        if derived_ctor then
          let open Type in
          let specific =
            VoidT (replace_reason_const RUninitializedThis (reason_of_t this))
          in
          Scope.Entry.new_var ~loc:(loc_of_t t) ~specific (OptionalT t)
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
      SMap.iter (field config this super) s.fields
    end
  )
