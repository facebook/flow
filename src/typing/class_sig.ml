(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js

open Reason

type set_asts =
  (Loc.t, Loc.t * Type.t) Ast.Function.body option *
  (Loc.t, Loc.t * Type.t) Ast.Expression.t option
  -> unit

type field =
  | Annot of Type.t
  | Infer of Func_sig.t * set_asts

type field' = Loc.t option * Type.polarity * field

type func_info = Loc.t option * Func_sig.t * set_asts

type signature = {
  reason: reason;
  fields: field' SMap.t;
  private_fields: field' SMap.t;
  proto_fields: field' SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: func_info Nel.t SMap.t;
  getters: func_info SMap.t;
  setters: func_info SMap.t;
  calls: Type.t list;
  call_deprecated: Type.t option;
}

type t = {
  id: int;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  super: super;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: func_info list;
  static: signature;
  instance: signature;
}

and super =
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
  let empty_sig reason = {
    reason;
    fields = SMap.empty;
    private_fields = SMap.empty;
    proto_fields = SMap.empty;
    methods = SMap.empty;
    getters = SMap.empty;
    setters = SMap.empty;
    calls = [];
    call_deprecated = None;
  } in
  let constructor = [] in
  let static =
    let reason = replace_reason (fun desc -> RStatics desc) reason in
    empty_sig reason
  in
  let instance = empty_sig reason in
  { id; tparams; tparams_map; super; constructor; static; instance }

let structural x =
  match x.super with
  | Interface _ -> true
  | Class _ -> false

let map_sig ~static f s =
  if static
  then {s with static = f s.static}
  else {s with instance = f s.instance}

let with_sig ~static f s =
  if static then f s.static else f s.instance

let add_private_field name loc polarity field = map_sig (fun s -> {
  s with
  private_fields = SMap.add name (Some loc, polarity, field) s.private_fields;
})

let add_constructor loc fsig ?(set_asts=ignore) s =
  {s with constructor = [loc, Func_sig.to_ctor_sig fsig, set_asts]}

let add_default_constructor reason s =
  let fsig = Func_sig.default_constructor reason in
  add_constructor None fsig s

let append_constructor loc fsig ?(set_asts=ignore) s =
  {s with constructor = (loc, Func_sig.to_ctor_sig fsig, set_asts)::s.constructor}

let add_field' ~static name fld x =
  let flat = static || structural x in
  map_sig ~static (fun s -> {
    s with
    fields = SMap.add name fld s.fields;
    proto_fields = if flat then SMap.remove name s.proto_fields else s.proto_fields;
    methods = if flat then SMap.remove name s.methods else s.methods;
    getters = if flat then SMap.remove name s.getters else s.getters;
    setters = if flat then SMap.remove name s.setters else s.setters;
  }) x

let add_field ~static name loc polarity field x =
  add_field' ~static name (Some loc, polarity, field) x

let add_indexer ~static polarity ~key ~value x =
  let kloc, k = key in
  let vloc, v = value in
  x |> add_field ~static "$key" kloc polarity (Annot k)
    |> add_field ~static "$value" vloc polarity (Annot v)

let add_name_field x =
  let r = replace_reason (fun desc -> RNameProperty desc) x.instance.reason in
  let t = Type.StrT.why r in
  add_field' ~static:true "name" (None, Type.Neutral, Annot t) x

let add_proto_field name loc polarity field x =
  map_sig ~static:false (fun s -> {
    s with
    proto_fields = SMap.add name (Some loc, polarity, field) s.proto_fields;
    methods = SMap.remove name s.methods;
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

let add_method ~static name loc fsig ?(set_asts=ignore) x =
  let flat = static || structural x in
  let func_info = Some loc, fsig, set_asts in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.add name (Nel.one func_info) s.methods;
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method ~static name loc fsig ?(set_asts=ignore) x =
  let flat = static || structural x in
  let func_info = Some loc, fsig, set_asts in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = (
      match SMap.get name s.methods with
      | Some fsigs -> SMap.add name (Nel.cons func_info fsigs) s.methods
      | None -> SMap.add name (Nel.one func_info) s.methods
    );
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

let append_call ~static t = map_sig ~static (fun s ->
  (* Note that $call properties always override the call property syntax.
     As before, if both are present, the $call property is used and the call
     property is ignored. *)
  match s.call_deprecated with
  | None -> { s with calls = t :: s.calls }
  | Some _ -> s
)

let add_call_deprecated ~static t = map_sig ~static (fun s ->
  (* Note that $call properties always override the call property syntax.
     As before, if both are present, the $call property is used and the call
     property is ignored. *)
  { s with call_deprecated = Some t; calls = [] }
)

let add_getter ~static name loc fsig ?(set_asts=ignore) x =
  let flat = static || structural x in
  let func_info = Some loc, fsig, set_asts in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.remove name s.methods;
    getters = SMap.add name func_info s.getters;
  }) x

let add_setter ~static name loc fsig ?(set_asts=ignore) x =
  let flat = static || structural x in
  let func_info = Some loc, fsig, set_asts in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.remove name s.methods;
    setters = SMap.add name func_info s.setters;
  }) x

let mem_constructor {constructor; _} = constructor <> []

let mem_field x = with_sig (fun s -> SMap.mem x s.fields)

let iter_methods f s =
  SMap.iter (fun _ -> Nel.iter f) s.methods;
  SMap.iter (fun _ -> f) s.getters;
  SMap.iter (fun _ -> f) s.setters

(* TODO? *)
let subst_field cx map (loc, polarity, field) =
  loc, polarity, match field with
  | Annot t -> Annot (Flow.subst cx map t)
  | Infer (fsig, set_asts) -> Infer (Func_sig.subst cx map fsig, set_asts)

let subst_sig cx map s =
  let subst_func_sig (loc, sig_, f) = (loc, Func_sig.subst cx map sig_, f) in
  {
    reason = s.reason;
    fields = SMap.map (subst_field cx map) s.fields;
    private_fields = SMap.map (subst_field cx map) s.private_fields;
    proto_fields = SMap.map (subst_field cx map) s.proto_fields;
    methods = SMap.map (Nel.map subst_func_sig) s.methods;
    getters = SMap.map (subst_func_sig) s.getters;
    setters = SMap.map (subst_func_sig) s.setters;
    calls = List.map (Flow.subst cx map) s.calls;
    call_deprecated = Option.map ~f:(Flow.subst cx map) s.call_deprecated;
  }

let subst_extends cx map = function
  | Explicit t -> Explicit (Flow.subst cx map t)
  | Implicit {null=_} as extends -> extends

let subst_super cx map = function
  | Interface { extends; callable } ->
    Interface {
      extends = List.map (Flow.subst cx map) extends;
      callable;
    }
  | Class { extends; mixins; implements } ->
    Class {
      extends = subst_extends cx map extends;
      mixins = List.map (Flow.subst cx map) mixins;
      implements = List.map (Flow.subst cx map) implements;
    }

let generate_tests cx f x =
  Flow.generate_tests cx x.tparams (fun map -> f {
    id = x.id;
    tparams = x.tparams;
    tparams_map = SMap.map (Flow.subst cx map) x.tparams_map;
    super = subst_super cx map x.super;
    constructor = List.map (fun (loc, sig_, g) -> loc, Func_sig.subst cx map sig_, g) x.constructor;
    static = subst_sig cx map x.static;
    instance = subst_sig cx map x.instance;
  })

let to_field (loc, polarity, field) =
  let t = match field with
  | Annot t -> t
  | Infer (fsig, _) -> Func_sig.gettertype fsig
  in
  Type.Field (loc, t, polarity)

let elements cx ?constructor s =
  let methods =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.mapi Type.(fun name xs ->
      let ms = Nel.rev_map (fun (loc, x, _) -> loc, Func_sig.methodtype cx x) xs in
      (* Keep track of these before intersections are merged, to enable
       * type information on every member of the intersection. *)
      ms |> Nel.iter (fun (loc, t) ->
        Option.iter loc ~f:(fun loc ->
          let id_info = name, t, Type_table.Other in
          Type_table.set_info loc id_info (Context.type_table cx)
        )
      );
      match ms with
      | x, [] -> x
      | (loc0, t0), (_, t1)::ts ->
          let ts = List.map (fun (_loc, t) -> t) ts in
          loc0, DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts))
    ) s.methods
  in

  (* Re-add the constructor as a method. *)
  let methods = match constructor with
  | Some t -> SMap.add "constructor" t methods
  | None -> methods
  in

  (* If there is a both a getter and a setter, then flow the setter type to
     the getter. Otherwise just use the getter type or the setter type *)
  let getters = SMap.map (fun (loc, t, _) -> loc, Func_sig.gettertype t) s.getters in
  let setters = SMap.map (fun (loc, t, _) -> loc, Func_sig.settertype t) s.setters in

  (* Register getters and setters with the type table *)
  let register_accessors = SMap.iter (fun name (loc, t) ->
    Option.iter ~f:(fun loc ->
      let id_info = name, t, Type_table.Other in
      Type_table.set_info loc id_info (Context.type_table cx)
    ) loc
  ) in
  register_accessors getters;
  register_accessors setters;

  let getters_and_setters = SMap.merge (fun _ getter setter ->
    match getter, setter with
    | Some (loc1, t1), Some (loc2, t2) -> Some (Type.GetSet (loc1, t1, loc2, t2))
    | Some (loc, t), None -> Some (Type.Get (loc, t))
    | None, Some (loc, t) -> Some (Type.Set (loc, t))
    | _ -> None
  ) getters setters in

  let fields = SMap.map to_field s.fields in

  (* Register fields with the type table *)
  SMap.iter (fun name fld ->
    let loc_type_opt = match fld with
    | Some loc, _, Annot t -> Some (loc, t)
    | Some loc, _, Infer (func_sig, _) -> Some (loc, Func_sig.gettertype func_sig)
    | _ -> None
    in
    Option.iter ~f:(fun (loc, t) ->
      let id_info = name, t, Type_table.Other in
      Type_table.set_info loc id_info (Context.type_table cx)
    ) loc_type_opt
  ) s.fields;

  let methods = SMap.map (fun (loc, t) -> Type.Method (loc, t)) methods in

  (* Treat proto fields as methods, as they are on the proto object *)
  let methods = SMap.fold (fun name fld acc ->
    SMap.add name (to_field fld) acc
  ) s.proto_fields methods in

  (* Treat getters and setters as methods *)
  let methods = SMap.union getters_and_setters methods in

  (* Previously, call properties were stored in the props map under the key
     $call. Unfortunately, this made it possible to specify call properties
     using this syntax in interfaces, declared classes, and even normal classes.

     Note that $call properties always override the call property syntax
     As before, if both are present, the $call property is used and the call
     property is ignored. *)
  let call = match s.call_deprecated with
  | Some t -> Some t
  | None ->
    match List.rev s.calls with
    | [] -> None
    | [t] -> Some t
    | t0::t1::ts ->
      let open Type in
      let t = DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts)) in
      Some t
  in

  (* Only un-initialized fields require annotations, so determine now
   * (syntactically) which fields have initializers *)
  let initialized_fields = SMap.fold (fun x (_, _, field) acc ->
    match field with
    | Annot _ -> acc
    | Infer _ -> SSet.add x acc
  ) s.fields SSet.empty in

  initialized_fields, fields, methods, call

let arg_polarities x =
  List.fold_left Type.(fun acc tp ->
    SMap.add tp.name tp.polarity acc
  ) SMap.empty x.tparams

let statictype cx x =
  let s = x.static in
  let inited_fields, fields, methods, call = elements cx s in
  let props = SMap.union fields methods
    ~combine:(fun _ _ ->
      Utils_js.assert_false (Utils_js.spf
        "static fields and methods must be disjoint: %s"
        (Debug_js.dump_reason cx s.reason)))
  in
  let static_proto = match x.super with
  | Interface _ -> Type.NullProtoT s.reason (* interfaces don't have statics *)
  | Class { extends; _ } ->
    match extends with
    (* class B extends A {}; B.__proto__ === A *)
    | Explicit t -> Type.class_type t
    (* class A {}; A.__proto__ === Function.prototype *)
    | Implicit _ -> Type.FunProtoT s.reason
  in
  (* Statics are not exact, because we allow width subtyping between them.
     Specifically, given class A and class B extends A, Class<B> <: Class<A>. *)
  let static =
    Obj_type.mk_with_proto cx s.reason ~props ?call static_proto
      ~sealed:true ~exact:false
  in
  let open Type in
  match static with
  | DefT (_, ObjT o) -> inited_fields, o
  | _ -> failwith "statics must be an ObjT"

let insttype cx ~initialized_static_fields s =
  let constructor =
    let ts = List.rev_map (fun (loc, t, _) -> loc, Func_sig.methodtype cx t) s.constructor in
    match ts with
    | [] -> None
    | [x] -> Some x
    | (loc0, t0)::(_loc1, t1)::ts ->
      let ts = List.map snd ts in
      let open Type in
      let t = DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts)) in
      Some (loc0, t)
  in
  let initialized_fields, fields, methods, call = elements cx ?constructor s.instance in
  { Type.
    class_id = s.id;
    type_args = SMap.map (fun t -> (Type.reason_of_t t, t)) s.tparams_map;
    arg_polarities = arg_polarities s;
    own_props = Context.make_property_map cx fields;
    proto_props = Context.make_property_map cx methods;
    inst_call_t = Option.map call ~f:(Context.make_call_prop cx);
    initialized_fields;
    initialized_static_fields;
    has_unknown_react_mixins = false;
    structural = structural s;
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
  if structural x then x else {
    x with
    tparams = List.rev (List.tl (List.rev x.tparams));
    tparams_map = SMap.remove "this" x.tparams_map;
  }

let supertype _cx x =
  let super_reason = replace_reason (fun d -> RSuperOf d) x.instance.reason in
  let open Type in
  match x.super with
  | Interface {extends; callable} ->
    (* If the interface definition includes a callable property, add the
       function prototype to the super type *)
    let extends =
      if callable
      then (FunProtoT super_reason)::extends
      else extends
    in
    (* Interfaces support multiple inheritance, which is modelled as an
       intersection of super types. TODO: Instead of using an intersection for
       this, we should resolve the extends and build a flattened type, and just
       use FunProtoT/ObjProtoT as the prototype *)
    (match extends with
    | [] -> ObjProtoT super_reason
    | [t] -> t
    | t0::t1::ts -> DefT (super_reason, IntersectionT (InterRep.make t0 t1 ts)))
  | Class {extends; mixins; _} ->
    let t = match extends with
    | Explicit t -> t
    | Implicit {null} ->
      if null then NullProtoT super_reason else ObjProtoT super_reason
    in
    (match mixins with
    | [] -> t
    | t0::ts ->
      let t1, ts = match ts with
      | [] -> t, []
      | t1::ts -> t1, ts@[t]
      in
      DefT (super_reason, IntersectionT (InterRep.make t0 t1 ts)))

let thistype cx x =
  let x = remove_this x in
  let {
    static = {reason = sreason; _};
    instance = {reason; _};
    _;
  } = x in
  let super = supertype cx x in
  let implements = match x.super with
  | Interface _ -> []
  | Class {implements; _} -> implements
  in
  let initialized_static_fields, static_objtype = statictype cx x in
  let insttype = insttype cx ~initialized_static_fields x in
  let open Type in
  let static = DefT (sreason, ObjT static_objtype) in
  DefT (reason, InstanceT (static, super, implements, insttype))

let check_implements cx def_reason x =
  match x.super with
  | Interface _ -> ()
  | Class {implements; _} ->
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
    ) implements

let check_super cx def_reason x =
  let x = remove_this x in
  let reason = x.instance.reason in
  let open Type in
  let initialized_static_fields, static_objtype = statictype cx x in
  let insttype = insttype cx ~initialized_static_fields x in
  let super = supertype cx x in
  let use_op = Op (ClassExtendsCheck {
    def = def_reason;
    name = reason;
    extends = reason_of_t super;
  }) in
  Flow.flow cx (super, SuperT (use_op, reason, Derived {
    instance = insttype;
    statics = static_objtype
  }))

(* TODO: Ideally we should check polarity for all class types, but this flag is
   flipped off for interface/declare class currently. *)
let classtype cx ?(check_polarity=true) x =
  let this = thistype cx x in
  let { tparams; _ } = remove_this x in
  let open Type in
  (if check_polarity then Flow.check_polarity cx Positive this);
  let t = if structural x then class_type this else this_class_type this in
  poly_type (Context.make_nominal cx) tparams t

(* Processes the bodies of instance and static class members. *)
let toplevels cx ~decls ~stmts ~expr x =
  Env.in_lex_scope cx (fun () ->
    let new_entry t = Scope.Entry.new_var ~loc:(Type.loc_of_t t) t in

    let method_ this super ~set_asts f =
      let save_return = Abnormal.clear_saved Abnormal.Return in
      let save_throw = Abnormal.clear_saved Abnormal.Throw in
      let asts = f |> Func_sig.generate_tests cx (
        Func_sig.toplevels None cx this super ~decls ~stmts ~expr
      ) in
      set_asts asts;
      ignore (Abnormal.swap_saved Abnormal.Return save_return);
      ignore (Abnormal.swap_saved Abnormal.Throw save_throw)
    in

    let field config this super _name (_, _, value) =
      match config, value with
      | Options.ESPROPOSAL_IGNORE, _ -> ()
      | _, Annot _ -> ()
      | _, Infer (fsig, set_asts) -> method_ this super ~set_asts fsig
    in

    let this = SMap.find_unsafe "this" x.tparams_map in
    let static = Type.class_type this in

    let super, static_super =
      let super_reason = replace_reason (fun d -> RSuperOf d) x.instance.reason in
      match x.super with
      | Interface _ -> failwith "tried to evaluate toplevel of interface"
      | Class {extends; _} ->
        match extends with
        | Explicit t -> t, Type.class_type t
        | Implicit {null} ->
          let open Type in
          (if null then NullProtoT super_reason else ObjProtoT super_reason),
          FunProtoT super_reason
    in

    (* Bind private fields to the environment *)
    let to_prop_map = fun x -> Context.make_property_map cx (SMap.map to_field x) in
    Env.bind_class cx x.id (to_prop_map x.instance.private_fields)
    (to_prop_map x.static.private_fields);

    x |> with_sig ~static:true (fun s ->
      (* process static methods and fields *)
      let this, super = new_entry static, new_entry static_super in
      iter_methods (fun (_loc, f, set_asts) -> method_ this super ~set_asts f) s;
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
        let derived_ctor = match x.super with
          | Class {extends = Explicit _; _} -> true
          | _ -> false
        in
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
        let this, super = new_entry this, new_entry super in
        x.constructor |> List.iter (fun (_, fsig, set_asts) ->
          method_ this super ~set_asts fsig
        )
      end;

      (* process instance methods and fields *)
      begin
        let this, super = new_entry this, new_entry super in
        iter_methods (fun (_, msig, set_asts) -> method_ this super ~set_asts msig) s;
        let config = Context.esproposal_class_instance_fields cx in
        SMap.iter (field config this super) s.fields;
        SMap.iter (field config this super) s.private_fields;
        SMap.iter (field config this super) s.proto_fields;
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

let with_typeparams cx f x =
  Type_table.with_typeparams x.tparams (Context.type_table cx) f
