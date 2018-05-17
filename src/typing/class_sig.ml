(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js

open Reason

type field = Loc.t option * Type.polarity * field'
and field' = Annot of Type.t | Infer of Func_sig.t

type signature = {
  reason: reason;
  super: Type.t;
  fields: field SMap.t;
  private_fields: field SMap.t;
  proto_fields: field SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: (Loc.t option * Func_sig.t) Nel.t SMap.t;
  getters: (Loc.t option * Func_sig.t) SMap.t;
  setters: (Loc.t option * Func_sig.t) SMap.t;
}

type t = {
  id: int;
  structural: bool;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  implements: Type.t list;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: (Loc.t option * Func_sig.t) list;
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
    proto_fields = SMap.empty;
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

let add_constructor loc fsig s =
  {s with constructor = [loc, Func_sig.to_ctor_sig fsig]}

let add_default_constructor reason s =
  let fsig = Func_sig.default_constructor reason in
  add_constructor None fsig s

let append_constructor loc fsig s =
  {s with constructor = (loc, Func_sig.to_ctor_sig fsig)::s.constructor}

let add_field ~static name fld x =
  let flat = static || x.structural in
  map_sig ~static (fun s -> {
    s with
    fields = SMap.add name fld s.fields;
    proto_fields = if flat then SMap.remove name s.proto_fields else s.proto_fields;
    methods = if flat then SMap.remove name s.methods else s.methods;
    getters = if flat then SMap.remove name s.getters else s.getters;
    setters = if flat then SMap.remove name s.setters else s.setters;
  }) x

let add_proto_field name fld x =
  map_sig ~static:false (fun s -> {
    s with
    proto_fields = SMap.add name fld s.proto_fields;
    methods = SMap.remove name s.methods;
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

let add_method ~static name loc fsig x =
  let flat = static || x.structural in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.add name (Nel.one (loc, fsig)) s.methods;
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method ~static name loc fsig x =
  let flat = static || x.structural in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = (
      match SMap.get name s.methods with
      | Some fsigs -> SMap.add name (Nel.cons (loc, fsig) fsigs) s.methods
      | None -> SMap.add name (Nel.one (loc, fsig)) s.methods
    );
    getters = SMap.remove name s.getters;
    setters = SMap.remove name s.setters;
  }) x

let add_getter ~static name loc fsig x =
  let flat = static || x.structural in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.remove name s.methods;
    getters = SMap.add name (loc, fsig) s.getters;
  }) x

let add_setter ~static name loc fsig x =
  let flat = static || x.structural in
  map_sig ~static (fun s -> {
    s with
    fields = if flat then SMap.remove name s.fields else s.fields;
    proto_fields = SMap.remove name s.proto_fields;
    methods = SMap.remove name s.methods;
    setters = SMap.add name (loc, fsig) s.setters;
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
  | Infer fsig -> Infer (Func_sig.subst cx map fsig)

let subst_sig cx map s =
  let subst_func_sig (loc, sig_) = (loc, Func_sig.subst cx map sig_) in
  {
    reason = s.reason;
    super = Flow.subst cx map s.super;
    fields = SMap.map (subst_field cx map) s.fields;
    private_fields = SMap.map (subst_field cx map) s.private_fields;
    proto_fields = SMap.map (subst_field cx map) s.proto_fields;
    methods = SMap.map (Nel.map subst_func_sig) s.methods;
    getters = SMap.map (subst_func_sig) s.getters;
    setters = SMap.map (subst_func_sig) s.setters;
  }

let generate_tests cx f x =
  Flow.generate_tests cx x.tparams (fun map -> f {
    id = x.id;
    structural = x.structural;
    tparams = x.tparams;
    tparams_map = SMap.map (Flow.subst cx map) x.tparams_map;
    implements = List.map (Flow.subst cx map) x.implements;
    constructor = List.map (fun (loc, sig_) -> loc, Func_sig.subst cx map sig_) x.constructor;
    static = subst_sig cx map x.static;
    instance = subst_sig cx map x.instance;
  })

let to_field (loc, polarity, field) =
  let t = match field with
  | Annot t -> t
  | Infer fsig -> Func_sig.gettertype fsig
  in
  Type.Field (loc, t, polarity)

let elements cx ~tparams_map ?constructor s =
  let methods =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.mapi Type.(fun name xs ->
      let ms = Nel.rev_map (fun (loc, x) -> loc, Func_sig.methodtype cx x) xs in
      (* Keep track of these before intersections are merged, to enable
       * type information on every member of the intersection. *)
      ms |> Nel.iter (fun (loc, t) ->
        Option.iter loc ~f:(fun loc ->
          let id_info = name, t, Type_table.Other in
          Env.add_type_table_info cx ~tparams_map loc id_info
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
  let getters = SMap.map (fun (loc, t) -> loc, Func_sig.gettertype t) s.getters in
  let setters = SMap.map (fun (loc, t) -> loc, Func_sig.settertype t) s.setters in

  (* Register getters and setters with the type table *)
  let register_accessors = SMap.iter (fun name (loc, t) ->
    Option.iter ~f:(fun loc ->
      let id_info = name, t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map loc id_info
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
    | Some loc, _, Infer func_sig -> Some (loc, Func_sig.gettertype func_sig)
    | _ -> None
    in
    Option.iter ~f:(fun (loc, t) ->
      let id_info = name, t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map loc id_info
    ) loc_type_opt
  ) s.fields;

  (* Treat getters and setters as fields *)
  let fields = SMap.union getters_and_setters fields in

  let methods = SMap.map (fun (loc, t) -> Type.Method (loc, t)) methods in

  (* Treat proto fields as methods, as they are on the proto object *)
  let methods = SMap.fold (fun name fld acc ->
    SMap.add name (to_field fld) acc
  ) s.proto_fields methods in

  (* Only un-initialized fields require annotations, so determine now
   * (syntactically) which fields have initializers *)
  let initialized_field_names = SMap.fold (fun x (_, _, field) acc ->
    match field with
    | Annot _ -> acc
    | Infer _ -> SSet.add x acc
  ) s.fields SSet.empty in

  initialized_field_names, fields, methods

let arg_polarities x =
  List.fold_left Type.(fun acc tp ->
    SMap.add tp.name tp.polarity acc
  ) SMap.empty x.tparams

let statictype cx ~tparams_map s =
  let inited_fields, fields, methods = elements cx ~tparams_map s in
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

let insttype cx ~tparams_map ~initialized_static_field_names s =
  let constructor =
    let ts = List.rev_map (fun (loc, t) -> loc, Func_sig.methodtype cx t) s.constructor in
    match ts with
    | [] -> None
    | [x] -> Some x
    | (loc0, t0)::(_loc1, t1)::ts ->
      let ts = List.map snd ts in
      let open Type in
      let t = DefT (reason_of_t t0, IntersectionT (InterRep.make t0 t1 ts)) in
      Some (loc0, t)
  in
  let inited_fields, fields, methods = elements cx ~tparams_map ?constructor s.instance in
  { Type.
    class_id = s.id;
    type_args = SMap.map (fun t -> (Type.reason_of_t t, t)) s.tparams_map;
    arg_polarities = arg_polarities s;
    fields_tmap = Context.make_property_map cx fields;
    initialized_field_names = inited_fields;
    initialized_static_field_names;
    methods_tmap = Context.make_property_map cx methods;
    has_unknown_react_mixins = false;
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
  let tparams_map_with_this = x.tparams_map in
  let x = remove_this x in
  let {
    implements;
    static = {reason = sreason; _};
    instance = {reason; super; _};
    tparams_map;
    _;
  } = x in
  let open Type in
  let initialized_static_field_names, static_objtype = statictype cx ~tparams_map x.static in
  let insttype = insttype cx ~tparams_map:tparams_map_with_this ~initialized_static_field_names x in
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
  let tparams_map = x.tparams_map in
  let initialized_static_field_names, static_objtype = statictype cx ~tparams_map x.static in
  let insttype = insttype cx ~tparams_map ~initialized_static_field_names x in
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
  let { structural; tparams; _ } = remove_this x in
  let open Type in
  (if check_polarity then Flow.check_polarity cx Positive this);
  let t = if structural then class_type this else this_class_type this in
  poly_type (Context.make_nominal cx) tparams t

(* Processes the bodies of instance and static class members. *)
let toplevels cx ~decls ~stmts ~expr x =
  Env.in_lex_scope cx (fun () ->
    let new_entry t = Scope.Entry.new_var ~loc:(Type.loc_of_t t) t in

    let method_ this super ~static f =
      let save_return = Abnormal.clear_saved Abnormal.Return in
      let save_throw = Abnormal.clear_saved Abnormal.Throw in
      f |> Func_sig.generate_tests cx (
        Func_sig.toplevels None cx this super static ~decls ~stmts ~expr
      );
      ignore (Abnormal.swap_saved Abnormal.Return save_return);
      ignore (Abnormal.swap_saved Abnormal.Throw save_throw)
    in

    let field config this super ~static _name (_, _, value) =
      match config, value with
      | Options.ESPROPOSAL_IGNORE, _ -> ()
      | _, Annot _ -> ()
      | _, Infer fsig -> method_ this super ~static fsig
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
      iter_methods (fun (_loc, f) -> method_ this super true f) s;
      let config = Context.esproposal_class_static_fields cx in
      SMap.iter (field config this super ~static:true) s.fields;
      SMap.iter (field config this super ~static:true) s.private_fields
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
        x.constructor |> List.iter (fun (_, fsig) -> method_ this super false fsig)
      end;

      (* process instance methods and fields *)
      begin
        let this, super = new_entry this, new_entry s.super in
        iter_methods (fun (_, msig) -> method_ this super false msig) s;
        let config = Context.esproposal_class_instance_fields cx in
        SMap.iter (field config this super ~static:false) s.fields;
        SMap.iter (field config this super ~static:false) s.private_fields;
        SMap.iter (field config this super ~static:false) s.proto_fields;
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
