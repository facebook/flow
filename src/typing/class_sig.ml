(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js
module ConsGen = Type_annotation_cons_gen
open Reason
open Utils_js
include Class_sig_intf

module Make
    (CT : Func_class_sig_types.Config.S)
    (C : Func_params_intf.Config with module Types := CT)
    (P : Func_params_intf.S with module Config_types := CT and module Config := C)
    (F : Func_sig_intf.S with module Config_types := CT and module Config := C and module Param := P)
    (T : Func_class_sig_types.Class.S
           with module Config := CT
            and module Param := P.Types
            and module Func := F.Types) :
  S
    with module Config_types = CT
     and module Config = C
     and module Param = P
     and module Func = F
     and module Types = T = struct
  module Config_types = CT
  module Types = T
  module Config = C
  module Param = P
  module Func = F
  open Types

  let empty id class_name class_loc reason tparams tparams_map super =
    let empty_sig reason =
      {
        reason;
        fields = SMap.empty;
        private_fields = SMap.empty;
        proto_fields = SMap.empty;
        methods = SMap.empty;
        private_methods = SMap.empty;
        getters = SMap.empty;
        setters = SMap.empty;
        calls = [];
        dict = None;
      }
    in
    let constructor = [] in
    let static =
      let reason = update_desc_reason (fun desc -> RStatics desc) reason in
      empty_sig reason
    in
    let instance = empty_sig reason in
    { id; class_name; class_loc; tparams; tparams_map; super; constructor; static; instance }

  let structural x =
    match x.super with
    | Interface _ -> true
    | Class _ -> false

  let inst_kind x =
    match x.super with
    | Interface { inline; _ } -> Type.InterfaceKind { inline }
    | Class _ -> Type.ClassKind

  let map_sig ~static f s =
    if static then
      { s with static = f s.static }
    else
      { s with instance = f s.instance }

  let with_sig ~static f s =
    if static then
      f s.static
    else
      f s.instance

  let add_private_field name loc polarity field =
    map_sig (fun s ->
        { s with private_fields = SMap.add name (Some loc, polarity, field) s.private_fields }
    )

  let add_private_method ~static name ~id_loc ~this_write_loc ~func_sig ~set_asts ~set_type x =
    let func_info = { id_loc = Some id_loc; this_write_loc; func_sig; set_asts; set_type } in
    map_sig
      ~static
      (fun s -> { s with private_methods = SMap.add name func_info s.private_methods })
      x

  let public_fields_of_signature ~static s =
    ( if static then
      s.static
    else
      s.instance
    )
      .fields

  let private_fields_of_signature ~static s =
    ( if static then
      s.static
    else
      s.instance
    )
      .private_fields

  let add_constructor ~id_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) s =
    {
      s with
      constructor =
        [{ id_loc; this_write_loc = None; func_sig = F.to_ctor_sig func_sig; set_asts; set_type }];
    }

  let add_default_constructor reason s =
    let func_sig = F.default_constructor reason in
    add_constructor ~id_loc:None ~func_sig s

  let append_constructor ~id_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) s =
    {
      s with
      constructor =
        { id_loc; this_write_loc = None; func_sig = F.to_ctor_sig func_sig; set_asts; set_type }
        :: s.constructor;
    }

  let add_field' ~static name fld x =
    let flat = static || structural x in
    map_sig
      ~static
      (fun s ->
        {
          s with
          fields = SMap.add name fld s.fields;
          proto_fields =
            ( if flat then
              SMap.remove name s.proto_fields
            else
              s.proto_fields
            );
          methods =
            ( if flat then
              SMap.remove name s.methods
            else
              s.methods
            );
          getters =
            ( if flat then
              SMap.remove name s.getters
            else
              s.getters
            );
          setters =
            ( if flat then
              SMap.remove name s.setters
            else
              s.setters
            );
        })
      x

  let add_field ~static name loc polarity field x =
    add_field' ~static name (Some loc, polarity, field) x

  let add_indexer ~static dict x = map_sig ~static (fun s -> { s with dict = Some dict }) x

  let has_indexer ~static x = with_sig ~static (fun s -> Option.is_some s.dict) x

  let add_name_field x =
    let r = update_desc_reason (fun desc -> RNameProperty desc) x.instance.reason in
    let t = Type.StrModuleT.why r in
    add_field' ~static:true "name" (None, Polarity.Neutral, Annot t) x

  let add_proto_field name loc polarity field x =
    map_sig
      ~static:false
      (fun s ->
        {
          s with
          proto_fields = SMap.add name (Some loc, polarity, field) s.proto_fields;
          methods = SMap.remove name s.methods;
          getters = SMap.remove name s.getters;
          setters = SMap.remove name s.setters;
        })
      x

  let add_method
      ~static name ~id_loc ~this_write_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) x =
    let flat = static || structural x in
    let func_info = { id_loc = Some id_loc; this_write_loc; func_sig; set_asts; set_type } in
    map_sig
      ~static
      (fun s ->
        {
          s with
          fields =
            ( if flat then
              SMap.remove name s.fields
            else
              s.fields
            );
          proto_fields = SMap.remove name s.proto_fields;
          methods = SMap.add name (Nel.one func_info) s.methods;
          getters = SMap.remove name s.getters;
          setters = SMap.remove name s.setters;
        })
      x

  (* Appending a method builds a list of function signatures. This implements the
     bahvior of interfaces and declared classes, which interpret duplicate
     definitions as branches of a single overloaded method. *)
  let append_method
      ~static name ~id_loc ~this_write_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) x =
    let flat = static || structural x in
    let func_info = { id_loc = Some id_loc; this_write_loc; func_sig; set_asts; set_type } in
    map_sig
      ~static
      (fun s ->
        {
          s with
          fields =
            ( if flat then
              SMap.remove name s.fields
            else
              s.fields
            );
          proto_fields = SMap.remove name s.proto_fields;
          methods =
            (match SMap.find_opt name s.methods with
            | Some fsigs -> SMap.add name (Nel.cons func_info fsigs) s.methods
            | None -> SMap.add name (Nel.one func_info) s.methods);
          getters = SMap.remove name s.getters;
          setters = SMap.remove name s.setters;
        })
      x

  let append_call ~static t = map_sig ~static (fun s -> { s with calls = t :: s.calls })

  let add_getter
      ~static name ~id_loc ~this_write_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) x =
    let flat = static || structural x in
    let func_info = { id_loc = Some id_loc; this_write_loc; func_sig; set_asts; set_type } in
    map_sig
      ~static
      (fun s ->
        {
          s with
          fields =
            ( if flat then
              SMap.remove name s.fields
            else
              s.fields
            );
          proto_fields = SMap.remove name s.proto_fields;
          methods = SMap.remove name s.methods;
          getters = SMap.add name func_info s.getters;
        })
      x

  let add_setter
      ~static name ~id_loc ~this_write_loc ~func_sig ?(set_asts = ignore) ?(set_type = ignore) x =
    let flat = static || structural x in
    let func_info = { id_loc = Some id_loc; this_write_loc; func_sig; set_asts; set_type } in
    map_sig
      ~static
      (fun s ->
        {
          s with
          fields =
            ( if flat then
              SMap.remove name s.fields
            else
              s.fields
            );
          proto_fields = SMap.remove name s.proto_fields;
          methods = SMap.remove name s.methods;
          setters = SMap.add name func_info s.setters;
        })
      x

  let mem_constructor { constructor; _ } = constructor <> []

  let iter_methods_with_name f s =
    SMap.iter (f %> Nel.iter) s.methods;
    SMap.iter f s.private_methods;
    SMap.iter f s.getters;
    SMap.iter f s.setters

  let iter_methods f = iter_methods_with_name (fun _ -> f)

  let this_tparam x =
    match x.super with
    | Class { this_tparam; _ } -> Some this_tparam
    | Interface _ -> None

  let this_t x =
    match x.super with
    | Class { this_t; _ } -> Some this_t
    | Interface _ -> None

  let this_or_mixed loc = this_t %> Base.Option.value ~default:(Type.dummy_this loc)

  let tparams_with_this tparams this_tp =
    (* Use the loc for the original tparams, or just the loc for the this type if there are no
       * tparams *)
    let loc = Base.Option.value_map ~default:(loc_of_reason this_tp.Type.reason) ~f:fst tparams in
    (* Add the type of `this` to the end of the list of type
       parameters. Remember, order is important, since we don't have recursive
       bounds (aka F-bounds): the bound of This refers to all the other type
       parameters! *)
    let tparams_lst = Type.TypeParams.to_list tparams @ [this_tp] in
    (* Obviously there is at least one element since we just added `this_tp` *)
    let tparams_nel = Base.Option.value_exn (Nel.of_list tparams_lst) in
    Some (loc, tparams_nel)

  let to_field (key_loc, polarity, field) =
    let type_ =
      match field with
      | Annot t -> t
      | Infer (fsig, _) -> F.gettertype fsig
    in
    Type.Field { preferred_def_locs = None; key_loc; type_; polarity }

  let to_method
      cx this_default { id_loc; this_write_loc; func_sig = fsig; set_asts = _; set_type = _ } =
    Type.Method { key_loc = id_loc; type_ = F.methodtype cx this_write_loc this_default fsig }

  let to_prop_map to_prop_converter cx =
    SMap.map to_prop_converter %> NameUtils.namemap_of_smap %> Context.generate_property_map cx

  let fields_to_prop_map = to_prop_map to_field

  let methods_to_prop_map ~cx ~this_default = to_prop_map (to_method cx this_default) cx

  let elements cx ~this ?constructor s super =
    (* To determine the default `this` parameter for a method without `this` annotation, we
       default to the instance/static `this` type *)
    let this_default (x : F.Types.t) =
      match (x.F.Types.body, super) with
      (* We can use mixed for declared class methods here for two reasons:
         1) They can never be unbound
         2) They have no body
      *)
      | (None, Class _) -> Type.implicit_mixed_this x.F.Types.reason
      | (Some _, Class _) ->
        TypeUtil.mod_reason_of_t Reason.(update_desc_reason (fun desc -> RImplicitThis desc)) this
      | (_, Interface _) -> x.F.Types.reason |> Type.implicit_mixed_this
    in
    let methods =
      (* If this is an overloaded method, create an intersection, attributed
         to the first declared function signature. If there is a single
         function signature for this method, simply return the method type. *)
      let open Type in
      let open TypeUtil in
      SMap.mapi
        (fun _name xs ->
          let ms =
            Nel.rev_map
              (fun { id_loc; this_write_loc; func_sig = x; set_asts = _; set_type } ->
                (id_loc, F.methodtype cx this_write_loc (this_default x) x, set_type))
              xs
          in
          (* Keep track of these before intersections are merged, to enable
           * type information on every member of the intersection. *)
          ms |> Nel.iter (fun (loc, t, set_type) -> Base.Option.iter loc ~f:(fun _loc -> set_type t));
          match ms with
          | ((loc, t, _), []) -> (loc, t)
          | ((loc0, t0, _), (_, t1, _) :: ts) ->
            let ts = Base.List.map ~f:(fun (_loc, t, _) -> t) ts in
            (loc0, IntersectionT (reason_of_t t0, InterRep.make t0 t1 ts)))
        s.methods
    in
    let () =
      let private_this_default x = Type.(AnyT.make (Unsound BoundFunctionThis) x.F.Types.reason) in
      SMap.iter
        (fun _name { id_loc; this_write_loc; func_sig = x; set_asts = _; set_type } ->
          Base.Option.iter id_loc ~f:(fun _loc ->
              set_type (F.methodtype cx this_write_loc (private_this_default x) x)
          ))
        s.private_methods
    in
    (* Re-add the constructor as a method. *)
    let methods =
      match constructor with
      | Some t -> SMap.add "constructor" t methods
      | None -> methods
    in
    (* If there is a both a getter and a setter, then flow the setter type to
       the getter. Otherwise just use the getter type or the setter type *)
    let getters =
      SMap.map
        (fun { id_loc; this_write_loc = _; func_sig = t; set_asts = _; set_type } ->
          (id_loc, F.gettertype t, set_type))
        s.getters
    in
    let setters =
      SMap.map
        (fun { id_loc; this_write_loc = _; func_sig = t; set_asts = _; set_type } ->
          (id_loc, F.settertype t, set_type))
        s.setters
    in
    (* Register getters and setters with the typed AST *)
    let register_accessors =
      SMap.iter (fun _ (loc, t, set_type) -> Base.Option.iter ~f:(fun _ -> set_type t) loc)
    in
    register_accessors getters;
    register_accessors setters;

    let getters_and_setters =
      SMap.merge
        (fun _ getter setter ->
          match (getter, setter) with
          | (Some (get_key_loc, get_type, _), Some (set_key_loc, set_type, _)) ->
            Some (Type.GetSet { get_key_loc; get_type; set_key_loc; set_type })
          | (Some (key_loc, type_, _), None) -> Some (Type.Get { key_loc; type_ })
          | (None, Some (key_loc, type_, _)) -> Some (Type.Set { key_loc; type_ })
          | _ -> None)
        getters
        setters
    in
    let fields = SMap.map to_field s.fields in
    let methods = SMap.map (fun (key_loc, type_) -> Type.Method { key_loc; type_ }) methods in
    (* Treat proto fields as methods, as they are on the proto object *)
    let methods =
      SMap.fold (fun name fld acc -> SMap.add name (to_field fld) acc) s.proto_fields methods
    in
    (* Treat getters and setters as methods *)
    let methods = SMap.union getters_and_setters methods in
    let call =
      match List.rev s.calls with
      | [] -> None
      | [t] -> Some t
      | t0 :: t1 :: ts ->
        let open Type in
        let open TypeUtil in
        let t = IntersectionT (reason_of_t t0, InterRep.make t0 t1 ts) in
        Some t
    in
    (* Only un-initialized fields require annotations, so determine now
     * (syntactically) which fields have initializers *)
    let initialized_fields =
      SMap.fold
        (fun x (_, _, field) acc ->
          match field with
          | Annot _ -> acc
          | Infer _ -> SSet.add x acc)
        s.fields
        SSet.empty
    in
    (initialized_fields, fields, methods, call)

  let specialize cx use_op targs c =
    let reason = TypeUtil.reason_of_t c in
    ConsGen.specialize cx c use_op reason reason targs

  let statictype cx static_proto x =
    let s = x.static in
    let (inited_fields, fields, methods, call) =
      let loc = loc_of_reason x.static.reason in
      elements cx ~this:(this_or_mixed loc x |> TypeUtil.class_type) s x.super
    in
    let props =
      SMap.union fields methods ~combine:(fun _ _ ->
          Utils_js.assert_false
            (Utils_js.spf
               "static fields and methods must be disjoint: %s"
               (Debug_js.dump_reason cx s.reason)
            )
      )
      |> NameUtils.namemap_of_smap
    in
    (* Statics are not exact, because we allow width subtyping between them.
       Specifically, given class A and class B extends A, Class<B> <: Class<A>. *)
    let static =
      Obj_type.mk_with_proto cx s.reason ~props ?call static_proto ~obj_kind:Type.Inexact
    in
    Type.(
      match static with
      | DefT (_, ObjT o) -> (inited_fields, o)
      | _ -> failwith "statics must be an ObjT"
    )

  let insttype cx ~initialized_static_fields s =
    let constructor =
      (* Constructors do not bind `this` *)
      let ts =
        List.rev_map
          (fun { id_loc; this_write_loc = _; func_sig = t; set_asts = _; set_type } ->
            let t = F.methodtype cx None (Type.dummy_this (loc_of_reason s.instance.reason)) t in
            let () = Base.Option.iter id_loc ~f:(fun _loc -> set_type t) in
            (id_loc, t))
          s.constructor
      in
      match ts with
      | [] -> None
      | [x] -> Some x
      | (loc0, t0) :: (_loc1, t1) :: ts ->
        let ts = Base.List.map ~f:snd ts in
        let open Type in
        let open TypeUtil in
        let t = IntersectionT (reason_of_t t0, InterRep.make t0 t1 ts) in
        Some (loc0, t)
    in
    let type_args =
      Base.List.map
        ~f:(fun { Type.name; reason; polarity; _ } ->
          let t = Subst_name.Map.find name s.tparams_map in
          (name, reason, t, polarity))
        (Type.TypeParams.to_list s.tparams)
    in
    let (initialized_fields, fields, methods, call) =
      let loc = loc_of_reason s.instance.reason in
      elements cx ~this:(this_or_mixed loc s) ?constructor s.instance s.super
    in
    let private_this_type ({ reason; _ } : signature) =
      Type.(AnyT.make (Unsound BoundFunctionThis) reason)
    in
    {
      Type.class_id = s.id;
      inst_react_dro = None;
      class_name = s.class_name;
      type_args;
      own_props = Context.generate_property_map cx (NameUtils.namemap_of_smap fields);
      proto_props = Context.generate_property_map cx (NameUtils.namemap_of_smap methods);
      inst_call_t = Base.Option.map call ~f:(Context.make_call_prop cx);
      initialized_fields;
      initialized_static_fields;
      inst_kind = inst_kind s;
      inst_dict = s.instance.dict;
      class_private_fields = fields_to_prop_map cx s.instance.private_fields;
      class_private_static_fields = fields_to_prop_map cx s.static.private_fields;
      class_private_methods =
        methods_to_prop_map
          ~cx
          ~this_default:(private_this_type s.instance)
          s.instance.private_methods;
      class_private_static_methods =
        methods_to_prop_map ~cx ~this_default:(private_this_type s.static) s.static.private_methods;
    }

  let mk_this ~self cx reason =
    let this_reason = replace_desc_reason RThisType reason in
    let this_tp =
      {
        Type.name = Subst_name.Name "this";
        reason = this_reason;
        bound = self;
        polarity = Polarity.Positive;
        default = None;
        is_this = true;
        is_const = false;
      }
    in
    (this_tp, Flow_js_utils.generic_of_tparam cx ~f:(fun x -> x) this_tp)

  let supertype cx x =
    let super_reason = update_desc_reason (fun d -> RSuperOf d) x.instance.reason in
    let static_reason = x.static.reason in
    let open Type in
    let open TypeUtil in
    match x.super with
    | Interface { inline = _; extends; callable } ->
      let extends =
        Base.List.map
          ~f:(fun (annot_loc, c, targs_opt) ->
            match targs_opt with
            | None ->
              let reason = annot_reason ~annot_loc @@ repos_reason annot_loc (reason_of_t c) in
              ConsGen.mk_instance cx reason c
            | Some targs -> typeapp_annot ~from_value:false ~use_desc:false annot_loc c targs)
          extends
      in
      (* If the interface definition includes a callable property, add the
         function prototype to the super type *)
      let extends =
        if callable then
          FunProtoT super_reason :: extends
        else
          extends
      in
      (* Interfaces support multiple inheritance, which is modelled as an
         intersection of super types. TODO: Instead of using an intersection for
         this, we should resolve the extends and build a flattened type, and just
         use FunProtoT/ObjProtoT as the prototype *)
      let super =
        match extends with
        | [] -> ObjProtoT super_reason
        | [t] -> t
        | t0 :: t1 :: ts -> IntersectionT (super_reason, InterRep.make t0 t1 ts)
      in
      (* interfaces don't have statics *)
      let static_proto = Type.NullProtoT static_reason in
      (super, static_proto)
    | Class { extends; mixins; this_t; _ } ->
      let (extends_t, static_proto) =
        match extends with
        | Explicit (annot_loc, c, targs) ->
          (* Eagerly specialize when there are no targs *)
          let c =
            if targs = None then
              let use_op =
                Op (ClassExtendsCheck { def = reason_of_t c; extends = x.instance.reason })
              in
              specialize cx use_op targs c
            else
              c
          in
          let t = this_typeapp ~annot_loc c this_t targs in
          (* class B extends A {}; B.__proto__ === A *)
          let static_proto = class_type ~annot_loc t in
          (t, static_proto)
        | Implicit { null } ->
          let t =
            if null then
              NullProtoT super_reason
            else
              ObjProtoT super_reason
          in
          (* class A {}; A.__proto__ === Function.prototype *)
          let static_proto = FunProtoT static_reason in
          (t, static_proto)
      in
      let mixins_rev =
        List.rev_map
          (fun (annot_loc, c, targs) ->
            (* Eagerly specialize when there are no targs *)
            let c =
              if targs = None then
                specialize cx unknown_use targs c
              else
                c
            in
            this_typeapp ~annot_loc c this_t targs)
          mixins
      in
      let super =
        match List.rev_append mixins_rev [extends_t] with
        | [] -> failwith "impossible"
        | [t] -> t
        | t0 :: t1 :: ts -> IntersectionT (super_reason, InterRep.make t0 t1 ts)
      in
      (super, static_proto)

  let this_instance_type cx x =
    let { static = { reason = sreason; _ }; instance = { reason; _ }; _ } = x in
    let (super, static_proto) = supertype cx x in
    let implements =
      match x.super with
      | Interface _ -> []
      | Class { implements; _ } ->
        Base.List.map
          ~f:(fun (annot_loc, c, targs_opt) ->
            match targs_opt with
            | None ->
              let reason =
                annot_reason ~annot_loc @@ repos_reason annot_loc (TypeUtil.reason_of_t c)
              in
              ConsGen.mk_instance cx reason c
            | Some targs ->
              TypeUtil.typeapp_annot ~from_value:false ~use_desc:false annot_loc c targs)
          implements
    in
    let (initialized_static_fields, static_objtype) = statictype cx static_proto x in
    let inst = insttype cx ~initialized_static_fields x in
    let static = Type.DefT (sreason, Type.ObjT static_objtype) in
    (reason, { Type.static; super; implements; inst })

  let thistype cx x =
    let (reason, instance_t) = this_instance_type cx x in
    Type.(DefT (reason, InstanceT instance_t))

  let check_methods cx def_reason x =
    let open Type in
    let self =
      match x.Types.super with
      | Interface _ -> thistype cx x
      | Class { this_t; _ } -> this_t
    in
    let check_method msig ~static name id_loc =
      Base.Option.iter
        ~f:(fun this_param ->
          let self =
            if static then
              TypeUtil.class_type self
            else
              self
          in
          let reason = mk_reason (RMethod (Some name)) id_loc in
          let use_op = Op (ClassMethodDefinition { def = reason; name = def_reason }) in
          Context.add_post_inference_subtyping_check cx self use_op this_param)
        (F.this_param msig.F.Types.fparams)
    in

    with_sig
      ~static:true
      (iter_methods_with_name
         (fun name { id_loc; this_write_loc = _; func_sig = msig; set_asts = _; set_type = _ } ->
           (* Constructors don't bind this *)
           Base.Option.iter ~f:(check_method msig ~static:true name) id_loc
       )
      )
      x;

    with_sig
      ~static:false
      (iter_methods_with_name
         (* Constructors don't bind this *)
         (fun name { id_loc; this_write_loc = _; func_sig = msig; set_asts = _; set_type = _ } ->
           Base.Option.iter ~f:(check_method msig ~static:false name) id_loc
       )
      )
      x

  let check_implements cx def_reason x =
    match x.super with
    | Interface _ -> ()
    | Class { implements; _ } ->
      let this = thistype cx x in
      let reason = x.instance.reason in
      let open Type in
      let open TypeUtil in
      List.iter
        (fun (annot_loc, c, targs_opt) ->
          let i =
            match targs_opt with
            | None ->
              let reason = annot_reason ~annot_loc @@ repos_reason annot_loc (reason_of_t c) in
              ConsGen.mk_instance cx reason c
            | Some targs -> typeapp_annot ~from_value:false ~use_desc:false annot_loc c targs
          in
          let use_op =
            Op (ClassImplementsCheck { def = def_reason; name = reason; implements = reason_of_t i })
          in
          Context.add_post_inference_validation_flow cx i (ImplementsT (use_op, this)))
        implements

  let check_super cx def_reason x =
    let reason = x.instance.reason in
    let open Type in
    let open TypeUtil in
    (* NOTE: SuperT ignores the constructor anyway, so we don't pass it here.
       Call properties are also ignored, so we ignore that result. *)
    (* The this parameter of a class method is irrelvant for class subtyping, since
       dynamic dispatch enforces that the method is called on the right subclass
       at runtime even if the static type is a supertype. *)
    let inst_loc = loc_of_reason reason in
    let (_, own, proto, _call) =
      elements cx ~this:(this_or_mixed inst_loc x) ?constructor:None x.instance x.super
    in
    let static =
      (* NOTE: The own, proto maps are disjoint by construction. *)
      let (_, own, proto, _call) =
        elements cx ~this:(this_or_mixed inst_loc x |> class_type) x.static x.super
      in
      SMap.union own proto
    in
    SMap.iter
      (fun x p1 ->
        match SMap.find_opt x proto with
        | None -> ()
        | Some p2 ->
          let prop = OrdinaryName x in
          let use_op =
            Op
              (ClassOwnProtoCheck
                 { prop; own_loc = Property.first_loc p1; proto_loc = Property.first_loc p2 }
              )
          in
          let propref = mk_named_prop ~reason prop in
          Flow.flow_p cx ~use_op reason reason propref (Property.type_ p1, Property.type_ p2))
      own;

    let (super, _) = supertype cx x in
    let use_op = Op (ClassExtendsCheck { def = def_reason; extends = reason_of_t super }) in
    Context.add_post_inference_validation_flow
      cx
      super
      (SuperT
         ( use_op,
           reason,
           Derived
             {
               own = NameUtils.namemap_of_smap own;
               proto = NameUtils.namemap_of_smap proto;
               static = NameUtils.namemap_of_smap static;
             }
         )
      )

  let check_signature_compatibility cx def_reason x =
    check_super cx def_reason x;
    check_implements cx def_reason x;
    check_methods cx def_reason x

  (* TODO: Ideally we should check polarity for all class types, but this flag is
     flipped off for interface/declare class currently. *)
  let classtype cx ?(check_polarity = true) x =
    let (this_reason, this_instance_t) = this_instance_type cx x in
    let this = Type.(DefT (this_reason, InstanceT this_instance_t)) in
    let this_tparam = this_tparam x in
    let tparams_with_this =
      this_tparam |> Base.Option.value_map ~default:x.tparams ~f:(tparams_with_this x.tparams)
    in
    let this_name = Subst_name.Name "this" in
    begin
      match tparams_with_this with
      | Some (_, tps) when check_polarity ->
        (* TODO: use tparams_map instead of calculating this here *)
        let tparams =
          Nel.fold_left
            (fun acc tp -> Subst_name.Map.add tp.Type.name tp acc)
            Subst_name.Map.empty
            tps
        in
        (* Check delayed so that we don't have to force the currently unresolved OpenT
         * with implicit this tparam. *)
        Context.add_post_inference_polarity_check cx tparams Polarity.Positive this
      | _ -> ()
    end;
    let open TypeUtil in
    let (t_inner, t_outer) =
      if structural x then
        (this, class_type ~structural:true this)
      else
        ( Type.ThisInstanceT (this_reason, this_instance_t, true, this_name),
          class_type
            ~structural:false
            (Type.ThisInstanceT (this_reason, this_instance_t, false, this_name))
        )
    in
    let poly t = poly_type_of_tparams (Type.Poly.generate_id ()) x.tparams t in
    (t_inner, poly t_outer)

  let mk_class_binding _cx x = { Type.class_binding_id = x.id }

  let make_thises cx x =
    let open Type in
    let super_reason = update_desc_reason (fun d -> RSuperOf d) x.instance.reason in
    match x.super with
    | Interface _ -> failwith "tried to evaluate toplevel of interface"
    | Class { extends; this_t; _ } ->
      let (super, static_super) =
        match extends with
        | Explicit (annot_loc, c, targs) ->
          (* Eagerly specialize when there are no targs *)
          (* TODO: We can also specialize when there are targs, because this
             code is not instantiated. However, the type normalizer
             expects a PolyT here. *)
          let c =
            if targs = None then
              let use_op =
                Op (ClassExtendsCheck { def = TypeUtil.reason_of_t c; extends = x.instance.reason })
              in
              specialize cx use_op targs c
            else
              c
          in
          let t = TypeUtil.this_typeapp ~annot_loc c this_t targs in
          (t, TypeUtil.class_type ~annot_loc t)
        | Implicit { null } ->
          ( ( if null then
              NullProtoT super_reason
            else
              ObjProtoT super_reason
            ),
            FunProtoT super_reason
          )
      in

      (this_t, TypeUtil.class_type this_t, super, static_super)

  (* Processes the bodies of instance and static class members. *)
  let toplevels cx x =
    Type_env.in_class_scope cx x.class_loc (fun () ->
        let method_ ~set_asts f =
          let (params_ast, body_ast, init_ast) = F.toplevels cx f in
          Base.Option.iter init_ast ~f:(fun ((_, t), _) ->
              Context.add_missing_local_annot_lower_bound cx f.F.Types.ret_annot_loc t
          );
          set_asts (params_ast, body_ast, init_ast)
        in
        let field _name (_, _, value) =
          match value with
          | Annot _ -> ()
          | Infer (fsig, set_asts) -> method_ ~set_asts fsig
        in

        x
        |> with_sig ~static:true (fun s ->
               (* process static methods and fields *)
               iter_methods
                 (fun { id_loc = _; this_write_loc = _; func_sig; set_asts; set_type = _ } ->
                   method_ ~set_asts func_sig)
                 s;
               SMap.iter field s.fields;
               SMap.iter field s.private_fields
           );

        x
        |> with_sig ~static:false (fun s ->
               (* process constructor *)
               x.constructor
               |> List.iter
                    (fun { id_loc = _; this_write_loc = _; func_sig; set_asts; set_type = _ } ->
                      method_ ~set_asts func_sig
                  );
               (* process instance methods and fields *)
               iter_methods
                 (fun { id_loc = _; this_write_loc = _; func_sig; set_asts; set_type = _ } ->
                   method_ ~set_asts func_sig)
                 s;
               SMap.iter field s.fields;
               SMap.iter field s.private_fields;
               SMap.iter field s.proto_fields
           )
    )
end
