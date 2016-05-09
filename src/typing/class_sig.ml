module Flow = Flow_js

open Reason_js

type signature = {
  reason: reason;
  super: Type.t;
  fields: Type.t SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: Func_sig.t list SMap.t;
  getters: Func_sig.t SMap.t;
  setters: Func_sig.t SMap.t;
}

type t = {
  id: int;
  structural: bool;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: Func_sig.t list;
  static: signature;
  instance: signature;
}

let empty ?(structural=false) id reason super =
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
    let reason = prefix_reason "statics of " reason in
    empty_sig reason super
  in
  let instance = empty_sig reason super in
  { id; structural; constructor; static; instance }

let map_sig ~static f s =
  if static
  then {s with static = f s.static}
  else {s with instance = f s.instance}

let with_sig ~static f s =
  if static then f s.static else f s.instance

let mutually f = (f ~static:true, f ~static:false)

let add_field name t = map_sig (fun s -> {
  s with
  fields = SMap.add name t s.fields;
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

(* Adding a method *overwrites* any existing methods. This implements the
   behavior of classes, which permit duplicate definitions where latter
   definitions overwrite former ones. *)
let add_method name fsig = map_sig (fun s -> {
  s with
  methods = SMap.add name [fsig] s.methods;
  getters = SMap.remove name s.getters;
  setters = SMap.remove name s.setters;
})

(* Appending a method builds a list of function signatures. This implements the
   bahvior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method name fsig = map_sig (fun s ->
  let methods = match SMap.get name s.methods with
  | Some fsigs -> SMap.add name (fsig::fsigs) s.methods
  | None -> SMap.add name [fsig] s.methods
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

let find_unsafe f name = with_sig (fun s -> SMap.find_unsafe name (f s))
let find_field_unsafe = find_unsafe (fun s -> s.fields)
let find_constructor_unsafe s = List.hd s.constructor
let find_method_unsafe name ~static s =
  find_unsafe (fun s -> s.methods) ~static name s |> List.hd
let find_getter_unsafe = find_unsafe (fun s -> s.getters)
let find_setter_unsafe = find_unsafe (fun s -> s.setters)

let mem_constructor {constructor; _} = constructor <> []

let subst cx map s =
  let map_sig s = {
    reason = s.reason;
    super = Flow.subst cx map s.super;
    fields = SMap.map (Flow.subst cx map) s.fields;
    methods = SMap.map (List.map (Func_sig.subst cx map)) s.methods;
    getters = SMap.map (Func_sig.subst cx map) s.getters;
    setters = SMap.map (Func_sig.subst cx map) s.setters;
  } in
  let constructor = List.map (Func_sig.subst cx map) s.constructor in
  let static = map_sig s.static in
  let instance = map_sig s.instance in
  {s with constructor; static; instance}

let elements cx ?constructor = with_sig (fun s ->
  let methods =
    (* If this is an overloaded method, create an intersection, attributed
       to the first declared function signature. If there is a single
       function signature for this method, simply return the method type. *)
    SMap.map Type.(fun xs ->
      match List.rev_map Func_sig.methodtype xs with
      | [] -> EmptyT.t
      | [t] -> t
      | t::_ as ts -> IntersectionT (reason_of_t t, InterRep.make ts)
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
  let getters_and_setters = SMap.fold (fun name t ts ->
    match SMap.get name ts with
    | Some t' -> Flow.unify cx t t'; ts
    | None -> SMap.add name t ts
  ) setters getters in

  (* Treat getters and setters as fields *)
  let fields = SMap.union getters_and_setters s.fields in

  fields, methods
)

let insttype ~static cx type_args arg_polarities s =
  let class_id = if static then 0 else s.id in
  let constructor = if static then None else
    let ts = List.rev_map Func_sig.methodtype s.constructor in
    match ts with
    | [] -> None
    | [t] -> Some t
    | t::_ as ts ->
      let open Type in
      let t = IntersectionT (reason_of_t t, InterRep.make ts) in
      Some t
  in
  let fields, methods = elements ?constructor ~static cx s in
  { Type.
    class_id;
    type_args;
    arg_polarities;
    fields_tmap = Flow.mk_propmap cx fields;
    methods_tmap = Flow.mk_propmap cx methods;
    mixins = false;
    structural = s.structural;
  }

let check_super ~static cx type_args arg_polarities s =
  let reason = s.instance.reason in
  let super = with_sig ~static (fun s -> s.super) s in
  let insttype = insttype ~static cx type_args arg_polarities s in
  Flow.flow cx (super, Type.SuperT (reason, insttype))

let this_instance cx type_args arg_polarities s =
  let static_insttype, insttype =
    mutually (insttype cx type_args arg_polarities s)
  in
  let static =
    let {reason; super; _} = s.static in
    Type.InstanceT (reason, Type.MixedT.t, super, static_insttype)
  in
  let {reason; super; _} = s.instance in
  Type.InstanceT (reason, static, super, insttype)
