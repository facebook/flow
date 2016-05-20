module Anno = Type_annotation
module Ast = Spider_monkey_ast
module Flow = Flow_js

open Reason_js

type field = Type.t * Ast.Expression.t option

type method_kind = Method | Getter | Setter

type signature = {
  reason: reason;
  super: Type.t;
  fields: field SMap.t;
  (* Multiple function signatures indicates an overloaded method. Note that
     function signatures are stored in reverse definition order. *)
  methods: Func_sig.t list SMap.t;
  getters: Func_sig.t SMap.t;
  setters: Func_sig.t SMap.t;
  (* Track the order of method declarations, so they can be processed in the
     same order. The order shouldn't necessarily matter, but does currently at
     least due to a bug (facebook/flow/issues#1745). Note that this list is in
     reverse order. *)
  method_decls: (method_kind * string) list;
}

type t = {
  id: int;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  (* Multiple function signatures indicates an overloaded constructor. Note that
     function signatures are stored in reverse definition order. *)
  constructor: Func_sig.t list;
  static: signature;
  instance: signature;
}

let fold_pipe fn lst acc = List.fold_left fn acc lst

let empty id reason tparams tparams_map super =
  let empty_sig reason super = {
    reason; super;
    fields = SMap.empty;
    methods = SMap.empty;
    getters = SMap.empty;
    setters = SMap.empty;
    method_decls = [];
  } in
  let constructor = [] in
  let static =
    let super = Type.ClassT super in
    let reason = prefix_reason "statics of " reason in
    empty_sig reason super
  in
  let instance = empty_sig reason super in
  { id; tparams; tparams_map; constructor; static; instance }

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
  let fsig = Func_sig.default_constructor s.tparams_map reason in
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
  method_decls = (Method, name)::s.method_decls;
})

(* Appending a method builds a list of function signatures. This implements the
   behavior of interfaces and declared classes, which interpret duplicate
   definitions as branches of a single overloaded method. *)
let append_method name fsig = map_sig (fun s ->
  let methods = match SMap.get name s.methods with
  | Some fsigs -> SMap.add name (fsig::fsigs) s.methods
  | None -> SMap.add name [fsig] s.methods
  in
  let method_decls = (Method, name)::s.method_decls in
  {s with methods; method_decls}
)

let add_getter name fsig = map_sig (fun s -> {
  s with
  getters = SMap.add name fsig s.getters;
  methods = SMap.remove name s.methods;
  method_decls = (Getter, name)::s.method_decls;
})

let add_setter name fsig = map_sig (fun s -> {
  s with
  setters = SMap.add name fsig s.setters;
  methods = SMap.remove name s.methods;
  method_decls = (Setter, name)::s.method_decls;
})

let mk_method cx x reason func =
  Func_sig.mk cx x.tparams_map reason func

let mk_field cx x reason typeAnnotation value =
  let t = Anno.mk_type_annotation cx x.tparams_map reason typeAnnotation in
  (t, value)

let add_name reason class_sig =
  let reason = prefix_reason "`name` property of" reason in
  let t = Type.StrT.why reason in
  add_field ~static:true "name" (t, None) class_sig

let mem_constructor {constructor; _} = constructor <> []

(* visits all methods, getters, and setters in declaration order *)
let iter_methods f {methods; getters; setters; method_decls; _} =
  let rec loop methods = function
    | [] -> ()
    | (kind, name)::decls ->
      let x, methods = match kind with
      | Method ->
        let m = SMap.find_unsafe name methods in
        let methods = SMap.add name (List.tl m) methods in
        List.hd m, methods
      | Getter ->
        SMap.find_unsafe name getters, methods
      | Setter ->
        SMap.find_unsafe name setters, methods
      in
      f x; loop methods decls
  in
  let methods = SMap.map List.rev methods in
  let method_decls = List.rev method_decls in
  loop methods method_decls
