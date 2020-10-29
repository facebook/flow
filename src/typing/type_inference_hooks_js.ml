(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let id_nop _ _ _ = false

let lval_nop _ _ _ _ = ()

let member_nop _ _ _ _ = false

let call_nop _ _ _ _ = ()

let jsx_nop _ _ _ _ = false

let ref_nop _ _ _ = ()

let class_member_decl_nop _ _ _ _ _ = ()

let obj_prop_decl_nop _ _ _ = false

let obj_type_prop_decl_nop _ _ _ = ()

let obj_to_obj_nop _ _ _ = ()

let instance_to_obj_nop _ _ _ = ()

let export_named_nop _ _ = ()

(* This type represents the possible definition-points for an lvalue. *)
type def =
  (*
   * Given a variable declaration such as:
   *
   *   var a = 42; // <-- this
   *   var b; // <-- this
   *
   * We emit the type of the variable, given as an annotation or inferred by
   * looking at its assignments, as the "definition" for the lvalue.
   *)
  | Val of Type.t
  (*
   * Given a destructuring pattern for an initialization such as:
   *
   *  var {
   *    a // <-- this
   *  } = {a: {b: 42}};
   *
   * We emit the tvar for the "parent pattern" being destructured so that we
   * may extract the type of the corresponding property -- and thus the location
   * of that type -- as the "definition" for the binding generated from a
   * destructuring pattern.
   *)
  | Parent of Type.t
  (*
   * For assignments, we consider lvalues to have the same "definition" as
   * corresponding rvalues: both kinds of references point to the declaration site.
   *)
  | Id

type hook_state_t = {
  id_hook: Context.t -> string -> ALoc.t -> bool;
  lval_hook: Context.t -> string -> ALoc.t -> def -> unit;
  member_hook: Context.t -> string -> ALoc.t -> Type.t -> bool;
  (* TODO: This is inconsistent with the way the id/member hooks work, but we
     currently don't need a way to override call types, so it simplifies
     things a bit *)
  call_hook: Context.t -> string -> ALoc.t -> Type.t -> unit;
  jsx_hook: Context.t -> string -> ALoc.t -> Type.t -> bool;
  class_member_decl_hook:
    Context.t -> Type.t (* self *) -> bool (* static *) -> string -> ALoc.t -> unit;
  obj_prop_decl_hook: Context.t -> string -> ALoc.t -> bool;
  obj_type_prop_decl_hook: Context.t -> string -> ALoc.t -> unit;
  (* Called when ObjT 1 ~> ObjT 2 *)
  obj_to_obj_hook: Context.t -> Type.t (* ObjT 1 *) -> Type.t (* ObjT 2 *) -> unit;
  (* Called when InstanceT ~> ObjT *)
  instance_to_obj_hook: Context.t -> Type.t (* InstanceT *) -> Type.t (* ObjT *) -> unit;
  (* Dispatched with "default" for default exports *)
  export_named_hook: string (* name *) -> ALoc.t -> unit;
}

let nop_hook_state =
  {
    id_hook = id_nop;
    lval_hook = lval_nop;
    member_hook = member_nop;
    call_hook = call_nop;
    jsx_hook = jsx_nop;
    class_member_decl_hook = class_member_decl_nop;
    obj_prop_decl_hook = obj_prop_decl_nop;
    obj_type_prop_decl_hook = obj_type_prop_decl_nop;
    obj_to_obj_hook = obj_to_obj_nop;
    instance_to_obj_hook = instance_to_obj_nop;
    export_named_hook = export_named_nop;
  }

let hook_state = ref nop_hook_state

let set_id_hook hook = hook_state := { !hook_state with id_hook = hook }

let set_lval_hook hook = hook_state := { !hook_state with lval_hook = hook }

let set_member_hook hook = hook_state := { !hook_state with member_hook = hook }

let set_call_hook hook = hook_state := { !hook_state with call_hook = hook }

let set_jsx_hook hook = hook_state := { !hook_state with jsx_hook = hook }

let set_class_member_decl_hook hook =
  hook_state := { !hook_state with class_member_decl_hook = hook }

let set_obj_prop_decl_hook hook = hook_state := { !hook_state with obj_prop_decl_hook = hook }

let set_obj_type_prop_decl_hook hook =
  hook_state := { !hook_state with obj_type_prop_decl_hook = hook }

let set_obj_to_obj_hook hook = hook_state := { !hook_state with obj_to_obj_hook = hook }

let set_instance_to_obj_hook hook = hook_state := { !hook_state with instance_to_obj_hook = hook }

let set_export_named_hook hook = hook_state := { !hook_state with export_named_hook = hook }

let reset_hooks () = hook_state := nop_hook_state

let dispatch_id_hook cx name loc = !hook_state.id_hook cx name loc

let dispatch_lval_hook cx name lhs_loc rhs_loc = !hook_state.lval_hook cx name lhs_loc rhs_loc

let dispatch_member_hook cx name loc this_t = !hook_state.member_hook cx name loc this_t

let dispatch_call_hook cx name loc this_t = !hook_state.call_hook cx name loc this_t

let dispatch_jsx_hook cx name loc this_t = !hook_state.jsx_hook cx name loc this_t

let dispatch_class_member_decl_hook cx self static name loc =
  !hook_state.class_member_decl_hook cx self static name loc

let dispatch_obj_prop_decl_hook cx name loc = !hook_state.obj_prop_decl_hook cx name loc

let dispatch_obj_type_prop_decl_hook cx name loc = !hook_state.obj_type_prop_decl_hook cx name loc

let dispatch_obj_to_obj_hook cx t1 t2 = !hook_state.obj_to_obj_hook cx t1 t2

let dispatch_instance_to_obj_hook cx t1 t2 = !hook_state.instance_to_obj_hook cx t1 t2

let dispatch_export_named_hook loc = !hook_state.export_named_hook loc
