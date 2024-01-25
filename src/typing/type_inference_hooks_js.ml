(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let id_nop _ _ _ = false

let literal_nop _ _ = false

let member_nop _ _ _ _ = false

let jsx_nop _ _ _ = false

let ref_nop _ _ _ = ()

let obj_prop_decl_nop _ _ _ = false

let obj_to_obj_nop _ _ _ = ()

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
  literal_hook: Context.t -> ALoc.t -> bool;
  jsx_hook: Context.t -> string -> ALoc.t -> bool;
  obj_prop_decl_hook: Context.t -> string -> ALoc.t -> bool;
  (* Called when ObjT 1 ~> ObjT 2 *)
  obj_to_obj_hook: Context.t -> Type.t (* ObjT 1 *) -> Type.t (* ObjT 2 *) -> unit;
}

let nop_hook_state =
  {
    id_hook = id_nop;
    literal_hook = literal_nop;
    jsx_hook = jsx_nop;
    obj_prop_decl_hook = obj_prop_decl_nop;
    obj_to_obj_hook = obj_to_obj_nop;
  }

let hook_state = ref nop_hook_state

let set_id_hook hook = hook_state := { !hook_state with id_hook = hook }

let set_literal_hook hook = hook_state := { !hook_state with literal_hook = hook }

let set_jsx_hook hook = hook_state := { !hook_state with jsx_hook = hook }

let set_obj_prop_decl_hook hook = hook_state := { !hook_state with obj_prop_decl_hook = hook }

let set_obj_to_obj_hook hook = hook_state := { !hook_state with obj_to_obj_hook = hook }

let reset_hooks () = hook_state := nop_hook_state

let dispatch_id_hook cx name loc = !hook_state.id_hook cx name loc

let dispatch_literal_hook cx loc = !hook_state.literal_hook cx loc

let dispatch_jsx_hook cx name loc = !hook_state.jsx_hook cx name loc

let dispatch_obj_prop_decl_hook cx name loc = !hook_state.obj_prop_decl_hook cx name loc

let dispatch_obj_to_obj_hook cx t1 t2 = !hook_state.obj_to_obj_hook cx t1 t2
