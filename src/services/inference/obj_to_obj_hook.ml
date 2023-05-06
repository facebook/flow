(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* If the given type refers to an object literal, return the location of the object literal.
 * Otherwise return None *)
let get_object_literal_loc ty : ALoc.t option =
  let open TypeUtil in
  let open Reason in
  let reason_desc = reason_of_t ty (* TODO look into unwrap *) |> desc_of_reason ~unwrap:false in
  match reason_desc with
  | RObjectLit -> Some (def_loc_of_t ty)
  | _ -> None

let with_obj_to_obj_hook ~enabled ~loc_of_aloc ~f =
  let obj_to_obj_map = ref Loc_collections.LocMap.empty in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    match get_object_literal_loc obj1 with
    | Some obj1_aloc ->
      let open Type in
      (match (obj1, obj2) with
      | (DefT (_, _, ObjT _), DefT (_, _, ObjT { props_tmap = obj2_props_id; _ })) ->
        obj_to_obj_map :=
          Loc_collections.LocMap.adjust
            (loc_of_aloc obj1_aloc)
            (function
              | None -> Properties.Set.add obj2_props_id Properties.Set.empty
              | Some ids -> Properties.Set.add obj2_props_id ids)
            !obj_to_obj_map
      | _ -> ())
    | _ -> ()
  in
  if enabled then Type_inference_hooks_js.set_obj_to_obj_hook obj_to_obj_hook;
  let result = f () in
  Type_inference_hooks_js.reset_hooks ();
  (result, !obj_to_obj_map)
