(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(* The function that is called every time we encounter a definition or a
   reference to it. In the special case where we are at the point the user is
   interested in, we record the location of the definition, which is used to
   recover results later.

   Let pos be the location of the point for which we want to find all
   references. References belong to a set whose representative is the definition
   they all point to. (For convenience we also include the definition itself in
   the set of its references.) Let ref_loc be the location of such a reference,
   and def_loc be the location of the definition it points to.

   We maintain a table of bindings from locations of definitions to locations of
   references pointing to them.
*)
let def (state, pos) cx def_loc ref_loc =
  if Reason.in_range pos ref_loc
  then state := Some def_loc;
  let table = Context.refs_table cx in
  Hashtbl.add table def_loc ref_loc

(* To recover results, we simply look up the entries in the table corresponding
   to the location of the definition the user is interested in. (Note that the
   user may be pointing at one of the references to that definition, but by the
   above scheme we will recover all other references as well, including the
   definition itself.) *)
let result cx state =
  match !state with
  | Some def_loc ->
    Hashtbl.find_all (Context.refs_table cx) def_loc
    |> List.sort_uniq Loc.compare
  | _ ->
    []

let set_hooks pos =
  let state = ref None in
  Type_inference_hooks_js.set_ref_hook (def (state, pos));
  state

let unset_hooks () =
    Type_inference_hooks_js.reset_hooks ()
