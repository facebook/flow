(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make
    (L : Loc_sig.S)
    (Scope_api : Scope_api_sig.S with module L = L)
    (Ssa_api : Ssa_api.S with module L = L) =
struct
  let is_const_like info values loc =
    let uses = Scope_api.uses_of_use info loc in
    (* We consider a binding to be const-like if all reads point to the same
     write, modulo initialization. *)
    let writes =
      L.LSet.fold
        (fun use acc ->
          match L.LMap.find_opt use values with
          | None -> (* use is a write *) acc
          | Some write_locs ->
            (* use is a read *)
            (* collect writes pointed to by the read, modulo initialization *)
            List.fold_left
              (fun acc -> function
                | Ssa_api.Uninitialized -> acc
                | Ssa_api.Write reason -> L.LSet.add (Reason.poly_loc_of_reason reason) acc)
              acc
              write_locs)
        uses
        L.LSet.empty
    in
    L.LSet.cardinal writes <= 1

  let is_not_captured_by_closure =
    let rec lookup in_current_var_scope info scope def =
      if SMap.exists (fun _ def' -> def = def') scope.Scope_api.Scope.defs then
        in_current_var_scope
      else
        match scope.Scope_api.Scope.parent with
        | None -> true
        | Some scope_id' ->
          let scope' = Scope_api.scope info scope_id' in
          lookup (in_current_var_scope && scope.Scope_api.Scope.lexical) info scope' def
    in
    fun info use ->
      let scopes = info.Scope_api.scopes in
      IMap.fold
        (fun _scope_id scope acc ->
          match L.LMap.find_opt use scope.Scope_api.Scope.locals with
          | Some def -> acc && lookup true info scope def
          | None -> acc)
        scopes
        true

  let written_by_closure info values loc =
    let uses = Scope_api.uses_of_use info loc in
    L.LSet.fold
      (fun use acc ->
        match L.LMap.find_opt use values with
        | None ->
          (* use is a write *)
          if is_not_captured_by_closure info use then
            acc
          else
            L.LSet.add use acc
        | Some _write_locs ->
          (* use is a read *)
          (* collect writes pointed to by the read, modulo initialization *)
          acc)
      uses
      L.LSet.empty

  (* Some variables are unhavocable by making a function call but still can be havoced in other
     situations. `via_call` indicates whether the havocing operation is a call or something
     else (resetting an activation scope, yielding, entering a new scope *)
  let should_invalidate ~all info values r =
    let loc = Reason.poly_loc_of_reason r in
    (not (is_const_like info values loc))
    && (all || (not @@ L.LSet.is_empty (written_by_closure info values loc)))
end

module With_ALoc = Make (Loc_sig.ALocS) (Scope_api.With_ALoc) (Ssa_api.With_ALoc)
module With_Loc = Make (Loc_sig.LocS) (Scope_api.With_Loc) (Ssa_api.With_Loc)
include With_ALoc
