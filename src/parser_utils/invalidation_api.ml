(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make
    (L : Loc_sig.S)
    (Scope_api : Scope_api_sig.S with module L = L)
    (Ssa_api : Ssa_api.S with module L = L)
    (Provider_api : Provider_api.S with module L = L) =
struct
  let is_const_like info values loc =
    try
      let { Scope_api.Def.locs = def_locs; _ } = Scope_api.def_of_use info loc in
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
      match L.LSet.choose_opt writes with
      | None -> true
      | Some loc -> L.LSet.cardinal writes <= 1 && Nel.mem ~equal:L.equal loc def_locs
    with
    (* TODO: anywhere where we would raise a Missing_def
       error is likely a bug with the Scope_builder that
       should be fixed for the SSA environment to work *)
    | Scope_api.Missing_def _ -> false

  type initialization_valid =
    | Valid
    | NotWritten
    | NullWritten of L.t

  let declaration_validity info values providers loc =
    match Provider_api.providers_of_def providers loc with
    | None -> Valid
    | Some (true, _ :: _) -> Valid
    | Some (_, providers) ->
      (try
         let null_providers =
           (* Since this variable is not fully initialized, if there are any providers then
              they must be null providers *)
           Base.List.map ~f:Reason.poly_loc_of_reason providers |> L.LSet.of_list
         in
         let uses = Scope_api.uses_of_use info loc in
         let reads_exist =
           L.LSet.exists (fun use -> L.LMap.find_opt use values |> Base.Option.is_some) uses
         in
         match (reads_exist, not (L.LSet.is_empty null_providers)) with
         | (false, _) -> Valid
         | (true, false) -> NotWritten
         | (true, true) -> NullWritten (L.LSet.min_elt null_providers)
       with
      (* TODO: anywhere where we would raise a Missing_def
         error is likely a bug with the Scope_builder that
         should be fixed for the SSA environment to work *)
      | Scope_api.Missing_def _ -> Valid)

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

  let mk_caches () = (ref L.LMap.empty, ref L.LMap.empty)

  let should_invalidate ~all (const_like_cache, written_by_closure_cache) info values loc =
    let const_like =
      match L.LMap.find_opt loc !const_like_cache with
      | Some b -> b
      | None ->
        let b = is_const_like info values loc in
        const_like_cache := L.LMap.add loc b !const_like_cache;
        b
    in
    if const_like then
      false
    else if all then
      true
    else
      let not_written_by_closure =
        match L.LMap.find_opt loc !written_by_closure_cache with
        | Some b -> b
        | None ->
          let b = L.LSet.is_empty (written_by_closure info values loc) in
          written_by_closure_cache := L.LMap.add loc b !written_by_closure_cache;
          b
      in
      not not_written_by_closure
end

module With_ALoc =
  Make (Loc_sig.ALocS) (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Provider_api.ALocProviders)
module With_Loc =
  Make (Loc_sig.LocS) (Scope_api.With_Loc) (Ssa_api.With_Loc) (Provider_api.LocProviders)
include With_ALoc
