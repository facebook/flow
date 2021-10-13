(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_ALoc
module Ssa_api = Ssa_api.With_ALoc
module Provider_api = Provider_api.ALocProviders
open Loc_collections

let declaration_locs_of_constrained_write_error cx error =
  let tables = Context.aloc_tables cx in
  let rec find_constrained_writes op =
    let open Type in
    match op with
    | Frame (ConstrainedAssignment { declaration; _ }, op) ->
      LocSet.add (ALoc.to_loc_with_tables tables declaration) (find_constrained_writes op)
    | Op (Speculation op)
    | Frame (_, op) ->
      find_constrained_writes op
    | _ -> LocSet.empty
  in
  let msg = Flow_error.msg_of_error error in
  let use_op_opt = Error_message.util_use_op_of_msg None (fun op _ -> Some op) msg in
  Base.Option.value_map ~f:find_constrained_writes ~default:LocSet.empty use_op_opt

(* An assignment A to a variable X is trivially extractable into a const if:
  1. A is not a provider for X (if it is a provider, it may be used to constrain writes to X later on)
  2. All reads of X either reach *only* A, and no other assignments, OR they reach a set of writes that do not
     include A.
*)
let is_extractable_assignment cx relevant_declarations =
  let { Loc_env.var_info = { Env_api.ssa_values; scopes; providers; _ }; _ } =
    Context.environment cx
  in
  let tables = Context.aloc_tables cx in
  let loc_of_aloc = ALoc.to_loc_with_tables tables in
  fun loc ->
    match Scope_api.def_of_use_opt scopes loc with
    | Some ({ Scope_api.Def.locs = (declaration_loc, _); _ } as def)
      when LocSet.mem (loc_of_aloc declaration_loc) relevant_declarations ->
      let uses = Scope_api.uses_of_def scopes def in
      let is_renamable =
        ALocSet.for_all
          (fun use ->
            match ALocMap.find_opt use ssa_values with
            | None -> (* use is a write, ignore *) true
            | Some writes ->
              (* Either the write that reaches the read is *exactly* this location, or it is unrelated *)
              let locs =
                Base.List.filter_map
                  ~f:(function
                    | Ssa_api.Uninitialized -> None
                    | Ssa_api.Write reason -> Some (Reason.aloc_of_reason reason))
                  writes
              in
              begin
                match locs with
                | [loc'] when ALoc.concretize_equal tables loc' loc -> true
                | locs -> not (Base.List.mem ~equal:(ALoc.concretize_equal tables) locs loc)
              end)
          uses
      in
      let is_provider =
        Provider_api.providers_of_def providers loc
        |> Base.Option.value_map ~default:false ~f:(fun (_, providers) ->
               Base.List.exists providers ~f:(fun r ->
                   Reason.poly_loc_of_reason r |> ALoc.concretize_equal tables loc))
      in
      is_renamable && not is_provider
    | _ -> false
