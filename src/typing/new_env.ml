(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Type
open Reason
open Loc_collections

(************************)
(* Helpers **************)
(************************)

let find_var { Env_api.env_values; _ } loc = ALocMap.find loc env_values

let find_refi { Env_api.refinement_of_id; _ } = refinement_of_id

let _is_provider { Env_api.providers; _ } = Env_api.Provider_api.is_provider providers

let _find_providers { Env_api.providers; _ } loc =
  Env_api.Provider_api.providers_of_def providers loc
  |> Base.Option.value ~default:[]
  |> Base.List.map ~f:Reason.aloc_of_reason
(*************)
(*  Reading  *)
(*************)

(** Computes the phi type for a node given all its lower bounds
 *  Currently, this just produces a new type variable with the types of
 *  all the incoming writes as lower bounds. In the future, however, we
 *  may want to compute a more specific least upper bound for these writes.
 *)
let phi cx reason ts =
  Tvar.mk_where cx reason (fun tvar -> Base.List.iter ts ~f:(fun t -> Flow_js.flow_t cx (t, tvar)))

let rec predicate_of_refinement cx =
  Env_api.Refi.(
    function
    | AndR (r1, r2) -> AndP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
    | OrR (r1, r2) -> OrP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
    | NotR r -> NotP (predicate_of_refinement cx r)
    | TruthyR loc -> ExistsP (Some loc)
    | NullR -> NullP
    | UndefinedR -> VoidP
    | MaybeR -> MaybeP
    | InstanceOfR loc ->
      (* Instanceof refinements store the loc they check against, which is a read in the env *)
      let reason = mk_reason (RCustom "RHS of `instanceof` operator") loc in
      let t = read cx loc reason in
      Flow_js.flow cx (t, AssertInstanceofRHST reason);
      LeftP (InstanceofTest, t)
    | IsArrayR -> ArrP
    | BoolR loc -> BoolP loc
    | FunctionR -> FunP
    | NumberR loc -> NumP loc
    | ObjectR -> ObjP
    | StringR loc -> StrP loc
    | SymbolR loc -> SymbolP loc
    | SingletonBoolR { loc; sense = _; lit } -> SingletonBoolP (loc, lit)
    | SingletonStrR { loc; sense; lit } -> SingletonStrP (loc, sense, lit)
    | SingletonNumR { loc; sense; lit } -> SingletonNumP (loc, sense, lit)
    | SentinelR (prop, loc) ->
      PropExistsP (prop, mk_reason (RProperty (Some (OrdinaryName prop))) loc))

and refine cx reason loc refi t =
  Base.Option.value_map
    ~f:(fun predicate ->
      let predicate = predicate |> snd |> predicate_of_refinement cx in
      let reason = mk_reason (RRefined (desc_of_reason reason)) loc in
      Tvar.mk_no_wrap_where cx reason (fun tvar ->
          Flow_js.flow cx (t, PredicateT (predicate, tvar))))
    ~default:t
    refi

and read cx loc reason =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  let rec type_of_state states refi =
    Base.List.map
      ~f:(function
        | Env_api.With_ALoc.Uninitialized -> Type.(VoidT.at loc |> with_trust Trust.bogus_trust)
        | Env_api.With_ALoc.Write reason ->
          Debug_js.Verbose.print_if_verbose
            cx
            [
              spf
                "reading %s from location %s"
                (ALoc.debug_to_string loc)
                (Reason.aloc_of_reason reason |> ALoc.debug_to_string);
            ];
          Base.Option.value_exn (Reason.aloc_of_reason reason |> Loc_env.find_write env)
        | Env_api.With_ALoc.Refinement { refinement_id; writes } ->
          find_refi var_info refinement_id |> Base.Option.some |> type_of_state writes
        | Env_api.With_ALoc.Global name -> Flow_js.get_builtin cx (Reason.OrdinaryName name) reason)
      states
    |> phi cx reason
    |> refine cx reason loc refi
  in
  let var_state = find_var var_info loc in
  let t = type_of_state var_state None in
  Flow_js.reposition cx loc t

(************************)
(* Variable Declaration *)
(************************)

let initialize_all cx =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  Base.List.fold
    ~f:(fun env reason ->
      let loc = aloc_of_reason reason in
      let t = Inferred (Tvar.mk cx reason) in
      (* Treat everything as inferred for now for the purposes of annotated vs inferred *)
      Loc_env.initialize env loc t)
    var_info.Env_api.env_entries
    ~init:env
  |> Context.set_environment cx
