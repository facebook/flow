(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Utils_js

exception EncounteredPlaceholderType

exception EncounteredUnresolvedTvar

let has_placeholders =
  let visitor =
    object (this)
      inherit [ISet.t] Type_visitor.t as super

      method! type_ cx pole seen t =
        match t with
        | AnyT (_, Placeholder) -> raise EncounteredPlaceholderType
        | t -> super#type_ cx pole seen t

      method! tvar cx pole seen _r id =
        let module C = Type.Constraint in
        let (root_id, constraints) = Context.find_constraints cx id in
        if ISet.mem root_id seen then
          seen
        else
          let seen = ISet.add root_id seen in
          match constraints with
          | C.FullyResolved _ -> seen
          | C.Resolved t -> this#type_ cx pole seen t
          | C.Unresolved bounds ->
            TypeMap.fold (fun t _ seen -> this#type_ cx pole seen t) bounds.C.lower seen
    end
  in
  fun cx t ->
    match visitor#type_ cx Polarity.Positive ISet.empty t with
    | exception EncounteredPlaceholderType -> true
    | _ -> false

let has_unresolved_tvars_visitor =
  object (this)
    inherit [ISet.t] Type_visitor.t

    method! tvar cx pole seen _r id =
      let module C = Type.Constraint in
      let (root_id, constraints) = Context.find_constraints cx id in
      if ISet.mem root_id seen then
        seen
      else
        let seen = ISet.add root_id seen in
        match constraints with
        | C.FullyResolved _ -> seen
        | C.Resolved t -> this#type_ cx pole seen t
        | C.Unresolved _ -> raise EncounteredUnresolvedTvar
  end

let has_unresolved_tvars cx t =
  match has_unresolved_tvars_visitor#type_ cx Polarity.Positive ISet.empty t with
  | exception EncounteredUnresolvedTvar -> true
  | _ -> false

let has_unresolved_tvars_in_destructors cx d =
  match has_unresolved_tvars_visitor#destructor cx ISet.empty d with
  | exception EncounteredUnresolvedTvar -> true
  | _ -> false

let default_no_lowers r =
  let desc =
    match desc_of_reason r with
    | RIdentifier (OrdinaryName x) -> RCustom (spf "`%s` (resolved to type `empty`)" x)
    | _ -> REmpty
  in
  EmptyT.make (replace_desc_reason desc r) (bogus_trust ())

class resolver ~no_lowers =
  object (this)
    inherit [ISet.t] Type_visitor.t

    method! tvar cx pole seen r id =
      let module C = Type.Constraint in
      let (root_id, _, root) = Context.find_root cx id in
      match root.C.constraints with
      | C.FullyResolved _ -> seen
      | _ when ISet.mem root_id seen -> seen
      | _ ->
        let t =
          match Flow_js_utils.merge_tvar_opt cx r root_id with
          | Some t -> Some t
          | None -> Some (no_lowers r)
        in
        Base.Option.value_map t ~default:seen ~f:(fun t ->
            let constraints =
              if Context.typing_mode cx <> Context.CheckingMode then
                C.Resolved t
              else
                C.FullyResolved (lazy t)
            in
            root.C.constraints <- constraints;
            let seen = ISet.add root_id seen in
            this#type_ cx pole seen t
        )

    method call_arg cx seen t =
      match t with
      | Arg t
      | SpreadArg t ->
        let _ = this#type_ cx Polarity.Positive seen t in
        seen
  end

let run_conditionally cx f =
  match Context.current_phase cx with
  | Context.InitLib -> ()
  | _ -> ignore @@ f ()

let resolve ?(no_lowers = default_no_lowers) cx t =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      resolver#type_ cx Polarity.Positive ISet.empty t
  )

let resolved_t ?(no_lowers = default_no_lowers) cx t =
  resolve ~no_lowers cx t;
  t

let mk_tvar_and_fully_resolve_where cx reason f =
  let tvar = Tvar.mk_where cx reason f in
  resolve cx tvar;
  tvar

let mk_tvar_and_fully_resolve_no_wrap_where cx reason f =
  let tvar = Tvar.mk_no_wrap_where cx reason f in
  resolve cx tvar;
  tvar
