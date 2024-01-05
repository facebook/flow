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

class has_unresolved_tvars_visitor =
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

let has_unresolved_tvars_or_placeholders_visitor =
  object
    inherit has_unresolved_tvars_visitor as super

    method! type_ cx pole seen t =
      match t with
      | AnyT (_, Placeholder) -> raise EncounteredPlaceholderType
      | t -> super#type_ cx pole seen t
  end

let has_unresolved_tvars_visitor = new has_unresolved_tvars_visitor

let has_unresolved_tvars_or_placeholders cx t =
  match has_unresolved_tvars_or_placeholders_visitor#type_ cx Polarity.Positive ISet.empty t with
  | exception EncounteredUnresolvedTvar -> true
  | exception EncounteredPlaceholderType -> true
  | _ -> false

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
  EmptyT.make (replace_desc_reason desc r)

(* The resolver accummulator consists of the usual "seen" set and an "immediate"
 * set of tvar ids. This latter set is intended to detect trivial cycles of the
 * form:
 *
 *   OpenT (_, id0)
 *   id0 -> AnnotT (_, OpenT (_, id1))
 *   id1 -> AnnotT (_, OpenT (_, id0))
 *
 * These can arise for example from code like the following
 *
 *   const x: X = {};
 *   type X = typeof x;
 *
 * and are currently not detectable by the name_def_ordering checks since the cycle
 * involves an annotation.
 *)
class resolver ~no_lowers =
  object (this)
    inherit [ISet.t * ISet.t] Type_visitor.t as super

    method! tvar cx pole (seen, immediate) r id =
      let module C = Type.Constraint in
      let (root_id, _, root) = Context.find_root cx id in
      if ISet.mem root_id immediate then (
        root.C.constraints <- C.FullyResolved (lazy (no_lowers r));
        (seen, immediate)
      ) else
        let immediate = ISet.add root_id immediate in
        match root.C.constraints with
        | C.FullyResolved _ -> (seen, ISet.empty)
        | _ when ISet.mem root_id seen -> (seen, ISet.empty)
        | _ ->
          let t =
            match Flow_js_utils.merge_tvar_opt cx r root_id with
            | Some t -> t
            | None -> no_lowers r
          in
          let constraints =
            if Context.typing_mode cx <> Context.CheckingMode then
              C.Resolved t
            else
              C.FullyResolved (lazy t)
          in
          root.C.constraints <- constraints;
          let seen = ISet.add root_id seen in
          this#type_ cx pole (seen, immediate) t

    method call_arg cx (seen, (_ : ISet.t)) t =
      match t with
      | Arg t
      | SpreadArg t ->
        let _ = this#type_ cx Polarity.Positive (seen, ISet.empty) t in
        (seen, ISet.empty)

    method! type_ cx pole (seen, immediate) =
      function
      | OpenT (r, id) -> this#tvar cx pole (seen, immediate) r id
      | AnnotT (_, t, _) -> this#type_ cx Polarity.Positive (seen, immediate) t
      | t -> super#type_ cx pole (seen, ISet.empty) t
  end

let resolve ?(no_lowers = default_no_lowers) cx t =
  let resolver = new resolver ~no_lowers in
  let (_ : ISet.t * ISet.t) = resolver#type_ cx Polarity.Positive (ISet.empty, ISet.empty) t in
  ()

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
