(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Utils_js

let default_no_lowers r =
  let desc =
    match desc_of_reason r with
    | RIdentifier (OrdinaryName x) -> RCustom (spf "`%s` (resolved to type `empty`)" x)
    | _ -> REmpty
  in
  EmptyT.make (replace_desc_reason desc r)

class resolver ~no_lowers ~filter_empty =
  object (this)
    inherit [bool IMap.t * bool] Type_visitor.t as super

    method! type_ cx pole seen t =
      match t with
      | AnyT (_, Placeholder) ->
        let (seen_tvars, _) = seen in
        (seen_tvars, true)
      | _ -> super#type_ cx pole seen t

    method! tvar cx pole ((seen_tvars, seen_placeholders) as seen) r id =
      let module C = Type.Constraint in
      let (root_id, _, root) = Context.find_root cx id in
      match root.C.constraints with
      | C.FullyResolved _ -> seen
      | C.Unresolved _
      | C.Resolved _ ->
        (match IMap.find_opt root_id seen_tvars with
        | Some seen_placeholders ->
          (* Case 1: We have already resolved the tvar earlier.
           * In this case, seen_placeholders in the map is the answer.
           *
           * Case 2: We are in the process of resolving a cyclic tvar
           * At this point, seen_placeholders in the map will be false. If after visiting the entire
           * type we do find placeholders, it will be OR-ed to true eventually.
           *)
          (seen_tvars, seen_placeholders)
        | None ->
          let seen_tvars = IMap.add root_id false seen_tvars in
          let t =
            match Flow_js_utils.merge_tvar_opt ~filter_empty cx r root_id with
            | Some t -> t
            | None -> no_lowers r
          in
          let (seen_tvars, seen_placeholders_in_t) = this#type_ cx pole (seen_tvars, false) t in
          let seen_tvars = IMap.add root_id seen_placeholders_in_t seen_tvars in
          let constraints =
            if seen_placeholders_in_t then
              C.Resolved t
            else
              C.FullyResolved (C.ForcingState.of_non_lazy_t t)
          in
          root.C.constraints <- constraints;
          (seen_tvars, seen_placeholders || seen_placeholders_in_t))
  end

let resolve cx ?(no_lowers = default_no_lowers) ?(filter_empty = true) t =
  let resolver = new resolver ~no_lowers ~filter_empty in
  let (_ : bool IMap.t * bool) = resolver#type_ cx Polarity.Positive (IMap.empty, false) t in
  ()

let resolved_t ?(no_lowers = default_no_lowers) ?(filter_empty = true) cx t =
  resolve ~no_lowers ~filter_empty cx t;
  t

let mk_tvar_and_fully_resolve_where cx reason f =
  let tvar = Tvar.mk_where cx reason f in
  resolve cx tvar;
  tvar

let mk_tvar_and_fully_resolve_no_wrap_where cx reason f =
  let tvar = Tvar.mk_no_wrap_where cx reason f in
  resolve cx tvar;
  tvar
