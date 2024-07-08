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

class resolver ~fully_resolve ~no_lowers ~filter_empty =
  object (this)
    inherit [ISet.t] Type_visitor.t as _super

    method! tvar cx pole seen r id =
      let module C = Type.Constraint in
      let (root_id, _, root) = Context.find_root cx id in
      match root.C.constraints with
      | C.FullyResolved _ -> seen
      | _ when ISet.mem root_id seen -> seen
      | _ ->
        let t =
          match Flow_js_utils.merge_tvar_opt ~filter_empty cx r root_id with
          | Some t -> t
          | None -> no_lowers r
        in
        let constraints =
          if fully_resolve then
            C.FullyResolved (C.ForcingState.of_non_lazy_t t)
          else
            C.Resolved t
        in
        root.C.constraints <- constraints;
        let seen = ISet.add root_id seen in
        this#type_ cx pole seen t
  end

let resolve
    cx
    ?(fully_resolve = Context.typing_mode cx = Context.CheckingMode)
    ?(no_lowers = default_no_lowers)
    ?(filter_empty = true)
    t =
  let resolver = new resolver ~fully_resolve ~no_lowers ~filter_empty in
  let (_ : ISet.t) = resolver#type_ cx Polarity.Positive ISet.empty t in
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
