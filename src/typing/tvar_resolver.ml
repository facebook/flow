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
        let (root_id, root) = Context.find_root cx id in
        if ISet.mem root_id seen then
          seen
        else
          let seen = ISet.add root_id seen in
          match root.C.constraints with
          | C.FullyResolved _ -> seen
          | C.Resolved (_, t) -> this#type_ cx pole seen t
          | C.Unresolved bounds ->
            TypeMap.fold (fun t _ seen -> this#type_ cx pole seen t) bounds.C.lower seen
    end
  in
  fun cx t ->
    match visitor#type_ cx Polarity.Positive ISet.empty t with
    | exception EncounteredPlaceholderType -> true
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
      let (root_id, root) = Context.find_root cx id in
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
            let new_root =
              if Context.in_synthesis_mode cx then
                C.Root { root with C.constraints = C.Resolved (unknown_use, t) }
              else
                C.Root { root with C.constraints = C.FullyResolved (unknown_use, lazy t) }
            in
            Context.add_tvar cx root_id new_root;
            let seen = ISet.add root_id seen in
            this#type_ cx pole seen t
        )

    method call_arg cx seen t =
      match t with
      | Arg t
      | SpreadArg t ->
        let _ = this#type_ cx Polarity.Positive seen t in
        seen

    method fun_call_type cx strategy t =
      let {
        call_this_t;
        call_targs;
        call_args_tlist;
        call_tout = (r, id);
        call_strict_arity = _;
        call_speculation_hint_state = _;
      } =
        t
      in
      let _ = this#type_ cx Polarity.Positive strategy call_this_t in
      let _ = Option.map (List.map (this#targ cx Polarity.Positive strategy)) call_targs in
      let _ = List.map (this#call_arg cx strategy) call_args_tlist in
      let _ = this#tvar cx Polarity.Positive strategy r id in
      strategy
  end

let run_conditionally cx f =
  match (Context.lti cx, Context.current_phase cx) with
  | (_, Context.InitLib) -> ()
  | (true, _)
  | (_, Context.PostInference) ->
    ignore @@ f ()
  | _ -> ()

let resolve ?(no_lowers = default_no_lowers) cx t =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      resolver#type_ cx Polarity.Positive ISet.empty t
  )

let resolved_t ?(no_lowers = default_no_lowers) cx t =
  resolve ~no_lowers cx t;
  t

let resolved_fun_call_type ?(no_lowers = default_no_lowers) cx funcalltype =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      resolver#fun_call_type cx ISet.empty funcalltype
  );
  funcalltype

let resolved_call_arg ?(no_lowers = default_no_lowers) cx call_arg =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      resolver#call_arg cx ISet.empty call_arg
  );
  call_arg

let resolved_type_args ?(no_lowers = default_no_lowers) cx targs =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      Option.map (List.map (resolver#targ cx Polarity.Positive ISet.empty)) targs
  );
  targs

let resolved_typeparam ?(no_lowers = default_no_lowers) cx typeparam =
  run_conditionally cx (fun () ->
      let resolver = new resolver ~no_lowers in
      resolver#type_param cx Polarity.Positive ISet.empty typeparam
  );
  typeparam
