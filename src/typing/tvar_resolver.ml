(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Utils_js

let resolver =
  object (this)
    inherit [bool] Type_visitor.t

    method! tvar cx pole require_resolution r id =
      let module C = Type.Constraint in
      let (root_id, root) = Context.find_root cx id in
      match root.C.constraints with
      | C.FullyResolved _ -> require_resolution
      | _ ->
        let t =
          match Flow_js_utils.merge_tvar_opt cx r root_id with
          | Some t -> Some t
          | None ->
            if Context.in_synthesis_mode cx then
              None
            else begin
              ( if require_resolution then
                let id_msg =
                  match Context.verbose cx with
                  | Some verbose when Debug_js.Verbose.verbose_in_file cx verbose -> Some root_id
                  | _ -> None
                in
                Flow_js_utils.add_output
                  cx
                  Error_message.(EInternal (aloc_of_reason r, UnconstrainedTvar id_msg))
              );
              let desc =
                match desc_of_reason r with
                | RIdentifier (OrdinaryName x) -> RCustom (spf "`%s` (resolved to type `empty`)" x)
                | _ -> REmpty
              in
              Some (EmptyT.make (replace_desc_reason desc r) (bogus_trust ()))
            end
        in
        Base.Option.iter t ~f:(fun t ->
            let root = C.Root { root with C.constraints = C.FullyResolved (unknown_use, lazy t) } in
            Context.add_tvar cx root_id root;
            let (_ : bool) = this#type_ cx pole require_resolution t in
            ()
        );
        require_resolution

    method call_arg cx require_resolution t =
      match t with
      | Arg t
      | SpreadArg t ->
        let _ = this#type_ cx Polarity.Positive require_resolution t in
        require_resolution

    method fun_call_type cx require_resolution t =
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
      let _ = this#type_ cx Polarity.Positive require_resolution call_this_t in
      let _ =
        Option.map (List.map (this#targ cx Polarity.Positive require_resolution)) call_targs
      in
      let _ = List.map (this#call_arg cx require_resolution) call_args_tlist in
      let _ = this#tvar cx Polarity.Positive require_resolution r id in
      require_resolution
  end

let run_conditionally cx f =
  match (Context.env_mode cx, Context.current_phase cx) with
  | (_, Context.InitLib) -> ()
  | (Options.LTI, _)
  | (_, Context.PostInference) ->
    let (_ : bool) = f () in
    ()
  | _ -> ()

let resolve cx ~require_resolution t =
  run_conditionally cx (fun () -> resolver#type_ cx Polarity.Positive require_resolution t)

let resolved_t cx ~require_resolution t =
  resolve cx ~require_resolution t;
  t

let resolved_fun_call_type cx ~require_resolution funcalltype =
  run_conditionally cx (fun () -> resolver#fun_call_type cx require_resolution funcalltype);
  funcalltype

let resolved_call_arg cx ~require_resolution call_arg =
  run_conditionally cx (fun () -> resolver#call_arg cx require_resolution call_arg);
  call_arg

let resolved_typeparam cx ~require_resolution typeparam =
  run_conditionally cx (fun () ->
      resolver#type_param cx Polarity.Positive require_resolution typeparam
  );
  typeparam
