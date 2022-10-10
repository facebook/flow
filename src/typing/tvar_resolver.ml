(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Utils_js

type unconstrained_tvar_resolution_strategy =
  | Allow
  | Error
  | Exception

exception UnconstrainedTvarException of int

let resolver =
  object (this)
    inherit [unconstrained_tvar_resolution_strategy] Type_visitor.t

    method! tvar cx pole strategy r id =
      let module C = Type.Constraint in
      let (root_id, root) = Context.find_root cx id in
      match root.C.constraints with
      | C.FullyResolved _ -> strategy
      | _ ->
        let t =
          match Flow_js_utils.merge_tvar_opt cx r root_id with
          | Some t -> Some t
          | None ->
            if Context.in_synthesis_mode cx then
              None
            else begin
              (match strategy with
              | Allow -> ()
              | Error ->
                let id_msg =
                  match Context.verbose cx with
                  | Some verbose when Debug_js.Verbose.verbose_in_file cx verbose -> Some root_id
                  | _ -> None
                in
                Flow_js_utils.add_output
                  cx
                  Error_message.(EInternal (aloc_of_reason r, UnconstrainedTvar id_msg))
              | Exception -> raise (UnconstrainedTvarException root_id));
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
            let (_ : unconstrained_tvar_resolution_strategy) = this#type_ cx pole strategy t in
            ()
        );
        strategy

    method call_arg cx strategy t =
      match t with
      | Arg t
      | SpreadArg t ->
        let _ = this#type_ cx Polarity.Positive strategy t in
        strategy

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
  match (Context.env_mode cx, Context.current_phase cx) with
  | (_, Context.InitLib) -> ()
  | (Options.LTI, _)
  | (_, Context.PostInference) ->
    ignore @@ f ()
  | _ -> ()

let resolve cx ~on_unconstrained_tvar t =
  run_conditionally cx (fun () -> resolver#type_ cx Polarity.Positive on_unconstrained_tvar t)

let resolved_t cx ~on_unconstrained_tvar t =
  resolve cx ~on_unconstrained_tvar t;
  t

let resolved_fun_call_type cx ~on_unconstrained_tvar funcalltype =
  run_conditionally cx (fun () -> resolver#fun_call_type cx on_unconstrained_tvar funcalltype);
  funcalltype

let resolved_call_arg cx ~on_unconstrained_tvar call_arg =
  run_conditionally cx (fun () -> resolver#call_arg cx on_unconstrained_tvar call_arg);
  call_arg

let resolved_type_args cx ~on_unconstrained_tvar targs =
  run_conditionally cx (fun () ->
      Option.map (List.map (resolver#targ cx Polarity.Positive on_unconstrained_tvar)) targs
  );
  targs

let resolved_typeparam cx ~on_unconstrained_tvar typeparam =
  run_conditionally cx (fun () ->
      resolver#type_param cx Polarity.Positive on_unconstrained_tvar typeparam
  );
  typeparam
