(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Typed_ast_finder

type 'a result =
  | FailureNoMatch
  | FailureUnparseable of Loc.t * Type.t * string
  | Success of Loc.t * 'a

let concretize_loc_pairs pair_list =
  Base.List.map ~f:(fun (loc, x) -> (ALoc.to_loc_exn loc, x)) pair_list

let sort_loc_pairs pair_list = List.sort (fun (a, _) (b, _) -> Loc.compare a b) pair_list

let result_of_normalizer_error loc t err =
  let msg = Ty_normalizer.error_to_string err in
  FailureUnparseable (loc, t, msg)

let max_size_of_evaluated_type = 100

let dump_type_at_pos ~cx ~typed_ast loc =
  match find_type_at_pos_annotation cx typed_ast loc with
  | Type_at_pos.NoResult -> None
  | Type_at_pos.HardcodedModuleResult (loc, _) -> Some (loc, "ModuleT")
  | Type_at_pos.TypeResult (loc, _, t) -> Some (loc, Debug_js.dump_t cx ~depth:10 t)

let type_at_pos_type
    ~cx
    ~file_sig
    ~omit_targ_defaults
    ~verbose_normalizer
    ~max_depth
    ~typed_ast
    ~no_typed_ast_for_imports
    ~include_refs
    loc : Ty.type_at_pos_result result =
  match find_type_at_pos_annotation cx typed_ast loc with
  | Type_at_pos.NoResult -> FailureNoMatch
  | Type_at_pos.HardcodedModuleResult (loc, name) ->
    let module_symbol =
      Ty_symbol.
        {
          sym_provenance = Local;
          sym_name = Reason.OrdinaryName name;
          sym_anonymous = false;
          sym_def_loc = ALoc.of_loc loc;
        }
    in
    let unevaluated =
      Ty.(Decl (ModuleDecl { name = Some module_symbol; exports = []; default = None }))
    in
    Success (loc, { Ty.unevaluated; evaluated = None; refs = None })
  | Type_at_pos.TypeResult (loc, toplevel_is_type_identifier_reference, t) ->
    let typed_ast_opt =
      if no_typed_ast_for_imports then
        None
      else
        Some typed_ast
    in
    let options evaluate_type_destructors =
      {
        Ty_normalizer_env.expand_internal_types = false;
        preserve_inferred_literal_types = false;
        evaluate_type_destructors;
        optimize_types = true;
        omit_targ_defaults_option = omit_targ_defaults;
        merge_bot_and_any_kinds = true;
        verbose_normalizer;
        max_depth = Some max_depth;
        toplevel_is_type_identifier_reference;
      }
    in

    let from_type evaluate_type_destructors =
      let options = options evaluate_type_destructors in
      let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~file_sig ~typed_ast_opt in
      Ty_normalizer_flow.from_type_with_found_computed_type genv t
    in
    let (unevaluated, found_computed_type) = from_type Ty_normalizer_env.EvaluateNone in
    let evaluated =
      if found_computed_type then
        (* We need to roll back caches and errors, because server state persists
           through IDE requests. If evaluation results in new errors, future
           requests at the same location should also result in "new" errors. *)
        Context.run_and_rolled_back_cache cx (fun () ->
            let errors = Context.errors cx in
            let (evaluated, _) = from_type Ty_normalizer_env.EvaluateAll in
            let errors' = Context.errors cx in
            Context.reset_errors cx errors;
            if Flow_error.ErrorSet.equal errors errors' then
              Some evaluated
            else
              None
        )
      else
        None
    in
    let refs (unevaluated, evaluated) =
      let open Ty in
      match include_refs with
      | None -> None
      | Some loc_of_aloc ->
        let syms = Ty.symbols_of_elt ~loc_of_aloc unevaluated in
        Some
          (Base.Option.value_map evaluated ~default:syms ~f:(fun e ->
               LocSymbolSet.union syms (symbols_of_elt ~loc_of_aloc e)
           )
          )
    in
    let tys =
      begin
        match (unevaluated, evaluated) with
        | (Ok unevaluated, Some (Ok evaluated)) ->
          (match Ty_utils.size_of_elt ~max:max_size_of_evaluated_type evaluated with
          | Some _ -> Ok (unevaluated, Some evaluated)
          | None -> Ok (unevaluated, None))
        | (Ok unevaluated, _) -> Ok (unevaluated, None)
        | (Error err, _) -> Error err
      end
    in
    (match tys with
    | Ok (unevaluated, evaluated) ->
      let refs = refs (unevaluated, evaluated) in
      Success (loc, { Ty.unevaluated; evaluated; refs })
    | Error err -> result_of_normalizer_error loc t err)

let dump_types ~printer ~evaluate_type_destructors cx file_sig typed_ast =
  let options =
    { Ty_normalizer_env.default_options with Ty_normalizer_env.evaluate_type_destructors }
  in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~typed_ast_opt:(Some typed_ast) ~file_sig in
  let result = Ty_normalizer_flow.from_types genv (Typed_ast_utils.typed_ast_to_list typed_ast) in
  let print_ok = function
    | (l, Ok t) -> Some (l, printer t)
    | _ -> None
  in
  Base.List.filter_map result ~f:print_ok |> concretize_loc_pairs |> sort_loc_pairs

let dump_types_for_tool cx typed_ast =
  let types = Typed_ast_utils.typed_ast_to_list typed_ast in
  let print_type_json (loc, t) =
    ( loc,
      Flow_js.singleton_concrete_type_for_inspection cx (TypeUtil.reason_of_t t) t
      |> ConvertTypes.type_to_json cx 10
      |> Hh_json.json_to_string
    )
  in
  Base.List.map types ~f:print_type_json |> concretize_loc_pairs |> sort_loc_pairs

let insert_type_normalize ~cx ~file_sig ~omit_targ_defaults ~typed_ast loc t : Ty.elt result =
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      (* We eventually want to elimitate literal types, so let's not expose them here. *)
      preserve_inferred_literal_types = false;
      (* Utility types won't are not serialized so it may be worth evaluating them away
       * if we find them in the resulting Ty.t. The trade off is that types might get
       * larger. *)
      evaluate_type_destructors = Ty_normalizer_env.EvaluateNone;
      (* Optimize types is false because Insert_types manually calls the simplifier with
         a custom comparison operation *)
      optimize_types = false;
      omit_targ_defaults_option = omit_targ_defaults;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = None;
      toplevel_is_type_identifier_reference = false;
    }
  in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast) in
  match Ty_normalizer_flow.from_type genv t with
  | Ok elt -> Success (loc, elt)
  | Error err -> result_of_normalizer_error loc t err
