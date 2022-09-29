(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)

let mapper
    ~preserve_literals ~generalize_maybe ~max_type_size ~default_any (cctx : Codemod_context.Typed.t)
    =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let _flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  let reader = cctx.Codemod_context.Typed.reader in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let errors = Codemod_context.Typed.context cctx |> Context.errors in

  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true
        ~generalize_react_mixed_element:true
        () as super

    val mutable loc_error_set = LSet.empty

    method! call loc expr =
      let expr = super#call loc expr in
      (* TODO: Add annotations *)
      if LSet.mem loc loc_error_set then ();
      expr

    method private init_loc_error_set =
      loc_error_set <-
        Flow_error.ErrorSet.fold
          (fun error acc ->
            match Flow_error.msg_of_error error with
            | Error_message.EImplicitInstantiationUnderconstrainedError _ ->
              (match Flow_error.loc_of_error error with
              | Some loc -> LSet.add (loc_of_aloc loc) acc
              | None -> acc)
            | _ -> acc)
          errors
          loc_error_set

    method private post_run () = ()

    method! program prog =
      this#init_loc_error_set;
      if LSet.is_empty loc_error_set then
        prog
      else
        super#program prog
  end
