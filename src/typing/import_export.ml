(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AST handling and type setup for import/export *)

module Flow = Flow_js
open Type

let check_platform_availability cx reason imported_module_available_platforms =
  let current_module_available_platforms = Context.available_platforms cx in
  match (current_module_available_platforms, imported_module_available_platforms) with
  | (None, None)
  | (None, Some _)
  | (Some _, None) ->
    ()
  | (Some required_platforms, Some available_platforms) ->
    let file_options = Context.((metadata cx).file_options) in
    let required_platforms = Platform_set.to_platform_string_set ~file_options required_platforms in
    let available_platforms =
      Platform_set.to_platform_string_set ~file_options available_platforms
    in
    let missing_platforms = SSet.diff required_platforms available_platforms in
    if SSet.cardinal missing_platforms > 0 then
      let loc = Reason.loc_of_reason reason in
      let message =
        Error_message.EMissingPlatformSupport { loc; available_platforms; required_platforms }
      in
      Flow_js_utils.add_output cx message

let get_module_t cx ?(perform_platform_validation = false) (loc, mref) =
  if Context.in_declare_module cx then
    Flow.get_builtin_module cx loc mref
  else
    let reason = Reason.(mk_reason (RCustom mref) loc) in
    let module_t =
      match Context.find_require cx mref with
      | Ok t -> t
      | Error m_name ->
        Flow_js_utils.lookup_builtin_strict_error cx m_name reason
        |> Flow_js_utils.apply_env_errors cx loc
    in
    ( if perform_platform_validation && Context.((metadata cx).file_options.Files.multi_platform)
    then
      match Flow_js.possible_concrete_types_for_inspection cx reason module_t with
      | [ModuleT m] -> check_platform_availability cx reason m.module_available_platforms
      | _ -> ()
    );
    module_t
