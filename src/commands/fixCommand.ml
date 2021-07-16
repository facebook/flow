(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* transformable errors are a subset of all errors; specifically,
 * the errors for which Code_action_service.ast_transform_of_error = Some _ *)
type transformable_error = Loc.t Error_message.t'

(* Codemod-specific shared mem heap *)
(* stores a mapping from a file to all the errors that have transformations in that file *)
module TransformableErrorsHeap =
  SharedMem.NoCache
    (File_key)
    (struct
      type t = transformable_error list

      let description = "TransformableErrorsHeap"
    end)

let union_transformable_errors_maps :
    transformable_error list FilenameMap.t ->
    transformable_error list FilenameMap.t ->
    transformable_error list FilenameMap.t =
  FilenameMap.union ~combine:(fun _ tes1 tes2 -> Some (tes1 @ tes2))

let reader = State_reader.create ()

module type FIX_CODEMOD_OPTIONS = sig
  val error_codes : string list option
end

module FixCodemod (Opts : FIX_CODEMOD_OPTIONS) = struct
  type accumulator = unit

  type prepass_state = unit

  type prepass_result = transformable_error list FilenameMap.t

  let prepass_init () = ()

  let prepass_run cx () _file _reader _file_sig _typed_ast =
    let should_include_error =
      match Opts.error_codes with
      | None
      | Some [] ->
        Base.Fn.const true
      | Some codes ->
        fun error_message ->
          (match Error_message.error_code_of_message error_message with
          | None -> false
          | Some error_code ->
            let error_code_string = Error_codes.string_of_code error_code in
            Base.List.exists codes ~f:(fun code -> code = error_code_string))
    in
    Flow_error.ErrorSet.fold
      (fun error acc ->
        let error_message =
          error
          |> Flow_error.msg_of_error
          |> Error_message.map_loc_of_error_message (Parsing_heaps.Reader.loc_of_aloc ~reader)
        in
        match Code_action_service.ast_transform_of_error error_message with
        | Some Code_action_service.{ target_loc; _ } when should_include_error error_message ->
          let file_key = Base.Option.value_exn (Loc.source target_loc) in
          let transformable_error_map = FilenameMap.singleton file_key [error_message] in
          union_transformable_errors_maps transformable_error_map acc
        | _ -> acc)
      (Context.errors cx)
      FilenameMap.empty

  let store_precheck_result transform_maps =
    transform_maps
    |> FilenameMap.values
    |> Base.List.filter_map ~f:Base.Result.ok
    |> Base.List.fold ~init:FilenameMap.empty ~f:union_transformable_errors_maps
    |> FilenameMap.iter TransformableErrorsHeap.add

  let reporter =
    let combine _ _ = () in
    let report = Codemod_report.UnitReporter combine in
    let empty = () in
    Codemod_report.{ report; combine; empty }

  let visit =
    Codemod_utils.make_visitor
      (Codemod_utils.Mapper
         (fun Codemod_context.Typed.{ file; _ } ->
           object
             inherit [unit] Codemod_ast_mapper.mapper "Apply code action transformations" ~init:()

             method! program ast =
               match TransformableErrorsHeap.get file with
               | None -> ast
               | Some transformable_errors ->
                 Base.List.fold transformable_errors ~init:ast ~f:(fun acc_ast error_message ->
                     let Code_action_service.{ transform; target_loc; _ } =
                       Base.Option.value_exn
                         (Code_action_service.ast_transform_of_error error_message)
                     in
                     transform acc_ast target_loc)
           end))
end

let spec =
  CommandSpec.
    {
      name = "fix";
      doc = "Apply all known error fixes";
      usage = Printf.sprintf "%s fix [OPTION]... [FILE]\n" CommandUtils.exe_name;
      args =
        (ArgSpec.empty
        |> CommandUtils.codemod_flags
        |> ArgSpec.(
             flag
               "--error-codes"
               (list_of string)
               ~doc:"Codes of errors to fix. If omitted, all fixable errors will be fixed."));
    }

let main (CommandUtils.Codemod_params ({ anon; _ } as codemod_params)) error_codes () =
  let komodo_flags =
    let anon =
      match anon with
      | None
      | Some [] ->
        Some ["."]
      | _ -> anon
    in
    CommandUtils.Codemod_params { codemod_params with anon }
  in
  let module Runner = Codemod_runner.MakeTypedRunnerWithPrepass (FixCodemod (struct
    let error_codes = error_codes
  end)) in
  CodemodCommand.main (module Runner) komodo_flags ()

let command = CommandSpec.command spec main
