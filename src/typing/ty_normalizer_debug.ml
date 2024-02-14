(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[@@@warning "-60"]

(* Don't use Flow_js in here. *)
module Flow_js = struct end

[@@@warning "+60"]

module Normalizer = Ty_normalizer.Make (struct
  let eval _ ~should_eval:_ ~cont:_ ~default:_ ~non_eval (t, Type.TypeDestructorT (_, _, d), _) =
    non_eval t d

  let keys _ ~should_evaluate:_ ~cont:_ ~default _ _ = default ()

  let typeapp _ ~cont:_ ~type_ ~app ~from_value:_ _ t targs =
    let c = type_ t in
    let targs = Base.List.map ~f:type_ targs in
    app c targs

  let builtin cx ~cont reason x =
    let t =
      match Flow_js_utils.lookup_builtin_value_result cx x reason with
      | Ok t -> t
      | Error (t, _) -> t
    in
    cont t

  let builtin_type cx ~cont reason x =
    (* TODO the pattern matching on the result of lookup_builtin_name_result might need
     * some refinement. This is replacing what mk_instance would do. *)
    let t =
      match Flow_js_utils.lookup_builtin_type_result cx x reason with
      | Ok (Type.DefT (_, Type.TypeT (_, t))) -> t
      | Ok t -> t
      | Error (t, _) -> t
    in
    cont t

  let builtin_typeapp cx ~cont ~type_ ~app reason name targs =
    let c = builtin cx ~cont reason name in
    let targs = Base.List.map ~f:type_ targs in
    app c targs
end)

open Normalizer

let from_type ~options ~genv t =
  let imported_names = run_imports ~options ~genv in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams_rev:[] State.empty t in
  result

let string_of_t cx t =
  let typed_ast =
    ( ALoc.none,
      { Flow_ast.Program.statements = []; interpreter = None; comments = None; all_comments = [] }
    )
  in
  let file_sig = File_sig.empty in
  let genv = Ty_normalizer_env.mk_genv ~cx ~file:(Context.file cx) ~file_sig ~typed_ast in
  match from_type ~options:Ty_normalizer_env.default_options ~genv t with
  | Error (e, _) -> Utils_js.spf "<Error %s>" (Ty_normalizer.error_kind_to_string e)
  | Ok elt -> Ty_printer.string_of_elt_single_line ~exact_by_default:true elt
