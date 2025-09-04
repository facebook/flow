(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Normalizer = Ty_normalizer.Make (struct
  let eval _cx ~should_eval:_ ~cont:_ ~default:_ ~non_eval (t, d, _id) =
    let (Type.TypeDestructorT (_, _, d)) = d in
    non_eval t d

  let keys _cx ~should_evaluate:_ ~cont:_ ~default _r _t = default ()

  let typeapp _ ~cont:_ ~type_ ~app ~from_value:_ _ t targs =
    let c = type_ t in
    let targs = Base.List.map ~f:type_ targs in
    app c targs

  let builtin_type cx ~cont reason x =
    (* TODO the pattern matching on the result of lookup_builtin_strict_result might need
     * some refinement. This is replacing what mk_instance would do. *)
    let t =
      match Flow_js_utils.lookup_builtin_type cx x reason with
      | Type.DefT (_, Type.TypeT (_, t)) -> t
      | t -> t
    in
    cont t

  let builtin_typeapp cx ~cont ~type_:_ ~app:_ reason name targs =
    let t = Flow_js_utils.lookup_builtin_type cx name reason in
    let t = TypeUtil.typeapp ~from_value:false ~use_desc:false reason t targs in
    cont t
end)

open Normalizer

let from_type genv t =
  let (result, _) = run_type ~genv State.empty t in
  result

let mk_genv ~options ~cx ~typed_ast_opt ~file_sig =
  (* Computing proper import information would introduce cycles making this module
   * unusable in any code depending on Flow_js. *)
  let dummy_import_list = [] in
  let imported_names =
    lazy (normalize_imports cx file_sig typed_ast_opt options dummy_import_list)
  in
  { Ty_normalizer_env.options; cx; typed_ast_opt; file_sig; imported_names }

let mk_default_genv ?(options = Ty_normalizer_env.default_options) cx =
  let typed_ast =
    ( ALoc.none,
      { Flow_ast.Program.statements = []; interpreter = None; comments = None; all_comments = [] }
    )
  in
  let file_sig = File_sig.empty in
  mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast)

let debug_string_of_t cx t =
  let genv = mk_default_genv cx in
  match from_type genv t with
  | Error (e, _) -> Utils_js.spf "<Error %s>" (Ty_normalizer.error_kind_to_string e)
  | Ok elt ->
    Ty_printer.string_of_elt_single_line
      ~exact_by_default:true
      ~ts_syntax:(Context.ts_syntax cx)
      elt

let type_to_desc_for_invariant_subtyping_error cx =
  let genv =
    mk_default_genv ~options:Ty_normalizer_env.{ default_options with optimize_types = false } cx
  in
  fun t ->
    let desc = TypeUtil.desc_of_t t in
    let open Reason in
    match desc with
    | RArrayLit
    | RArrayLit_UNSOUND
    | RObjectLit
    | RObjectLit_UNSOUND ->
      (* For these descriptions, the normalized types can be huge,
       * but the reason description can be quite simple.
       * It's also guaranteed to be in one place,
       * instead of spreading across multiple places due to implicit instantiation. *)
      Error desc
    | _ ->
      (* Why do we try to use the printed types? For these non-leaf-node types, it's
       * reference locations can be spread across multiple locations, but the reason infra
       * can only point code reference to one place, instead of multiple places that
       * contribute to the composition of a type.
       *
       * e.g.
       * ```
       * declare function f<T>(x: T): Array<T>;
       * //                           ^^^^^^^^
       * const x = f(1);
       * //          ^
       * ```
       *
       * To fully describe the type of `x`, we need to point to multiple locations, which
       * is not well supported by the reason infra. So we use the printed types instead.
       * The location might still be non-ideal, but at least the description makes sense.
       *)
      (match from_type genv t with
      | Error _ -> Error desc
      | Ok elt ->
        (match Ty_utils.typify_elt elt with
        | None -> Error desc
        | Some t -> Ok t))
