(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module File_sig = File_sig

type evaluate_type_destructors_mode =
  | EvaluateNone
  | EvaluateAll
  | EvaluateSome

type options = {
  (* If this flag is set to `true` then the normalizer will attempt to reuse the
   * cached results of evaluated type-destructors. If this is set to `false`, then
   * instead it will try to use:
   *  - a potentially attendant type-alias annotation, or
   *  - reuse the utility type that corresponds to this the specific type-destructor.
   *
   * Choosing 'false' will typically result in smaller produced types, which makes
   * it a more appropriate option for codemods.
   *)
  evaluate_type_destructors: evaluate_type_destructors_mode;
  (* Expand the signatures of built-in functions, such as:
   * Function.prototype.apply: (thisArg: any, argArray?: any): any
   *)
  expand_internal_types: bool;
  (* Consider all kinds of Bot and Any the same when simplifying types.
   *
   * The normalized type Ty.Bot may correspond to either the `Empty` type,
   * or not have any lower-bounds. These types are not easy to
   * normalize, but may still encode some constraint. When using normalized types
   * for codemods we might want to know if there might be some constraints that we
   * missing in the normalized type.
   *
   * Any can be due to an annotation or implicitly arising from inference.
   *)
  merge_bot_and_any_kinds: bool;
  (* Omits type params if they match the defaults, e.g:
   *
   * Given `type Foo<A, B = Baz>`, `Foo<Bar, Baz>` is reduced to `Foo<Bar>`
   *
   * WARNING: May be slow due to the structural equality checks that this necessitates.
   *)
  omit_targ_defaults_option: bool;
  (* Run an optimization pass that removes duplicates from unions and intersections.
   *
   * WARNING May be slow for large types
   *)
  optimize_types: bool;
  (* Makes the normalizer more aggressive in preserving inferred literal types *)
  preserve_inferred_literal_types: bool;
  (* Debug *)
  verbose_normalizer: bool;
  (* Maximum depth of recursion *)
  max_depth: int option;
  (* In typed AST for type references, we store the type as if it's read under the
   * value namespace. In some places, we might want to record the fact that it's
   * a type-namespace read. *)
  toplevel_is_type_identifier_reference: bool;
}

let default_options =
  {
    evaluate_type_destructors = EvaluateNone;
    expand_internal_types = false;
    merge_bot_and_any_kinds = true;
    omit_targ_defaults_option = false;
    optimize_types = true;
    preserve_inferred_literal_types = false;
    verbose_normalizer = false;
    max_depth = Some 40;
    toplevel_is_type_identifier_reference = false;
  }

let default_codemod_options =
  {
    expand_internal_types = false;
    preserve_inferred_literal_types = false;
    evaluate_type_destructors = EvaluateSome;
    optimize_types = false;
    omit_targ_defaults_option = true;
    merge_bot_and_any_kinds = false;
    verbose_normalizer = false;
    max_depth = None;
    toplevel_is_type_identifier_reference = false;
  }

(** Global environment that does not change during normalization *)
type genv = {
  (* Full (merged) context *)
  cx: Context.t;
  (* Typed AST of the current file *)
  typed_ast_opt: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option;
  (* The file_sig of the current file *)
  file_sig: File_sig.t;
  (* In determining whether a symbol is Local, Imported, Remote, etc, it is
     useful to keep a map of imported names and the corresponding
     location available. We can then make this decision by comparing the
     source file with the current context's file information. *)
  imported_names: Ty.imported_ident Loc_collections.ALocMap.t Lazy.t;
  (* Normalization parameters *)
  options: options;
}

module SymbolSet = Flow_set.Make (struct
  type t = Ty_symbol.symbol

  let compare = Stdlib.compare
end)

type t = {
  (* Does not change. Set once in the beginning. *)
  genv: genv;
  infer_tparams: Type.typeparam list;
  (* For debugging purposes mostly *)
  depth: int;
  keep_only_namespace_name: bool;
  (* Detect recursive types *)
  seen_tvar_ids: ISet.t;
  seen_eval_ids: Type.EvalIdSet.t;
  omit_targ_defaults: bool;
}

let init ~genv =
  {
    genv;
    depth = 0;
    infer_tparams = [];
    keep_only_namespace_name = false;
    seen_tvar_ids = ISet.empty;
    seen_eval_ids = Type.EvalIdSet.empty;
    omit_targ_defaults = genv.options.omit_targ_defaults_option;
  }

let descend e = { e with depth = e.depth + 1 }

let get_cx e = e.genv.cx

let imported_names e = Lazy.force e.genv.imported_names

let expand_internal_types e = e.genv.options.expand_internal_types

let evaluate_type_destructors e = e.genv.options.evaluate_type_destructors

let preserve_inferred_literal_types e = e.genv.options.preserve_inferred_literal_types

let omit_targ_defaults e = e.omit_targ_defaults

let optimize_types e = e.genv.options.optimize_types

let max_depth e = e.genv.options.max_depth

let merge_bot_and_any_kinds e = e.genv.options.merge_bot_and_any_kinds

let verbose e = e.genv.options.verbose_normalizer

let toplevel_is_type_identifier_reference e = e.genv.options.toplevel_is_type_identifier_reference
