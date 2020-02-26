(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module File_sig = File_sig.With_ALoc

type member_expansion_options = {
  (* Whether to include prototypes' members *)
  include_proto_members: bool;
  (* A function which the implementation will call if the given type is in an IdxWrapper.
   * Feels hacky, but idx isn't super important now that we have optional chaining. *)
  idx_hook: unit -> unit;
}

type options = {
  (* If this flag is set to `true` then the normalizer will attempt to reuse the
   * cached results of evaluated type-destructors. If this is set to `false`, then
   * instread it will try to use:
   *  - a potentially attendant type-alias annotation, or
   *  - reuse the utility type that corresponds to this the specific type-destructor.
   *
   * Choosing 'false' will typically result in smaller produced types, which makes
   * it a more appropriate option for codemods.
   *)
  evaluate_type_destructors: bool;
  (* Expand the signatures of built-in functions, such as:
   * Function.prototype.apply: (thisArg: any, argArray?: any): any
   *)
  expand_internal_types: bool;
  (* If set to `true` type aliase names will be expanded to the types they represent.
   *
   * WARNING: This can cause a blow-up in the size of the produced types.
   *)
  expand_type_aliases: bool;
  (* MergedT is somewhat unconventional. It introduces UseT's that the
   * normalizer is not intended to handle. If this flag is set to true, all
   * instances of MergedT will fall through and return Top. Otherwise, we
   * attempt to convert the use_t's under the MergedT. This operation only
   * succeeds if the use is a UseT and the underlying type is successfully
   * normalized.
   *
   * Pick `true` if the result does not need to be "parseable", e.g. coverage.
   *)
  fall_through_merged: bool;
  (* The normalizer keeps a stack of type parameters that are in scope. This stack
   * may contain the same name twice (but with different associated locations).
   * This is a case of shadowing. For certain uses of normalized types (e.g. suggest)
   * we do not wish to allow the generation of type parameters that are shadowed by
   * another definition. For example the inferred type for `z` in:*

   * function outer<T>(y: T) {
   *     function inner<T>(x: T, z) { inner(x, y); }
   * }
   *
   * is the _outer_ T. Adding the annotation ": T" for `z` would not be correct.
   * This flags toggles this behavior.
   *)
  flag_shadowed_type_params: bool;
  (* Consider all kinds of Bot and Any the same when simplifying types.
   *
   * The normalized type Ty.Bot may correspond to either the `Empty` type, not
   * lower-bounds or the internal types MatchingPropT or TypeDestructorTriggerT.
   * These types are not easy to normalize, but may still encode some constraint.
   * When using normalized types for codemods we might want to know if there might
   * be some constraints that we missing in the normalized type.
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
  omit_targ_defaults: bool;
  (* Run an optimization pass that removes duplicates from unions and intersections.
   *
   * WARNING May be slow for large types
   *)
  optimize_types: bool;
  (* Makes the normalizer more aggressive in preserving inferred literal types *)
  preserve_inferred_literal_types: bool;
  (* Debug *)
  verbose_normalizer: bool;
  (* If set to `Some _` then render top-level InstanceTs, primitive types,
   * class types, type variables/aliases as object types instead of nominally.
   *
   * Used for member autocompletion.
   *)
  expand_toplevel_members: member_expansion_options option;
  (* Maximum depth of recursion *)
  max_depth: int option;
}

let default_options =
  {
    evaluate_type_destructors = false;
    expand_internal_types = false;
    expand_type_aliases = false;
    fall_through_merged = false;
    flag_shadowed_type_params = false;
    merge_bot_and_any_kinds = true;
    omit_targ_defaults = false;
    optimize_types = true;
    preserve_inferred_literal_types = false;
    verbose_normalizer = false;
    expand_toplevel_members = None;
    max_depth = Some 50;
  }

(* This is a global environment that should not change during normalization *)
type genv = {
  (* File the query originated from *)
  file: File_key.t;
  (* Full (merged) context *)
  cx: Context.t;
  (* Typed AST of the current file *)
  typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.program;
  (* The file_sig of the current file *)
  file_sig: File_sig.t;
}

let mk_genv ~full_cx ~file ~typed_ast ~file_sig = { file; cx = full_cx; typed_ast; file_sig }

type instance_member_expansion_mode =
  | IMUnset
  | IMStatic
  | IMInstance

(* Info used for implementation of `expand_toplevel_members` *)
type member_expansion_info = {
  (* Information that was passed in via options *)
  member_expansion_options: member_expansion_options;
  (* Sets how to expand members upon encountering an InstanceT:
   * - if set to IMStatic then expand the static members
   * - if set to IMUnset or IMInstance then expand the instance members.
   *
   * We distinguish between this being not yet set (IMUnset) and this being explicitly
   * set to instance (IMInstance) for the sake of determining how to update this flag
   * upon a ThisClassT:
   * - if the flag was IMUnset, then we know we want to proceed by setting it to IMStatic
   *   so that the InstanceT within is looked at as a static class.
   * - if the flag was IMInstance then we could be looking at the superclass of another
   *   InstanceT, in which case we want to look at the superclass as an instance. *)
  instance_member_expansion_mode: instance_member_expansion_mode;
  (* Keep track of whether we're currently expanding the proto members *)
  within_proto: bool;
}

type t = {
  (* Does not change. Set once in the beginning. *)
  genv: genv;
  (* Normalization parameters *)
  options: options;
  (* Type parameters in scope

     The parameter environment is useful in handling inferred type parameters.
     During checking, type parameters are expanded to so called generated tests,
     that hold the `RPolyTest` reason. During normalization we wish to recover the
     more informative original parameter names, instead of these tests. To do so
     it's important to know what type parameters were in scope at the location where
     a type was recorded. The type parameter name is saved with that `RPolyTest`
     reason. What is not guaranteed, however, is that the parameter is in scope at
     the point of the query. For example, consider:

       function f<T>(x: T) { return x; }
       const y = f(1);

     The type for `x` within `f` is reconstructed from the generated bounds (for
     `T`): Empty and Mixed. From the `RPolyTest` reasons on these types we can
     recover the name `T` and return that instead of the bounds.

     Due to the lack of a return type for `f`, however, querying `y` returns the
     exact same answer even outside the context of `f`. The reasons will still point
     to `T` even though it's now out of scope. In this case we need to fall back to
     the actual bounds and return those instead. So the normalized type here would
     be: Empty | Mixed, which simplifies to Mixed. *)
  tparams: (ALoc.t * string) list;
  (* In determining whether a symbol is Local, Imported, Remote, etc, it is
     useful to keep a map of imported names and the corresponding
     location available. We can then make this decision by comparing the
     source file with the current context's file information. *)
  imported_names: Ty.imported_ident Loc_collections.ALocMap.t;
  (* For debugging purposes mostly *)
  depth: int;
  (* The default behavior with type aliases is to return the name of the alias
     instead of the expansion of the type. When normalizing type aliases `TypeT t`,
     however, we proceed by recovering the name of the alias (say A) and then
     normalizing the body `T` to recover the type "type A = T". So for this case
     only it is useful to allow a one-off expansion of the contents of TypeT.
     We do this by setting this property to `Some A`.

     The reason we need the alias name (instead of just the fact that we're expanding
     an alias) is that the RTypeAlias reason (used to discover aliases) may be
     repeated across nested types (ExactT and MaybeT -- see statement.ml) and so we
     need to skip over duplicates until we either encounter another type constructor
     or a different type alias.

     NOTE: The use of the name might not be robust against aliases with the same name
     (coming from different scopes for example). Ideally we would use the location
     or a unique ID of the type alias to make this distinction, but at the moment
     keeping this information around introduces a small space regression. *)
  under_type_alias: string option;
  (* Info used in the implementation of `expand_toplevel_members`.
   * This is stored separately from the `expand_toplevel_members` option so that
   * we can keep track of more information than what we ask the caller to specify.
   * The bool tracks whether we should expand members. It's set to true initially,
   * and then gets flipped to false once we descend below the top level of the type. *)
  member_expansion_info: (bool * member_expansion_info) option;
}

let init ~options ~genv ~tparams ~imported_names =
  {
    options;
    genv;
    depth = 0;
    tparams;
    imported_names;
    under_type_alias = None;
    member_expansion_info =
      Base.Option.map options.expand_toplevel_members ~f:(fun member_expansion_options ->
          ( true,
            {
              member_expansion_options;
              instance_member_expansion_mode = IMUnset;
              within_proto = false;
            } ));
  }

let descend e = { e with depth = e.depth + 1 }

let get_cx e = e.genv.cx

let fall_through_merged e = e.options.fall_through_merged

let expand_internal_types e = e.options.expand_internal_types

let expand_type_aliases e = e.options.expand_type_aliases

let evaluate_type_destructors e = e.options.evaluate_type_destructors

let flag_shadowed_type_params e = e.options.flag_shadowed_type_params

let preserve_inferred_literal_types e = e.options.preserve_inferred_literal_types

let omit_targ_defaults e = e.options.omit_targ_defaults

let max_depth e = e.options.max_depth

let merge_bot_and_any_kinds e = e.options.merge_bot_and_any_kinds

let current_file e = e.genv.file

let add_typeparam env typeparam = { env with tparams = typeparam :: env.tparams }

let get_member_expansion_info e =
  match e.member_expansion_info with
  | Some (true, info) -> Some (info, { e with member_expansion_info = Some (false, info) })
  | Some (false, _)
  | None ->
    None

let continue_expanding_members e =
  {
    e with
    member_expansion_info =
      Base.Option.map e.member_expansion_info ~f:(fun (_, info) -> (true, info));
  }

let expand_instance_members e =
  {
    e with
    member_expansion_info =
      Base.Option.map e.member_expansion_info ~f:(fun (_, info) ->
          (true, { info with instance_member_expansion_mode = IMInstance }));
  }

let expand_static_members e =
  {
    e with
    member_expansion_info =
      Base.Option.map e.member_expansion_info ~f:(fun (_, info) ->
          (true, { info with instance_member_expansion_mode = IMStatic }));
  }

let expand_proto_members e =
  {
    e with
    member_expansion_info =
      Base.Option.map e.member_expansion_info ~f:(fun (_, info) ->
          (true, { info with within_proto = true; instance_member_expansion_mode = IMUnset }));
  }

(* We let ty_normalizer access this separately from get_member_expansion_info
 * because this is relevant after we've descended below the top level of the type *)
let within_proto e =
  match e.member_expansion_info with
  | None -> false
  | Some (_, { within_proto; _ }) -> within_proto
