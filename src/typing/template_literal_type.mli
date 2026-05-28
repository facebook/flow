(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Build a Flow type from a template literal's quasis and interpolated types.
   Eagerly collapses to [SingletonStrT] (or a [UnionT] of them) when every
   interpolation resolves to a concrete literal and the cross product fits
   within the complexity limit; otherwise returns a [TemplateLiteralT].

   When [possible_concrete_types_for_inspection] is provided, a deferred
   post-inference callback re-runs the syntactic placeholder-validity check
   after Flow_js concretizes types hidden behind [EvalT]/[OpenT]/[AnnotT].
   Callers from contexts that deliberately block Flow_js (e.g. type_sig_merge)
   should omit it. *)
val resolve :
  ?possible_concrete_types_for_inspection:(Context.t -> Reason.t -> Type.t -> Type.t list) ->
  quasis:string list ->
  types:Type.t list ->
  ALoc.t ->
  Context.t ->
  Type.t

(* Attempt to enumerate the strings a template literal could produce. Returns
   [None] when any interpolated type is non-literal or when the cross product
   exceeds the complexity limit. Pass [~expand_generics:true] at subtyping
   time to treat generics as their bound. *)
val try_resolve_to_strings :
  ?expand_generics:bool -> string list -> Type.t list -> string list option

(* SingletonStrT s <: TemplateLiteralT: decompose [s] by quasis and flow each
   extracted segment to the corresponding interpolated type. When
   [possible_concrete_types_for_inspection] is provided, indirected placeholder
   types (e.g. [T[0]] as an [EvalT]) are concretized and replaced with their
   stringified singleton/union form before matching, so number/bigint/etc.
   literals are compared in their template-coerced string form. *)
val subtype_str_lit_into_template :
  ?possible_concrete_types_for_inspection:(Context.t -> Reason.t -> Type.t -> Type.t list) ->
  rec_flow_t:(Context.t -> Type.DepthTrace.t -> use_op:Type.use_op -> Type.t * Type.t -> unit) ->
  Context.t ->
  Type.DepthTrace.t ->
  Type.use_op ->
  lower:Type.t ->
  upper:Type.t ->
  string ->
  string list ->
  Type.t list ->
  unit

(* TemplateLiteralT <: UnionT: enumerate the strings the LHS could produce
   (expanding generics) and flow each one into the RHS union independently. *)
val subtype_template_into_union :
  rec_flow_t:(Context.t -> Type.DepthTrace.t -> use_op:Type.use_op -> Type.t * Type.t -> unit) ->
  Context.t ->
  Type.DepthTrace.t ->
  Type.use_op ->
  lower:Type.t ->
  upper:Type.t ->
  string list ->
  Type.t list ->
  unit

(* Result of [try_subtype_template_to_template]. [Not_applicable] means no
   structural rule fired — the caller should fall through to the cross-product
   enumeration path ([subtype_template_to_other]). *)
type tl_to_tl_result =
  | Handled
  | Not_applicable

(* Combined dispatch for `TemplateLiteralT <: TemplateLiteralT`. Tries (in
   order) pairwise, prefix-extension, suffix-extension, and wide-string
   rules. *)
val try_subtype_template_to_template :
  rec_flow_t:(Context.t -> Type.DepthTrace.t -> use_op:Type.use_op -> Type.t * Type.t -> unit) ->
  Context.t ->
  Type.DepthTrace.t ->
  Type.use_op ->
  Reason.t ->
  upper:Type.t ->
  string list ->
  Type.t list ->
  string list ->
  Type.t list ->
  tl_to_tl_result

(* TemplateLiteralT on the left, against any non-TemplateLiteralT upper.
   Enumerate the producible strings (expanding generics) and flow as a
   singleton or union; fall back to a generic string type when the cross
   product can't be resolved. *)
val subtype_template_to_other :
  rec_flow_t:(Context.t -> Type.DepthTrace.t -> use_op:Type.use_op -> Type.t * Type.t -> unit) ->
  possible_concrete_types_for_inspection:(Context.t -> Reason.t -> Type.t -> Type.t list) ->
  Context.t ->
  Type.DepthTrace.t ->
  Type.use_op ->
  reason:Reason.t ->
  upper:Type.t ->
  string list ->
  Type.t list ->
  unit
