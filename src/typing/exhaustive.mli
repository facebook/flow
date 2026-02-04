(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_class_info : Context.t -> Type.t -> (ALoc.id * string option) option

val analyze :
  Context.t ->
  match_loc:ALoc.t ->
  ((ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * (* guarded *) bool) list ->
  Type.t ->
  unit

(* Incremental PatternUnion building *)
module PatternUnionBuilder : sig
  val add_pattern :
    Context.t ->
    raise_errors:bool ->
    Match_pattern_ir.PatternUnion.t * int ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * bool ->
    last:bool ->
    Match_pattern_ir.PatternUnion.t * int

  val finalize : Match_pattern_ir.PatternUnion.t -> Match_pattern_ir.PatternUnion.t
end

(* Filter a ValueUnion by a finalized PatternUnion *)
val filter_by_pattern_union :
  Context.t -> Type.t -> Match_pattern_ir.PatternUnion.t -> Match_pattern_ir.ValueUnion.t
