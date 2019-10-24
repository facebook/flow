(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sort = Signature_builder_kind.Sort

module type S = sig
  module L : Loc_sig.S

  module Error : sig
    type t =
      | ExpectedSort of Sort.t * string * L.t
      | ExpectedAnnotation of L.t * Expected_annotation_sort.t
      | InvalidTypeParamUse of L.t
      | UnexpectedObjectKey of L.t (* object loc *) * L.t (* key loc *)
      | UnexpectedObjectSpread of L.t (* object loc *) * L.t (* spread loc *)
      | UnexpectedArraySpread of L.t (* array loc *) * L.t (* spread loc *)
      | UnexpectedArrayHole of L.t (* array loc *)
      | EmptyArray of L.t (* array loc *)
      | EmptyObject of L.t (* object loc *)
      | UnexpectedExpression of L.t * Flow_ast_utils.ExpressionSort.t
      | SketchyToplevelDef of L.t
      | UnsupportedPredicateExpression of L.t
      | TODO of string * L.t

    val compare : t -> t -> int

    val debug_to_string : t -> string
  end

  module PrintableErrorSet : Set.S with type elt = Error.t

  module Dep : sig
    type t =
      | Local of local
      | Dynamic of dynamic
      | Remote of remote

    and local = Sort.t * string

    and dynamic =
      | Class of L.t * string
      | DynamicImport of L.t
      | DynamicRequire of L.t

    and remote =
      | ImportNamed of {
          sort: Sort.t;
          source: L.t Flow_ast_utils.source;
          name: L.t Flow_ast_utils.ident;
        }
      | ImportStar of {
          sort: Sort.t;
          source: L.t Flow_ast_utils.source;
        }
      | Require of {
          source: L.t Flow_ast_utils.source;
          name: L.t Flow_ast_utils.ident Nel.t option;
        }
      | Global of local

    val compare : t -> t -> int

    val expectation : Sort.t -> string -> L.t -> Error.t

    val remote : t -> bool

    val local_uses : t -> SSet.t -> SSet.t

    val to_string : t -> string
  end

  module DepSet : Set.S with type elt = Dep.t

  type t = DepSet.t * PrintableErrorSet.t

  val join : t * t -> t

  val bot : t

  val top : Error.t -> t

  val unreachable : t

  val todo : L.t -> string -> t

  val unit : Dep.t -> t

  val type_ : string -> t

  val value : string -> t

  val dynamic_import : L.t -> t

  val dynamic_require : L.t -> t

  val import_named : Sort.t -> L.t Flow_ast_utils.source -> L.t Flow_ast_utils.ident -> t

  val import_star : Sort.t -> L.t Flow_ast_utils.source -> t

  val require : ?name:L.t Flow_ast_utils.ident Nel.t -> L.t Flow_ast_utils.source -> t

  val global : Dep.local -> t

  val reduce_join : ('a -> t) -> t -> 'a -> t

  val recurse : (Dep.t -> PrintableErrorSet.t) -> t -> PrintableErrorSet.t

  val replace_local_with_dynamic_class : L.t Flow_ast_utils.ident -> t -> t
end
