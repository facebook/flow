(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type annot = unit

module Type : sig
  val error : annot Ast.Type.t'

  module Object : sig
    module Property : sig
      val error : annot Ast.Type.Object.Property.t'
    end

    module Indexer : sig
      val error : annot Ast.Type.Object.Indexer.t'
    end

    module InternalSlot : sig
      val error : annot Ast.Type.Object.InternalSlot.t'
    end

    module SpreadProperty : sig
      val error : annot Ast.Type.Object.SpreadProperty.t'
    end
  end
end

module Expression : sig
  val error : annot Ast.Expression.t'
  val expression_or_spread_list_error : annot Ast.Expression.expression_or_spread list
  val unimplemented : annot Ast.Expression.t'
  val targs_unimplemented : annot Ast.Type.ParameterInstantiation.t option
  val expression_or_spread_list_unimplemented : annot Ast.Expression.expression_or_spread list
  module Object : sig
    val property_error : annot Ast.Expression.Object.property
  end
end

module Pattern : sig
  val error : annot Ast.Pattern.t'
  val unimplemented : annot Ast.Pattern.t'
end

module Function : sig
  val unimplemented : annot Ast.Function.t
end
