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

module Statement : sig
  val error : annot Ast.Statement.t
  module Try : sig
    module CatchClause : sig
      val error : annot Ast.Statement.Try.CatchClause.t'
    end
  end
  module ForIn : sig
    val left_error : annot Ast.Statement.ForIn.left
  end
  module ForOf : sig
    val left_error : annot Ast.Statement.ForOf.left
  end
  module DeclareFunction : sig
    val error : annot Ast.Statement.DeclareFunction.t
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
    module Property : sig
      val error : annot Ast.Expression.Object.Property.t'
      val key_error : annot Ast.Expression.Object.Property.key
    end
  end
end

module Pattern : sig
  val error : annot Ast.Pattern.t'
  val unimplemented : annot Ast.Pattern.t'
end

module Function : sig
  val body_error : annot Ast.Function.body
  val error : annot Ast.Function.t
  val body_unimplemented : annot Ast.Function.body
  val unimplemented : annot Ast.Function.t
  module RestElement : sig
    val error : annot Ast.Function.RestElement.t'
  end
  module Params : sig
    val error : annot Ast.Function.Params.t'
  end
end

module Class : sig
  val unimplemented : annot Ast.Class.t
  module Body : sig
    val element_error : annot Ast.Class.Body.element
  end
end
