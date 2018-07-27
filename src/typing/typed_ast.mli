(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Type : sig
  val error : (unit, unit) Ast.Type.t'

  module Object : sig
    module Property : sig
      val error : (unit, unit) Ast.Type.Object.Property.t'
    end

    module Indexer : sig
      val error : (unit, unit) Ast.Type.Object.Indexer.t'
    end

    module InternalSlot : sig
      val error : (unit, unit) Ast.Type.Object.InternalSlot.t'
    end

    module SpreadProperty : sig
      val error : (unit, unit) Ast.Type.Object.SpreadProperty.t'
    end
  end
end

module Statement : sig
  val error : (unit, unit) Ast.Statement.t
  module Try : sig
    module CatchClause : sig
      val error : (unit, unit) Ast.Statement.Try.CatchClause.t'
    end
  end
  module ForIn : sig
    val left_error : (unit, unit) Ast.Statement.ForIn.left
  end
  module ForOf : sig
    val left_error : (unit, unit) Ast.Statement.ForOf.left
  end
  module DeclareFunction : sig
    val error : (unit, unit) Ast.Statement.DeclareFunction.t
  end
end

module Expression : sig
  val error : (unit, unit) Ast.Expression.t'
  val expression_or_spread_list_error : (unit, unit) Ast.Expression.expression_or_spread list
  val unimplemented : (unit, unit) Ast.Expression.t'
  val targs_unimplemented : (unit, unit) Ast.Type.ParameterInstantiation.t option
  val expression_or_spread_list_unimplemented : (unit, unit) Ast.Expression.expression_or_spread list
  module Object : sig
    val property_error : (unit, unit) Ast.Expression.Object.property
    module Property : sig
      val error : (unit, unit) Ast.Expression.Object.Property.t'
      val key_error : (unit, unit) Ast.Expression.Object.Property.key
    end
  end
end

module Pattern : sig
  val error : (unit, unit) Ast.Pattern.t'
  val unimplemented : (unit, unit) Ast.Pattern.t'
end

module Function : sig
  val body_error : (unit, unit) Ast.Function.body
  val error : (unit, unit) Ast.Function.t
  val body_unimplemented : (unit, unit) Ast.Function.body
  val unimplemented : (unit, unit) Ast.Function.t
  module RestElement : sig
    val error : (unit, unit) Ast.Function.RestElement.t'
  end
  module Params : sig
    val error : (unit, unit) Ast.Function.Params.t'
  end
end

module Class : sig
  val unimplemented : (unit, unit) Ast.Class.t
  module Body : sig
    val element_error : (unit, unit) Ast.Class.Body.element
  end
end
