(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module T = Type

val error_annot : Loc.t * Type.t

module Type : sig
  val error : (Loc.t, Loc.t * T.t) Flow_ast.Type.t'

  module Object : sig
    module Property : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Type.Object.Property.t'
    end

    module Indexer : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Type.Object.Indexer.t'
    end

    module InternalSlot : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Type.Object.InternalSlot.t'
    end

    module SpreadProperty : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Type.Object.SpreadProperty.t'
    end
  end
end

module Statement : sig
  val error : (Loc.t, Loc.t * T.t) Flow_ast.Statement.t
  module Try : sig
    module CatchClause : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Statement.Try.CatchClause.t'
    end
  end
  module ForIn : sig
    val left_error : (Loc.t, Loc.t * T.t) Flow_ast.Statement.ForIn.left
  end
  module ForOf : sig
    val left_error : (Loc.t, Loc.t * T.t) Flow_ast.Statement.ForOf.left
  end
  module DeclareFunction : sig
    val error : (Loc.t, Loc.t * T.t) Flow_ast.Statement.DeclareFunction.t
  end
end

module Expression : sig
  val error : (Loc.t, Loc.t * T.t) Flow_ast.Expression.t'
  val expression_or_spread_list_error
    : (Loc.t, Loc.t * T.t) Flow_ast.Expression.expression_or_spread list
  val unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Expression.t'
  val targs_unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Type.ParameterInstantiation.t option
  val expression_or_spread_list_unimplemented
    : (Loc.t, Loc.t * T.t) Flow_ast.Expression.expression_or_spread list
  module Object : sig
    val property_error : (Loc.t, Loc.t * T.t) Flow_ast.Expression.Object.property
    module Property : sig
      val error : (Loc.t, Loc.t * T.t) Flow_ast.Expression.Object.Property.t'
      val key_error : (Loc.t, Loc.t * T.t) Flow_ast.Expression.Object.Property.key
    end
  end
end

module Pattern : sig
  val error : (Loc.t, Loc.t * T.t) Flow_ast.Pattern.t'
  val unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Pattern.t'
end

module Function : sig
  val body_error : (Loc.t, Loc.t * T.t) Flow_ast.Function.body
  val error : (Loc.t, Loc.t * T.t) Flow_ast.Function.t
  val body_unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Function.body
  val unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Function.t
  module RestElement : sig
    val error : (Loc.t, Loc.t * T.t) Flow_ast.Function.RestElement.t'
  end
  module Params : sig
    val error : (Loc.t, Loc.t * T.t) Flow_ast.Function.Params.t'
  end
end

module Class : sig
  val unimplemented : (Loc.t, Loc.t * T.t) Flow_ast.Class.t
  module Body : sig
    val element_error : (Loc.t, Loc.t * T.t) Flow_ast.Class.Body.element
  end
end
