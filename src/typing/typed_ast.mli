(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module T = Type

type program = (ALoc.t, ALoc.t * Type.t) Flow_ast.program

val error_annot : ALoc.t * Type.t
val unimplemented_annot : ALoc.t * Type.t

module Type : sig
  val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Type.t'

  module Object : sig
    module Property : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Type.Object.Property.t'
    end

    module Indexer : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Type.Object.Indexer.t'
    end

    module InternalSlot : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Type.Object.InternalSlot.t'
    end

    module SpreadProperty : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Type.Object.SpreadProperty.t'
    end
  end
end

module Statement : sig
  val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Statement.t
  module Try : sig
    module CatchClause : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Statement.Try.CatchClause.t'
    end
  end
  module ForIn : sig
    val left_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Statement.ForIn.left
  end
  module ForOf : sig
    val left_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Statement.ForOf.left
  end
  module DeclareFunction : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Statement.DeclareFunction.t
  end
end

module Expression : sig
  val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.t'
  val expression_or_spread_list_error
    : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.expression_or_spread list
  val unimplemented : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.t'
  module Object : sig
    val property_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.Object.property
    module Property : sig
      val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.Object.Property.t'
      val key_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Expression.Object.Property.key
    end
  end
end

module Pattern : sig
  val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Pattern.t'
end

module Function : sig
  val body_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Function.body
  val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Function.t
  module RestParam : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Function.RestParam.t'
  end
  module Params : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.Function.Params.t'
  end
end

module Class : sig
  module Body : sig
    val element_error : (ALoc.t, ALoc.t * T.t) Flow_ast.Class.Body.element
  end
end

module JSX : sig
  module Identifier : sig
    val error : (ALoc.t * T.t) Flow_ast.JSX.Identifier.t
  end
  module Attribute : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.Attribute.t
  end
  module SpreadAttribute : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.SpreadAttribute.t
  end
  module MemberExpression : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.MemberExpression.t
    val error_object : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.MemberExpression._object
  end
  val error_name : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.name
  module Opening : sig
    val error_attribute_list :
      (ALoc.t, ALoc.t) Flow_ast.JSX.Opening.attribute list ->
      (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.Opening.attribute list
  end
  module Closing : sig
    val error : (ALoc.t, ALoc.t * T.t) Flow_ast.JSX.Closing.t
  end
end
