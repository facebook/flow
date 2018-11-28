(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(** Error AST nodes. These are used when, while generating the typed AST,
    errors prevent some part of the original AST from being translated.
    These are all chosen arbitrarily, and currently nothing relies on them
    being these specific values.
    TODO(vijayramamurthy): redo error nodes in a safer way (e.g. add error
    constructors to the datatype)

    This module also contains "unimplemented" AST nodes; these will be deleted once
    we've finished implementing the typed AST translation
  *)
module T = Type

type program = (ALoc.t, ALoc.t * Type.t) Ast.program

let error_annot = ALoc.none, Type.AnyT.locationless Type.AnyError
let unimplemented_annot = ALoc.none, Type.Unsoundness.unimplemented |> Type.AnyT.locationless

module Type = struct
  open Ast.Type
  let error =
    Generic { Ast.Type.Generic.
      id = (Ast.Type.Generic.Identifier.Unqualified (error_annot, "Error"));
      targs = None
    }

  module Function = struct
    open Function

    module Params = struct
      let error = { Params.
        params = [];
        rest = None;
      }
    end

    let error = {
      tparams = None;
      params = ALoc.none, Params.error;
      return = error_annot, error;
    }
  end

  module Object = struct
    open Object

    module Property = struct
      open Property
      let error = {
        key = Ast.Expression.Object.Property.Literal (error_annot, { Ast.Literal.
          value = Ast.Literal.Null;
          raw = "Error";
        });
        value = Init (error_annot, error);
        optional = false;
        static = false;
        proto = false;
        _method = false;
        variance = None
      }
    end

    module Indexer = struct
      open Indexer
      let error = {
        id = Some (ALoc.none, "Error");
        key = error_annot, error;
        value = error_annot, error;
        static = false;
        variance = None;
      }
    end

    module InternalSlot = struct
      open InternalSlot
      let error = {
        id = ALoc.none, "Error";
        value = error_annot, error;
        optional = false;
        static = false;
        _method = false;
      }
    end

    module SpreadProperty = struct
      open SpreadProperty
      let error = {
        argument = error_annot, error
      }
    end
  end
end

module Expression = struct
  open Ast.Expression
  let error = Identifier (error_annot, "Error")
  let expression_or_spread_list_error = [ Expression (error_annot, error) ]
  let unimplemented = Identifier (unimplemented_annot, "Unimplemented")

  module Object = struct
    open Object
    module Property = struct
      open Property
      let key_error = Property.Identifier (error_annot, "Error")
      let error = Init {
        key = key_error;
        value = error_annot, error;
        shorthand = false;
      }
    end
    let property_error =
      Property (ALoc.none, Property.error)
  end
end

module Pattern = struct
  open Ast.Pattern
  let error = Expression (error_annot, Expression.error)
end

module Statement = struct
  open Ast.Statement
  let error = ALoc.none, Labeled { Labeled.
    label = ALoc.none, "Error";
    body = ALoc.none, Empty;
  }
  module Try = struct
    open Try
    module CatchClause = struct
      open CatchClause
      let error = {
        param = None;
        body = ALoc.none, { Ast.Statement.Block.body = [error] }
      }
    end
  end
  module ForIn = struct
    open ForIn
    let left_error = LeftPattern (error_annot, Pattern.error)
  end
  module ForOf = struct
    open ForOf
    let left_error = LeftPattern (error_annot, Pattern.error)
  end
  module DeclareFunction = struct
    open DeclareFunction
    let error = {
      id = ALoc.none, "Error";
      annot = ALoc.none, (error_annot, Ast.Type.Function Type.Function.error);
      predicate = None;
    }
  end
end

module Function = struct
  open Ast.Function
  let body_error = BodyExpression (error_annot, Expression.error)

  module RestParam = struct
    open RestParam
    let error = {
      argument = error_annot, Pattern.error
    }
  end

  module Params = struct
    (* open Params *)
    let error = { Params.
      params = [];
      rest = None;
    }
  end

  let error = {
    id = Some (error_annot, "Error");
    params = ALoc.none, Params.error;
    body = BodyExpression (error_annot, Expression.error);
    async = false;
    generator = false;
    predicate = None;
    return = Ast.Type.Missing error_annot;
    tparams = None;
    sig_loc = ALoc.none;
  }

end

module Class = struct
  open Ast.Class
  module Body = struct
    open Body
    let element_error = Method (error_annot, { Method.
      kind = Method.Method;
      key = Expression.Object.Property.key_error;
      value = ALoc.none, Function.error;
      static = false;
      decorators = [];
    })
  end
end

module JSX = struct
  module Identifier = struct
    open Ast.JSX.Identifier
    let error = error_annot, { name = "Error" }
  end

  module Attribute = struct
    let error_name =
      Ast.JSX.Attribute.Identifier Identifier.error

    let error = ALoc.none, {
      Ast.JSX.Attribute.name = error_name;
      value = None;
    }
  end

  module SpreadAttribute = struct
    let error = ALoc.none, {
      Ast.JSX.SpreadAttribute.argument = error_annot, Expression.error
    }
  end

  module MemberExpression = struct
    let error = ALoc.none, {
      Ast.JSX.MemberExpression._object =
        Ast.JSX.MemberExpression.Identifier Identifier.error;
      Ast.JSX.MemberExpression.property = Identifier.error;
    }
    let error_object = Ast.JSX.MemberExpression.Identifier (
      Identifier.error
    )
  end

  let error_name =
    Ast.JSX.Identifier Identifier.error

  module Opening = struct
    open Ast.JSX.Opening
    let error_attribute_list attributes =
      Core_list.map ~f:(function
        | Attribute _ -> Attribute Attribute.error
        | SpreadAttribute _ -> SpreadAttribute SpreadAttribute.error
      ) attributes
  end

  module Closing = struct
    open Ast.JSX.Closing
    let error = ALoc.none,  {
      name = error_name;
    }
  end
end
