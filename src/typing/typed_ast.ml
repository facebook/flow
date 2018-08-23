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

let error_annot = Loc.none, Type.Locationless.AnyT.t

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
      params = Loc.none, Params.error;
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
        id = Some (Loc.none, "Error");
        key = error_annot, error;
        value = error_annot, error;
        static = false;
        variance = None;
      }
    end

    module InternalSlot = struct
      open InternalSlot
      let error = {
        id = Loc.none, "Error";
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
  let error = Identifier (Loc.none, "Error")
  let expression_or_spread_list_error = [ Expression (error_annot, error) ]
  let unimplemented = Identifier (Loc.none, "Unimplemented")
  let targs_unimplemented = None
  let expression_or_spread_list_unimplemented = [ Expression (error_annot, unimplemented) ]

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
      Property (Loc.none, Property.error)
  end
end

module Pattern = struct
  open Ast.Pattern
  let error = Expression (error_annot, Expression.error)
  let unimplemented = Expression (error_annot, Expression.unimplemented)
end

module Statement = struct
  open Ast.Statement
  let error = Loc.none, Labeled { Labeled.
    label = Loc.none, "Error";
    body = Loc.none, Empty;
  }
  module Try = struct
    open Try
    module CatchClause = struct
      open CatchClause
      let error = {
        param = None;
        body = Loc.none, { Ast.Statement.Block.body = [error] }
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
      id = Loc.none, "Error";
      annot = Loc.none, (error_annot, Ast.Type.Function Type.Function.error);
      predicate = None;
    }
  end
end

module Function = struct
  open Ast.Function
  let body_error = BodyExpression (error_annot, Expression.error)
  let body_unimplemented = BodyExpression (error_annot, Expression.unimplemented)
  let unimplemented = {
      id = Some (error_annot, "Unimplemented");
      params = Loc.none, { Params.params = []; rest = None; };
      body = body_unimplemented;
      async = false;
      generator = false;
      predicate = None;
      expression = false;
      return = None;
      tparams = None;
    }

  module RestElement = struct
    open RestElement
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
    params = Loc.none, Params.error;
    body = BodyExpression (error_annot, Expression.error);
    async = false;
    generator = false;
    predicate = None;
    expression = false;
    return = None;
    tparams = None;
  }

end

module Class = struct
  open Ast.Class
  let unimplemented = {
    id = Some (error_annot, "Unimplemented");
    body = error_annot, { Ast.Class.Body.body = [] };
    tparams = None;
    extends = None;
    implements = [];
    classDecorators = [];
  }

  module Body = struct
    open Body
    let element_error = Method (Loc.none, { Method.
      kind = Method.Method;
      key = Expression.Object.Property.key_error;
      value = Loc.none, Function.error;
      static = false;
      decorators = [];
    })
  end
end
