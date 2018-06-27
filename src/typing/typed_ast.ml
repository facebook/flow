(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type annot = unit

(** Error AST nodes. These are used when, while generating the typed AST,
    errors prevent some part of the original AST from being translated.
    These are all chosen arbitrarily, and currently nothing relies on them
    being these specific values.
    TODO(vijayramamurthy): redo error nodes in a safer way (e.g. add error
    constructors to the datatype)

    This module also contains "unimplemented" AST nodes; these will be deleted once
    we've finished implementing the typed AST translation
  *)
module Type = struct
  open Ast.Type
  let error =
    Generic { Ast.Type.Generic.
      id = (Ast.Type.Generic.Identifier.Unqualified ((), "Error"));
      targs = None
    }

  module Object = struct
    open Object

    module Property = struct
      open Property
      let error = {
        key = Ast.Expression.Object.Property.Literal ((), { Ast.Literal.
          value = Ast.Literal.Null;
          raw = "Error";
        });
        value = Init ((), error);
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
        id = Some ((), "Error");
        key = (), error;
        value = (), error;
        static = false;
        variance = None;
      }
    end

    module InternalSlot = struct
      open InternalSlot
      let error = {
        id = (), "Error";
        value = (), error;
        optional = false;
        static = false;
        _method = false;
      }
    end

    module SpreadProperty = struct
      open SpreadProperty
      let error = {
        argument = (), error
      }
    end
  end
end

module Expression = struct
  open Ast.Expression
  let error = Identifier ((), "Error")
  let expression_or_spread_list_error = [ Expression ((), error) ]
  let unimplemented = Identifier ((), "Unimplemented")
  let targs_unimplemented = None
  let expression_or_spread_list_unimplemented = [ Expression ((), unimplemented) ]

  module Object = struct
    open Object
    let property_error =
      Property (
        (),
        Property.Init {
          key = Property.Identifier ((), "Error");
          value = (), error;
          shorthand = false;
        }
      )
  end
end

module Pattern = struct
  open Ast.Pattern
  let error = Expression ((), Expression.error)
  let unimplemented = Expression ((), Expression.unimplemented)
end

module Function = struct
  open Ast.Function
  let body_unimplemented = BodyExpression ((), Expression.unimplemented)
  let unimplemented = {
      id = Some ((), "Unimplemented");
      params = (), { Params.params = []; rest = None; };
      body = body_unimplemented;
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
    id = Some ((), "Unimplemented");
    body = (), { Ast.Class.Body.body = [] };
    tparams = None;
    super = None;
    super_targs = None;
    implements = [];
    classDecorators = [];
  }
end
