(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * An Ocaml implementation of the SpiderMonkey Parser API
 * https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
 *)

module%gen rec Identifier : sig
  type 'M t = 'M * string
  [@@deriving show]
end = Identifier

and PrivateName : sig
  type 'M t = 'M * 'M Identifier.t
  [@@deriving show]
end = PrivateName

and Literal : sig
  module RegExp : sig
    type t = {
      pattern: string;
      flags: string;
    }
    [@@deriving show]
  end

  (* Literals also carry along their raw value *)
  type t = {
    value: value;
    raw: string;
  }
  and value =
    | String of string
    | Boolean of bool
    | Null
    | Number of float
    | RegExp of RegExp.t
  [@@deriving show]
end = Literal

and StringLiteral : sig
  type t = {
    value: string;
    raw: string;
  }
  [@@deriving show]
end = StringLiteral

and NumberLiteral : sig
  type t = {
    value: float;
    raw: string;
  }
  [@@deriving show]
end = NumberLiteral

and Variance : sig
  type 'M t = 'M * t'
  and t' = Plus | Minus
  [@@deriving show]
end = Variance

and Type : sig
  module Function : sig
    module Param : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        name: 'M Identifier.t option;
        annot: 'M Type.t;
        optional: bool;
      }
      [@@deriving show]
    end
    module RestParam : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        argument: 'M Param.t
      }
      [@@deriving show]
    end
    module Params : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        params: 'M Param.t list;
        rest: 'M RestParam.t option;
      }
      [@@deriving show]
    end
    type 'M t = {
      tparams: 'M Type.ParameterDeclaration.t option;
      params: 'M Params.t;
      return: 'M Type.t;
    }
    [@@deriving show]
  end

  module Generic : sig
    module Identifier : sig
      type 'M t =
        | Unqualified of 'M Identifier.t
        | Qualified of 'M qualified
      and 'M qualified = 'M * 'M qualified'
      and 'M qualified' = {
        qualification: 'M t;
        id: 'M Identifier.t
      }
      [@@deriving show]
    end
    type 'M t = {
      id: 'M Identifier.t;
      targs: 'M Type.ParameterInstantiation.t option;
    }
    [@@deriving show]
  end

  module Object : sig
    module Property : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        key: 'M Expression.Object.Property.key;
        value: 'M value;
        optional: bool;
        static: bool;
        proto: bool;
        _method: bool;
        variance: 'M Variance.t option;
      }
      and 'M value =
        | Init of 'M Type.t
        | Get of ('M * 'M Function.t)
        | Set of ('M * 'M Function.t)
      [@@deriving show]
    end
    module SpreadProperty : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        argument: 'M Type.t;
      }
      [@@deriving show]
    end
    module Indexer: sig
      type 'M t' = {
        id: 'M Identifier.t option;
        key: 'M Type.t;
        value: 'M Type.t;
        static: bool;
        variance: 'M Variance.t option;
      }
      and 'M t = 'M * 'M t'
      [@@deriving show]
    end
    module CallProperty: sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        value: 'M * 'M Function.t;
        static: bool;
      }
      [@@deriving show]
    end
    module InternalSlot: sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        id: 'M Identifier.t;
        value: 'M Type.t;
        optional: bool;
        static: bool;
        _method: bool;
      }
      [@@deriving show]
    end

    type 'M t = {
      exact: bool;
      properties: 'M property list;
    }
    and 'M property =
      | Property of 'M Property.t
      | SpreadProperty of 'M SpreadProperty.t
      | Indexer of 'M Indexer.t
      | CallProperty of 'M CallProperty.t
      | InternalSlot of 'M InternalSlot.t
    [@@deriving show]
  end

  module Interface : sig
    type 'M t = {
      body: 'M * 'M Object.t;
      extends: ('M * 'M Generic.t) list;
    }
    [@@deriving show]
  end

  type 'M t = 'M * 'M t'
  (* Yes, we could add a little complexity here to show that Any and Void
   * should never be declared nullable, but that check can happen later *)
  and 'M t' =
    | Any
    | Mixed
    | Empty
    | Void
    | Null
    | Number
    | String
    | Boolean
    | Nullable of 'M t
    | Function of 'M Function.t
    | Object of 'M Object.t
    | Interface of 'M Interface.t
    | Array of 'M t
    | Generic of 'M Generic.t
    | Union of 'M t * 'M t * 'M t list
    | Intersection of 'M t * 'M t * 'M t list
    | Typeof of 'M t
    | Tuple of 'M t list
    | StringLiteral of StringLiteral.t
    | NumberLiteral of NumberLiteral.t
    | BooleanLiteral of bool
    | Exists

  (* Type.annotation is a concrete syntax node with a location that starts at
   * the colon and ends after the type. For example, "var a: number", the
   * identifier a would have a property annot which contains a
   * Type.annotation with a location from column 6-14 *)
  and 'M annotation = 'M * 'M t

  [@@deriving show]

  module ParameterDeclaration : sig
    module TypeParam : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        name: 'M Identifier.t;
        bound: 'M Type.annotation option;
        variance: 'M Variance.t option;
        default: 'M Type.t option;
      }
      [@@deriving show]
    end
    type 'M t = 'M * 'M t'
    and 'M t' = 'M TypeParam.t list
    [@@deriving show]
  end
  module ParameterInstantiation : sig
    type 'M t = 'M * 'M t'
    and 'M t' = 'M Type.t list
    [@@deriving show]
  end

  module Predicate : sig
    type 'M t = 'M * 'M t'
    and 'M t' =
      | Declared of 'M Expression.t
      | Inferred
    [@@deriving show]
  end

end = Type

and Statement : sig
  module Block : sig
    type 'M t = {
      body: 'M Statement.t list
    }
    [@@deriving show]
  end
  module If : sig
    type 'M t = {
      test: 'M Expression.t;
      consequent: 'M Statement.t;
      alternate: 'M Statement.t option;
    }
    [@@deriving show]
  end
  module Labeled : sig
    type 'M t = {
      label: 'M Identifier.t;
      body: 'M Statement.t;
    }
    [@@deriving show]
  end
  module Break : sig
    type 'M t = {
      label: 'M Identifier.t option;
    }
    [@@deriving show]
  end
  module Continue : sig
    type 'M t = {
      label: 'M Identifier.t option;
    }
    [@@deriving show]
  end
  module With : sig
    type 'M t = {
      _object: 'M Expression.t;
      body: 'M Statement.t;
    }
    [@@deriving show]
  end
  module TypeAlias : sig
    type 'M t = {
      id: 'M Identifier.t;
      tparams: 'M Type.ParameterDeclaration.t option;
      right: 'M Type.t;
    }
    [@@deriving show]
  end
  module OpaqueType: sig
    type 'M t = {
      id: 'M Identifier.t;
      tparams: 'M Type.ParameterDeclaration.t option;
      impltype: 'M Type.t option;
      supertype: 'M Type.t option;
    }
    [@@deriving show]
  end
  module Switch : sig
    module Case : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        test: 'M Expression.t option;
        consequent: 'M Statement.t list;
      }
      [@@deriving show]
    end
    type 'M t = {
      discriminant: 'M Expression.t;
      cases: 'M Case.t list;
    }
    [@@deriving show]
  end
  module Return : sig
    type 'M t = {
      argument: 'M Expression.t option;
    }
    [@@deriving show]
  end
  module Throw : sig
    type 'M t = {
      argument: 'M Expression.t;
    }
    [@@deriving show]
  end
  module Try : sig
    module CatchClause : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        param: 'M Pattern.t option;
        body: 'M * 'M Block.t;
      }
      [@@deriving show]
    end
    type 'M t = {
      block: 'M * 'M Block.t;
      handler: 'M CatchClause.t option;
      finalizer: ('M * 'M Block.t) option;
    }
    [@@deriving show]
  end
  module VariableDeclaration : sig
    module Declarator : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        id: 'M Pattern.t;
        init: 'M Expression.t option;
      }
      [@@deriving show]
    end

    type 'M t = {
      declarations: 'M Declarator.t list;
      kind: kind;
    }
    and kind =
      | Var
      | Let
      | Const
    [@@deriving show]
  end
  module While : sig
    type 'M t = {
      test: 'M Expression.t;
      body: 'M Statement.t;
    }
    [@@deriving show]
  end
  module DoWhile : sig
    type 'M t = {
      body: 'M Statement.t;
      test: 'M Expression.t;
    }
    [@@deriving show]
  end
  module For : sig
    type 'M t = {
      init: 'M init option;
      test: 'M Expression.t option;
      update: 'M Expression.t option;
      body: 'M Statement.t;
    }
    and 'M init =
      | InitDeclaration of ('M * 'M VariableDeclaration.t)
      | InitExpression of 'M Expression.t
    [@@deriving show]
  end
  module ForIn : sig
    type 'M t = {
      left: 'M left;
      right: 'M Expression.t;
      body: 'M Statement.t;
      each: bool;
    }
    and 'M left =
      | LeftDeclaration of ('M * 'M VariableDeclaration.t)
      | LeftPattern of 'M Pattern.t
    [@@deriving show]
  end
  module ForOf : sig
    type 'M t = {
      left: 'M left;
      right: 'M Expression.t;
      body: 'M Statement.t;
      async: bool;
    }
    and 'M left =
      | LeftDeclaration of ('M * 'M VariableDeclaration.t)
      | LeftPattern of 'M Pattern.t
    [@@deriving show]
  end
  module Interface : sig
    type 'M t = {
      id: 'M Identifier.t;
      tparams: 'M Type.ParameterDeclaration.t option;
      extends: ('M * 'M Type.Generic.t) list;
      body: 'M * 'M Type.Object.t;
    }
    [@@deriving show]
  end
  module DeclareClass : sig
    type 'M t = {
      id: 'M Identifier.t;
      tparams: 'M Type.ParameterDeclaration.t option;
      body: 'M * 'M Type.Object.t;
      extends: ('M * 'M Type.Generic.t) option;
      mixins: ('M * 'M Type.Generic.t) list;
      implements: 'M Class.Implements.t list;
    }
    [@@deriving show]
  end
  module DeclareVariable : sig
    type 'M t = {
      id: 'M Identifier.t;
      annot: 'M Type.annotation option;
    }
    [@@deriving show]
  end
  module DeclareFunction : sig
    type 'M t = {
      id: 'M Identifier.t;
      annot: 'M Type.annotation;
      predicate: 'M Type.Predicate.t option;
    }
    [@@deriving show]
  end
  module DeclareModule : sig
    type 'M id =
      | Identifier of 'M Identifier.t
      | Literal of ('M * StringLiteral.t)

    and 'M module_kind =
      | CommonJS of 'M
      | ES of 'M

    and 'M t = {
      id: 'M id;
      body: 'M * 'M Block.t;
      kind: 'M module_kind;
    }

    [@@deriving show]
  end
  module ExportNamedDeclaration : sig
    module ExportSpecifier : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        local: 'M Identifier.t;
        exported: 'M Identifier.t option;
      }
      [@@deriving show]
    end

    type 'M t = {
      declaration: 'M Statement.t option;
      specifiers: 'M specifier option;
      source: ('M * StringLiteral.t) option;
      exportKind: Statement.exportKind;
    }
    and 'M specifier =
      | ExportSpecifiers of 'M ExportSpecifier.t list
      | ExportBatchSpecifier of 'M * 'M Identifier.t option
    [@@deriving show]
  end
  module ExportDefaultDeclaration : sig
    type 'M t = {
      default: 'M;
      declaration: 'M declaration;
    }
    and 'M declaration =
      | Declaration of 'M Statement.t
      | Expression of 'M Expression.t
    [@@deriving show]
  end
  module DeclareExportDeclaration : sig
    type 'M declaration =
      (* declare export var *)
      | Variable of ('M * 'M DeclareVariable.t)
      (* declare export function *)
      | Function of ('M * 'M DeclareFunction.t)
      (* declare export class *)
      | Class of ('M * 'M DeclareClass.t)
      (* declare export default [type]
       * this corresponds to things like
       * export default 1+1; *)
      | DefaultType of 'M Type.t
      (* declare export type *)
      | NamedType of ('M * 'M TypeAlias.t)
      (* declare export opaque type *)
      | NamedOpaqueType of ('M * 'M OpaqueType.t)
      (* declare export interface *)
      | Interface of ('M * 'M Interface.t)

    and 'M t = {
      default: 'M option;
      declaration: 'M declaration option;
      specifiers: 'M ExportNamedDeclaration.specifier option;
      source: ('M * StringLiteral.t) option;
    }

    [@@deriving show]
  end
  module ImportDeclaration : sig
    type importKind =
      | ImportType
      | ImportTypeof
      | ImportValue

    and 'M specifier =
      | ImportNamedSpecifiers of 'M named_specifier list
      | ImportNamespaceSpecifier of ('M * 'M Identifier.t)

    and 'M named_specifier = {
      kind: importKind option;
      local: 'M Identifier.t option;
      remote: 'M Identifier.t;
    }

    and 'M t = {
      importKind: importKind;
      source: ('M * StringLiteral.t);
      default: 'M Identifier.t option;
      specifiers: 'M specifier option;
    }

    [@@deriving show]
  end
  module Expression : sig
    type 'M t = {
      expression: 'M Expression.t;
      directive: string option;
    }
    [@@deriving show]
  end

  type exportKind =
    | ExportType
    | ExportValue

  and 'M t = 'M * 'M t'
  and 'M t' =
    | Block of 'M Block.t
    | Break of 'M Break.t
    | ClassDeclaration of 'M Class.t
    | Continue of 'M Continue.t
    | Debugger
    | DeclareClass of 'M DeclareClass.t
    | DeclareExportDeclaration of 'M DeclareExportDeclaration.t
    | DeclareFunction of 'M DeclareFunction.t
    | DeclareInterface of 'M Interface.t
    | DeclareModule of 'M DeclareModule.t
    | DeclareModuleExports of 'M Type.annotation
    | DeclareTypeAlias of 'M TypeAlias.t
    | DeclareOpaqueType of 'M OpaqueType.t
    | DeclareVariable of 'M DeclareVariable.t
    | DoWhile of 'M DoWhile.t
    | Empty
    | ExportDefaultDeclaration of 'M ExportDefaultDeclaration.t
    | ExportNamedDeclaration of 'M ExportNamedDeclaration.t
    | Expression of 'M Expression.t
    | For of 'M For.t
    | ForIn of 'M ForIn.t
    | ForOf of 'M ForOf.t
    | FunctionDeclaration of 'M Function.t
    | If of 'M If.t
    | ImportDeclaration of 'M ImportDeclaration.t
    | InterfaceDeclaration of 'M Interface.t
    | Labeled of 'M Labeled.t
    | Return of 'M Return.t
    | Switch of 'M Switch.t
    | Throw of 'M Throw.t
    | Try of 'M Try.t
    | TypeAlias of 'M TypeAlias.t
    | OpaqueType of 'M OpaqueType.t
    | VariableDeclaration of 'M VariableDeclaration.t
    | While of 'M While.t
    | With of 'M With.t

  [@@deriving show]
end = Statement

and Expression : sig
  module SpreadElement : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      argument: 'M Expression.t;
    }
    [@@deriving show]
  end

  type 'M expression_or_spread =
    | Expression of 'M Expression.t
    | Spread of 'M SpreadElement.t
    [@@deriving show]

  module Array : sig
    type 'M t = {
      elements: 'M expression_or_spread option list;
    }
    [@@deriving show]
  end
  module TemplateLiteral : sig
    module Element : sig
      type value = {
        raw: string;
        cooked: string;
      }
      and 'M t = 'M * t'
      and t' = {
        value: value;
        tail: bool;
      }
      [@@deriving show]
    end
    type 'M t = {
      quasis: 'M Element.t list;
      expressions: 'M Expression.t list;
    }
    [@@deriving show]
  end
  module TaggedTemplate : sig
    type 'M t = {
      tag: 'M Expression.t;
      quasi: 'M * 'M TemplateLiteral.t;
    }
    [@@deriving show]
  end
  module Object : sig
    module Property : sig
      type 'M key =
        | Literal of ('M * Literal.t)
        | Identifier of 'M Identifier.t
        | PrivateName of 'M PrivateName.t
        | Computed of 'M Expression.t
      and 'M t = 'M * 'M t'
      and 'M t' =
        | Init of {
            key: 'M key;
            value: 'M Expression.t;
            shorthand: bool;
          }
        | Method of {
            key: 'M key;
            value: 'M * 'M Function.t;
          }
        | Get of {
            key: 'M key;
            value: 'M * 'M Function.t;
          }
        | Set of {
            key: 'M key;
            value: 'M * 'M Function.t;
          }

      [@@deriving show]
    end
    module SpreadProperty : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        argument: 'M Expression.t;
      }
      [@@deriving show]
    end

    type 'M property =
      | Property of 'M Property.t
      | SpreadProperty of 'M SpreadProperty.t

    and 'M t = {
      properties: 'M property list;
    }

    [@@deriving show]
  end
  module Sequence : sig
    type 'M t = {
      expressions: 'M Expression.t list;
    }
    [@@deriving show]
  end
  module Unary : sig
    type operator =
      | Minus
      | Plus
      | Not
      | BitNot
      | Typeof
      | Void
      | Delete
      | Await

    and 'M t = {
      operator: operator;
      prefix: bool;
      argument: 'M Expression.t
    }

    [@@deriving show]
  end
  module Binary : sig
    type operator =
      | Equal
      | NotEqual
      | StrictEqual
      | StrictNotEqual
      | LessThan
      | LessThanEqual
      | GreaterThan
      | GreaterThanEqual
      | LShift
      | RShift
      | RShift3
      | Plus
      | Minus
      | Mult
      | Exp
      | Div
      | Mod
      | BitOr
      | Xor
      | BitAnd
      | In
      | Instanceof

    and 'M t = {
      operator: operator;
      left: 'M Expression.t;
      right: 'M Expression.t;
    }

    [@@deriving show]
  end
  module Assignment : sig
    type operator =
      | Assign
      | PlusAssign
      | MinusAssign
      | MultAssign
      | ExpAssign
      | DivAssign
      | ModAssign
      | LShiftAssign
      | RShiftAssign
      | RShift3Assign
      | BitOrAssign
      | BitXorAssign
      | BitAndAssign

    and 'M t = {
      operator: operator;
      left: 'M Pattern.t;
      right: 'M Expression.t;
    }

    [@@deriving show]
  end
  module Update : sig
    type operator =
      | Increment
      | Decrement

    and 'M t = {
      operator: operator;
      argument: 'M Expression.t;
      prefix: bool;
    }

    [@@deriving show]
  end
  module Logical : sig
    type operator =
      | Or
      | And
      | NullishCoalesce

    and 'M t = {
      operator: operator;
      left: 'M Expression.t;
      right: 'M Expression.t;
    }

    [@@deriving show]
  end
  module Conditional : sig
    type 'M t = {
      test: 'M Expression.t;
      consequent: 'M Expression.t;
      alternate: 'M Expression.t;
    }
    [@@deriving show]
  end
  module New : sig
    type 'M t = {
      callee: 'M Expression.t;
      targs: 'M Type.ParameterInstantiation.t option;
      arguments: 'M expression_or_spread list;
    }
    [@@deriving show]
  end
  module Call : sig
    type 'M t = {
      callee: 'M Expression.t;
      targs: 'M Type.ParameterInstantiation.t option;
      arguments: 'M expression_or_spread list;
    }
    [@@deriving show]
  end
  module OptionalCall : sig
    type 'M t = {
      call: 'M Call.t;
      optional: bool;
    }
    [@@deriving show]
  end
  module Member : sig
    type 'M property =
      | PropertyIdentifier of 'M Identifier.t
      | PropertyPrivateName of 'M PrivateName.t
      | PropertyExpression of 'M Expression.t

    and 'M t = {
      _object: 'M Expression.t;
      property: 'M property;
      computed: bool;
    }

    [@@deriving show]
  end
  module OptionalMember : sig
    type 'M t = {
      member: 'M Member.t;
      optional: bool;
    }
    [@@deriving show]
  end
  module Yield : sig
    type 'M t = {
      argument: 'M Expression.t option;
      delegate: bool;
    }
    [@@deriving show]
  end
  module Comprehension : sig
    module Block : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        left: 'M Pattern.t;
        right: 'M Expression.t;
        each: bool;
      }
      [@@deriving show]
    end
    type 'M t = {
      blocks: 'M Block.t list;
      filter: 'M Expression.t option;
    }
    [@@deriving show]
  end
  module Generator : sig
    type 'M t = {
      blocks: 'M Comprehension.Block.t list;
      filter: 'M Expression.t option;
    }
    [@@deriving show]
  end
  module TypeCast : sig
    type 'M t = {
      expression: 'M Expression.t;
      annot: 'M Type.annotation;
    }
    [@@deriving show]
  end
  module MetaProperty : sig
    type 'M t = {
      meta: 'M Identifier.t;
      property: 'M Identifier.t;
    }
    [@@deriving show]
  end

  type 'M t = 'M * 'M t'
  and 'M t' =
    | Array of 'M Array.t
    | ArrowFunction of 'M Function.t
    | Assignment of 'M Assignment.t
    | Binary of 'M Binary.t
    | Call of 'M Call.t
    | Class of 'M Class.t
    | Comprehension of 'M Comprehension.t
    | Conditional of 'M Conditional.t
    | Function of 'M Function.t
    | Generator of 'M Generator.t
    | Identifier of 'M Identifier.t
    | Import of 'M t
    | JSXElement of 'M JSX.element
    | JSXFragment of 'M JSX.fragment
    | Literal of Literal.t
    | Logical of 'M Logical.t
    | Member of 'M Member.t
    | MetaProperty of 'M MetaProperty.t
    | New of 'M New.t
    | Object of 'M Object.t
    | OptionalCall of 'M OptionalCall.t
    | OptionalMember of 'M OptionalMember.t
    | Sequence of 'M Sequence.t
    | Super
    | TaggedTemplate of 'M TaggedTemplate.t
    | TemplateLiteral of 'M TemplateLiteral.t
    | This
    | TypeCast of 'M TypeCast.t
    | Unary of 'M Unary.t
    | Update of 'M Update.t
    | Yield of 'M Yield.t

  [@@deriving show]
end = Expression

and JSX : sig
  module Identifier : sig
    type 'M t = 'M * t'
    and t' = {
      name: string;
    }
    [@@deriving show]
  end

  module NamespacedName : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      namespace: 'M Identifier.t;
      name: 'M Identifier.t;
    }
    [@@deriving show]
  end

  module ExpressionContainer : sig
    type 'M t = {
      expression: 'M expression;
    }

    and 'M expression =
      | Expression of 'M Expression.t
      | EmptyExpression of 'M

    [@@deriving show]
  end

  module Text : sig
    type t = {
      value: string;
      raw: string;
    }
    [@@deriving show]
  end

  module Attribute : sig
    type 'M t = 'M * 'M t'

    and 'M name =
      | Identifier of 'M Identifier.t
      | NamespacedName of 'M NamespacedName.t

    and 'M value =
      | Literal of 'M * Literal.t
      | ExpressionContainer of 'M * 'M ExpressionContainer.t

    and 'M t' = {
      name: 'M name;
      value: 'M value option;
    }

    [@@deriving show]
  end

  module SpreadAttribute : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      argument: 'M Expression.t;
    }
    [@@deriving show]
  end

  module MemberExpression : sig
    type 'M t = 'M * 'M t'

    and 'M _object =
      | Identifier of 'M Identifier.t
      | MemberExpression of 'M t

    and 'M t' = {
      _object: 'M _object;
      property: 'M Identifier.t;
    }

    [@@deriving show]
  end

  type 'M name =
    | Identifier of 'M Identifier.t
    | NamespacedName of 'M NamespacedName.t
    | MemberExpression of 'M MemberExpression.t
    [@@deriving show]

  module Opening : sig
    type 'M t = 'M * 'M t'

    and 'M attribute =
      | Attribute of 'M Attribute.t
      | SpreadAttribute of 'M SpreadAttribute.t

    and 'M t' = {
      name: 'M name;
      selfClosing: bool;
      attributes: 'M attribute list;
    }

    [@@deriving show]
  end

  module Closing : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      name: 'M name;
    }
    [@@deriving show]
  end

  type 'M child = 'M * 'M child'
  and 'M child' =
    | Element of 'M element
    | Fragment of 'M fragment
    | ExpressionContainer of 'M ExpressionContainer.t
    | SpreadChild of 'M Expression.t
    | Text of Text.t

  and 'M element = {
    openingElement: 'M Opening.t;
    closingElement: 'M Closing.t option;
    children: 'M child list
  }

  and 'M fragment = {
    frag_openingElement: 'M;
    frag_closingElement: 'M option;
    frag_children: 'M child list;
  }

  [@@deriving show]
end = JSX

and Pattern : sig
  module Object : sig
    module Property : sig
      type 'M key =
        | Literal of ('M * Literal.t)
        | Identifier of 'M Identifier.t
        | Computed of 'M Expression.t
      and 'M t = 'M * 'M t'
      and 'M t' = {
        key: 'M key;
        pattern: 'M Pattern.t;
        shorthand: bool;
      }
      [@@deriving show]
    end
    module RestProperty : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        argument: 'M Pattern.t;
      }
      [@@deriving show]
    end
    type 'M property =
      | Property of 'M Property.t
      | RestProperty of 'M RestProperty.t
    and 'M t = {
      properties: 'M property list;
      annot: 'M Type.annotation option;
    }
    [@@deriving show]
  end
  module Array : sig
    module RestElement : sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        argument: 'M Pattern.t;
      }
      [@@deriving show]
    end
    type 'M element =
      | Element of 'M Pattern.t
      | RestElement of 'M RestElement.t
    and 'M t = {
      elements: 'M element option list;
      annot: 'M Type.annotation option;
    }
    [@@deriving show]
  end
  module Assignment : sig
    type 'M t = {
      left: 'M Pattern.t;
      right: 'M Expression.t;
    }
    [@@deriving show]
  end
  module Identifier : sig
    type 'M t = {
      name: 'M Identifier.t;
      annot: 'M Type.annotation option;
      optional: bool;
    }
    [@@deriving show]
  end
  type 'M t = 'M * 'M t'
  and 'M t' =
    | Object of 'M Object.t
    | Array of 'M Array.t
    | Assignment of 'M Assignment.t
    | Identifier of 'M Identifier.t
    | Expression of 'M Expression.t
  [@@deriving show]
end = Pattern

and Comment : sig
  type 'M t = 'M * t'
  and t' =
    | Block of string
    | Line of string
  [@@deriving show]
end = Comment

and Class : sig
  module Method : sig
    type 'M t = 'M * 'M t'
    and kind =
      | Constructor
      | Method
      | Get
      | Set
    and 'M t' = {
      kind: kind;
      key: 'M Expression.Object.Property.key;
      value: 'M * 'M Function.t;
      static: bool;
      decorators: 'M Class.Decorator.t list;
    }
    [@@deriving show]
  end
  module Property : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      key: 'M Expression.Object.Property.key;
      value: 'M Expression.t option;
      annot: 'M Type.annotation option;
      static: bool;
      variance: 'M Variance.t option;
    }
    [@@deriving show]
  end
  module PrivateField: sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      key: 'M PrivateName.t;
      value: 'M Expression.t option;
      annot: 'M Type.annotation option;
      static: bool;
      variance: 'M Variance.t option;
    }
    [@@deriving show]
  end
  module Implements : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      id: 'M Identifier.t;
      targs: 'M Type.ParameterInstantiation.t option;
    }
    [@@deriving show]
  end
  module Body : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      body: 'M element list;
    }
    and 'M element =
      | Method of 'M Method.t
      | Property of 'M Property.t
      | PrivateField of 'M PrivateField.t
    [@@deriving show]
  end
  module Decorator : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      expression: 'M Expression.t;
    }
    [@@deriving show]
  end
  type 'M t = {
    id: 'M Identifier.t option;
    body: 'M Class.Body.t;
    tparams: 'M Type.ParameterDeclaration.t option;
    super: 'M Expression.t option;
    super_targs: 'M Type.ParameterInstantiation.t option;
    implements: 'M Class.Implements.t list;
    classDecorators: 'M Decorator.t list;
  }
  [@@deriving show]
end = Class

and Function : sig
  module RestElement : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      argument: 'M Pattern.t;
    }
    [@@deriving show]
  end
  module Params : sig
    type 'M t = 'M * 'M t'
    and 'M t' = {
      params: 'M Pattern.t list;
      rest: 'M RestElement.t option;
    }
    [@@deriving show]
  end

  type 'M t = {
    id: 'M Identifier.t option;
    params: 'M Params.t;
    body: 'M body;
    async: bool;
    generator: bool;
    predicate: 'M Type.Predicate.t option;
    expression: bool;
    return: 'M Type.annotation option;
    tparams: 'M Type.ParameterDeclaration.t option;
  }

  and 'M body =
    | BodyBlock of ('M * 'M Statement.Block.t)
    | BodyExpression of 'M Expression.t

  [@@deriving show]

end = Function

type 'M program = 'M * 'M Statement.t list * 'M Comment.t list [@@deriving show]
