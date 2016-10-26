(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
 * An Ocaml implementation of the SpiderMonkey Parser API
 * https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
 *)

module rec Identifier : sig
  type t = Loc.t * string
end = Identifier

and Literal : sig
  module RegExp : sig
    type t = {
      pattern: string;
      flags: string;
    }
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
end = Literal

and Variance : sig
  type t = Loc.t * t'
  and t' = Plus | Minus
end = Variance

and Type : sig
  module Function : sig
    module Param : sig
      type t = Loc.t * t'
      and t' = {
        name: Identifier.t option;
        typeAnnotation: Type.t;
        optional: bool;
      }
    end
    module RestParam : sig
      type t = Loc.t * t'
      and t' = {
        argument: Param.t
      }
    end
    type t = {
      params: Param.t list * RestParam.t option;
      returnType: Type.t;
      typeParameters: Type.ParameterDeclaration.t option;
    }
  end

  module Object : sig
    module Property : sig
      type t' = {
        key: Expression.Object.Property.key;
        value: Type.t;
        optional: bool;
        static: bool;
        _method: bool;
        variance: Variance.t option;
      }
      type t = Loc.t * t'
    end
    module Indexer: sig
      type t' = {
        id: Identifier.t option;
        key: Type.t;
        value: Type.t;
        static: bool;
        variance: Variance.t option;
      }
      and t = Loc.t * t'
    end
    module CallProperty: sig
      type t = Loc.t * t'
      and t' = {
        value: Loc.t * Function.t;
        static: bool;
      }
    end
    type t = {
      exact: bool;
      properties: Property.t list;
      indexers: Indexer.t list;
      callProperties: CallProperty.t list;
    }
  end

  module Generic : sig
    module Identifier : sig
      type t =
      | Unqualified of Identifier.t
      | Qualified of qualified
      and qualified = Loc.t * qualified'
      and qualified' = {
        qualification: t;
        id: Identifier.t
      }
    end
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterInstantiation.t option;
    }
  end

  module StringLiteral : sig
    type t = {
      value: string;
      raw: string;
    }
  end

  module NumberLiteral : sig
    type t = {
      value: float;
      raw: string;
    }
  end

  module BooleanLiteral : sig
    type t = {
      value: bool;
      raw: string;
    }
  end

  type t = Loc.t * t'
  (* Yes, we could add a little complexity here to show that Any and Void
   * should never be declared nullable, but that check can happen later *)
  and t' =
    | Any
    | Mixed
    | Empty
    | Void
    | Null
    | Number
    | String
    | Boolean
    | Nullable of t
    | Function of Function.t
    | Object of Object.t
    | Array of t
    | Generic of Generic.t
    | Union of t * t * t list
    | Intersection of t * t * t list
    | Typeof of t
    | Tuple of t list
    | StringLiteral of StringLiteral.t
    | NumberLiteral of NumberLiteral.t
    | BooleanLiteral of BooleanLiteral.t
    | Exists

  (* Type.annotation is a concrete syntax node with a location that starts at
   * the colon and ends after the type. For example, "var a: number", the
   * identifier a would have a property typeAnnotation which contains a
   * Type.annotation with a location from column 6-14 *)
  and annotation = Loc.t * t

  module ParameterDeclaration : sig
    module TypeParam : sig
      type t = Loc.t * t'
      and t' = {
        name: string;
        bound: Type.annotation option;
        variance: Variance.t option;
        default: Type.t option;
      }
    end
    type t = Loc.t * t'
    and t' = {
      params: TypeParam.t list;
    }
  end
  module ParameterInstantiation : sig
    type t = Loc.t * t'
    and t' = {
      params: Type.t list;
    }
  end

  module Predicate : sig
    type t = Loc.t * t'
    and t' =
      | Declared of Expression.t
      | Inferred
  end

end = Type

and Statement : sig
  module Block : sig
    type t = {
      body: Statement.t list
    }
  end
  module If : sig
    type t = {
      test: Expression.t;
      consequent: Statement.t;
      alternate: Statement.t option;
    }
  end
  module Labeled : sig
    type t = {
      label: Identifier.t;
      body: Statement.t;
    }
  end
  module Break : sig
    type t = {
      label: Identifier.t option;
    }
  end
  module Continue : sig
    type t = {
      label: Identifier.t option;
    }
  end
  module With : sig
    type t = {
      _object: Expression.t;
      body: Statement.t;
    }
  end
  module TypeAlias : sig
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterDeclaration.t option;
      right: Type.t;
    }
  end
  module Switch : sig
    module Case : sig
      type t = Loc.t * t'
      and t' = {
        test: Expression.t option;
        consequent: Statement.t list;
      }
    end
    type t = {
      discriminant: Expression.t;
      cases: Case.t list;
    }
  end
  module Return : sig
    type t = {
      argument: Expression.t option;
    }
  end
  module Throw : sig
    type t = {
      argument: Expression.t;
    }
  end
  module Try : sig
    module CatchClause : sig
      type t = Loc.t * t'
      and t' = {
        param: Pattern.t;
        body: Loc.t * Block.t;
      }
    end
    type t = {
      block: Loc.t * Block.t;
      handler: CatchClause.t option;
      finalizer: (Loc.t * Block.t) option;
    }
  end
  module VariableDeclaration : sig
    module Declarator : sig
      type t = Loc.t * t'
      and t' = {
        id: Pattern.t;
        init: Expression.t option;
      }
    end
    type kind =
      | Var
      | Let
      | Const
    type t = {
      declarations: Declarator.t list;
      kind: kind;
    }
  end
  module While : sig
    type t = {
      test: Expression.t;
      body: Statement.t;
    }
  end
  module DoWhile : sig
    type t = {
      body: Statement.t;
      test: Expression.t;
    }
  end
  module For : sig
    type init =
      | InitDeclaration of (Loc.t * VariableDeclaration.t)
      | InitExpression of Expression.t
    type t = {
      init: init option;
      test: Expression.t option;
      update: Expression.t option;
      body: Statement.t;
    }
  end
  module ForIn : sig
    type left =
      | LeftDeclaration of (Loc.t * VariableDeclaration.t)
      | LeftExpression of Expression.t
    type t = {
      left: left;
      right: Expression.t;
      body: Statement.t;
      each: bool;
    }
  end
  module ForOf : sig
    type left =
      | LeftDeclaration of (Loc.t * VariableDeclaration.t)
      | LeftExpression of Expression.t
    type t = {
      left: left;
      right: Expression.t;
      body: Statement.t;
      async: bool;
    }
  end
  module Interface : sig
    type t = {
      id: Identifier.t;
      typeParameters: Type.ParameterDeclaration.t option;
      body: Loc.t * Type.Object.t;
      extends: (Loc.t * Type.Generic.t) list;
      mixins: (Loc.t * Type.Generic.t) list;
    }
  end
  module DeclareVariable : sig
    type t = {
      id: Identifier.t;
      typeAnnotation: Type.annotation option;
    }
  end
  module DeclareFunction : sig
    type t = {
      id: Identifier.t;
      typeAnnotation: Type.annotation;
      predicate: Type.Predicate.t option;
    }
  end
  module DeclareModule : sig
    type id =
      | Identifier of Identifier.t
      | Literal of (Loc.t * Literal.t)

    type module_kind =
      | CommonJS of Loc.t
      | ES of Loc.t

    type t = {
      id: id;
      body: Loc.t * Block.t;
      kind: module_kind;
    }
  end
  module ExportNamedDeclaration : sig
    module ExportSpecifier : sig
      type t = Loc.t * t'
      and t' = {
        local: Identifier.t;
        exported: Identifier.t option;
      }
    end
    type specifier =
      | ExportSpecifiers of ExportSpecifier.t list
      | ExportBatchSpecifier of Loc.t * Identifier.t option
    type t = {
      declaration: Statement.t option;
      specifiers: specifier option;
      source: (Loc.t * Literal.t) option; (* This will always be a string *)
      exportKind: Statement.exportKind;
    }
  end
  module ExportDefaultDeclaration : sig
    type declaration =
      | Declaration of Statement.t
      | Expression of Expression.t
    type t = {
      declaration: declaration;
      exportKind: Statement.exportKind;
    }
  end
  module DeclareExportDeclaration : sig
    type declaration =
      (* declare export var *)
      | Variable of (Loc.t * DeclareVariable.t)
      (* declare export function *)
      | Function of (Loc.t * DeclareFunction.t)
      (* declare export class *)
      | Class of (Loc.t * Interface.t)
      (* declare export default [type]
       * this corresponds to things like
       * export default 1+1; *)
      | DefaultType of Type.t
      (* declare export type *)
      | NamedType of (Loc.t * TypeAlias.t)
      (* declare export interface *)
      | Interface of (Loc.t * Interface.t)

    type t = {
      default: bool;
      declaration: declaration option;
      specifiers: ExportNamedDeclaration.specifier option;
      source: (Loc.t * Literal.t) option; (* This will always be a string *)
    }
  end
  module ImportDeclaration : sig
    module NamedSpecifier : sig
      type t = Loc.t * t'
      and t' = {
        id: Identifier.t;
        name: Identifier.t option;
      }
    end
    type named_specifier = {
      local: Identifier.t option;
      remote: Identifier.t;
    }
    type specifier =
      | ImportNamedSpecifier of named_specifier
      | ImportDefaultSpecifier of Identifier.t
      | ImportNamespaceSpecifier of (Loc.t * Identifier.t)
    type importKind =
      | ImportType
      | ImportTypeof
      | ImportValue
    type t = {
      importKind: importKind;
      source: (Loc.t * Literal.t); (* Always a string literal *)
      specifiers: specifier list;
    }
  end
  module Expression : sig
    type t = {
      expression: Expression.t;
    }
  end

  type exportKind =
    | ExportType
    | ExportValue

  type t = Loc.t * t'
  and t' =
    | Empty
    | Block of Block.t
    | Expression of Expression.t
    | If of If.t
    | Labeled of Labeled.t
    | Break of Break.t
    | Continue of Continue.t
    | With of With.t
    | TypeAlias of TypeAlias.t
    | Switch of Switch.t
    | Return of Return.t
    | Throw of Throw.t
    | Try of Try.t
    | While of While.t
    | DoWhile of DoWhile.t
    | For of For.t
    | ForIn of ForIn.t
    | ForOf of ForOf.t
    | Debugger
    | FunctionDeclaration of Function.t
    | VariableDeclaration of VariableDeclaration.t
    | ClassDeclaration of Class.t
    | InterfaceDeclaration of Interface.t
    | DeclareVariable of DeclareVariable.t
    | DeclareFunction of DeclareFunction.t
    | DeclareClass of Interface.t
    | DeclareModule of DeclareModule.t
    | DeclareModuleExports of Type.annotation
    | DeclareExportDeclaration of DeclareExportDeclaration.t
    | ExportNamedDeclaration of ExportNamedDeclaration.t
    | ExportDefaultDeclaration of ExportDefaultDeclaration.t
    | ImportDeclaration of ImportDeclaration.t
end = Statement

and Expression : sig
  module SpreadElement : sig
    type t = Loc.t * t'
    and t' = {
      argument: Expression.t;
    }
  end
  type expression_or_spread =
    | Expression of Expression.t
    | Spread of SpreadElement.t

  module Array : sig
    type t = {
      elements: expression_or_spread option list;
    }
  end
  module TemplateLiteral : sig
    module Element : sig
      type value = {
        raw: string;
        cooked: string;
      }
      type t = Loc.t * t'
      and t' = {
        value: value;
        tail: bool;
      }
    end
    type t = {
      quasis: Element.t list;
      expressions: Expression.t list;
    }
  end
  module TaggedTemplate : sig
    type t = {
      tag: Expression.t;
      quasi: Loc.t * TemplateLiteral.t;
    }
  end
  module Object : sig
    (* This is a slight deviation from the Mozilla spec. In the spec, an object
      * property is not a proper node, and lacks a location and a "type" field.
      * Esprima promotes it to a proper node and that is useful, so I'm
      * following their example *)
    module Property : sig
      type key =
        | Literal of (Loc.t * Literal.t)
        | Identifier of Identifier.t
        | Computed of Expression.t
      type kind =
        | Init
        | Get
        | Set
      type t = Loc.t * t'
      and t' = {
        key: key;
        value: Expression.t;
        kind: kind;
        _method: bool;
        shorthand: bool;
      }
    end
    module SpreadProperty : sig
      type t = Loc.t * t'
      and t' = {
        argument: Expression.t;
      }
    end
    type property =
      | Property of Property.t
      | SpreadProperty of SpreadProperty.t
    type t = {
      properties: property list;
    }
  end
  module Sequence : sig
    type t = {
      expressions: Expression.t list;
    }
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
    type t = {
      operator: operator;
      prefix: bool;
      argument: Expression.t
    }
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
    type t = {
      operator: operator;
      left: Expression.t;
      right: Expression.t;
    }
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
    type t = {
      operator: operator;
      left: Pattern.t;
      right: Expression.t;
    }
  end
  module Update : sig
    type operator =
      | Increment
      | Decrement
    type t = {
      operator: operator;
      argument: Expression.t;
      prefix: bool;
    }
  end
  module Logical : sig
    type operator =
      | Or
      | And
    type t = {
      operator: operator;
      left: Expression.t;
      right: Expression.t;
    }
  end
  module Conditional : sig
    type t = {
      test: Expression.t;
      consequent: Expression.t;
      alternate: Expression.t;
    }
  end
  module New : sig
    type t = {
      callee: Expression.t;
      arguments: expression_or_spread list;
    }
  end
  module Call : sig
    type t = {
      callee: Expression.t;
      arguments: expression_or_spread list;
    }
  end
  module Member : sig
    type property =
      | PropertyIdentifier of Identifier.t
      | PropertyExpression of Expression.t
    type t = {
      _object: Expression.t;
      property: property;
      computed: bool;
    }
  end
  module Yield : sig
    type t = {
      argument: Expression.t option;
      delegate: bool;
    }
  end
  module Comprehension : sig
    module Block : sig
      type t = Loc.t * t'
      and t' = {
        left: Pattern.t;
        right: Expression.t;
        each: bool;
      }
    end
    type t = {
      blocks: Block.t list;
      filter: Expression.t option;
    }
  end
  module Generator : sig
    type t = {
      blocks: Comprehension.Block.t list;
      filter: Expression.t option;
    }
  end
  module TypeCast : sig
    type t = {
      expression: Expression.t;
      typeAnnotation: Type.annotation;
    }
  end
  module MetaProperty : sig
    type t = {
      meta: Identifier.t;
      property: Identifier.t;
    }
  end

  type t = Loc.t * t'
  and t' =
    | This
    | Super
    | Array of Array.t
    | Object of Object.t
    | Function of Function.t
    | ArrowFunction of Function.t
    | Sequence of Sequence.t
    | Unary of Unary.t
    | Binary of Binary.t
    | Assignment of Assignment.t
    | Update of Update.t
    | Logical of Logical.t
    | Conditional of Conditional.t
    | New of New.t
    | Call of Call.t
    | Member of Member.t
    | Yield of Yield.t
    | Comprehension of Comprehension.t
    | Generator of Generator.t
    | Identifier of Identifier.t
    | Literal of Literal.t
    | TemplateLiteral of TemplateLiteral.t
    | TaggedTemplate of TaggedTemplate.t
    | JSXElement of JSX.element
    | Class of Class.t
    | TypeCast of TypeCast.t
    | MetaProperty of MetaProperty.t
end = Expression

and JSX : sig
  module Identifier : sig
    type t = Loc.t * t'
    and t' = {
      name: string;
    }
  end

  module NamespacedName : sig
    type t = Loc.t * t'
    and t' = {
      namespace: Identifier.t;
      name: Identifier.t;
    }
  end

  module ExpressionContainer : sig
    type t = {
      expression: expression;
    }
    and expression =
    | Expression of Expression.t
    | EmptyExpression of Loc.t
  end

  module Text : sig
    type t = {
      value: string;
      raw: string;
    }
  end

  module Attribute : sig
    type t = Loc.t * t'
    and name =
    | Identifier of Identifier.t
    | NamespacedName of NamespacedName.t
    and value =
    | Literal of Loc.t * Literal.t
    | ExpressionContainer of Loc.t * ExpressionContainer.t
    and t' = {
      name: name;
      value: value option;
    }
  end

  module SpreadAttribute : sig
    type t = Loc.t * t'
    and t' = {
      argument: Expression.t;
    }
  end

  module MemberExpression : sig
    type t = Loc.t * t'
    and _object =
    | Identifier of Identifier.t
    | MemberExpression of t
    and t' = {
      _object: _object;
      property: Identifier.t;
    }
  end

  type name =
    | Identifier of Identifier.t
    | NamespacedName of NamespacedName.t
    | MemberExpression of MemberExpression.t

  module Opening : sig
    type t = Loc.t * t'

    and attribute =
      | Attribute of Attribute.t
      | SpreadAttribute of SpreadAttribute.t

    and t' = {
      name: name;
      selfClosing: bool;
      attributes: attribute list;
    }
  end

  module Closing : sig
    type t = Loc.t * t'
    and t' = {
      name: name;
    }
  end

  type child = Loc.t * child'
  and child' =
    | Element of element
    | ExpressionContainer of ExpressionContainer.t
    | Text of Text.t

  and element = {
    openingElement: Opening.t;
    closingElement: Closing.t option;
    children: child list
  }
end = JSX

and Pattern : sig
  module Object : sig
    module Property : sig
      type key =
        | Literal of (Loc.t * Literal.t)
        | Identifier of Identifier.t
        | Computed of Expression.t
      type t = Loc.t * t'
      and t' = {
        key: key;
        pattern: Pattern.t;
        shorthand: bool;
      }
    end
    module RestProperty : sig
      type t = Loc.t * t'
      and t' = {
        argument: Pattern.t;
      }
    end
    type property =
      | Property of Property.t
      | RestProperty of RestProperty.t
    type t = {
      properties: property list;
      typeAnnotation: Type.annotation option;
    }
  end
  module Array : sig
    module RestElement : sig
      type t = Loc.t * t'
      and t' = {
        argument: Pattern.t;
      }
    end
    type element =
      | Element of Pattern.t
      | RestElement of RestElement.t
    type t = {
      elements: element option list;
      typeAnnotation: Type.annotation option;
    }
  end
  module Assignment : sig
    type t = {
      left: Pattern.t;
      right: Expression.t;
    }
  end
  module Identifier : sig
    type t = {
      name: Identifier.t;
      typeAnnotation: Type.annotation option;
      optional: bool;
    }
  end
  type t = Loc.t * t'
  and t' =
    | Object of Object.t
    | Array of Array.t
    | Assignment of Assignment.t
    | Identifier of Identifier.t
    | Expression of Expression.t
end = Pattern

and Comment : sig
  type t = Loc.t * t'
  and t' =
    | Block of string
    | Line of string
end = Comment

and Class : sig
  module Method : sig
    type t = Loc.t * t'
    and kind =
      | Constructor
      | Method
      | Get
      | Set
    and t' = {
      kind: kind;
      key: Expression.Object.Property.key;
      value: Loc.t * Function.t;
      static: bool;
      decorators: Expression.t list;
    }
  end
  module Property : sig
    type t = Loc.t * t'
    and t' = {
      key: Expression.Object.Property.key;
      value: Expression.t option;
      typeAnnotation: Type.annotation option;
      static: bool;
      variance: Variance.t option;
    }
  end
  module Implements : sig
    type t = Loc.t * t'
    and t' = {
      id: Identifier.t;
      typeParameters: Type.ParameterInstantiation.t option;
    }
  end
  module Body : sig
    type element =
      | Method of Method.t
      | Property of Property.t
    type t = Loc.t * t'
    and t' = {
      body: element list;
    }
  end
  type t = {
    id: Identifier.t option;
    body: Class.Body.t;
    superClass: Expression.t option;
    typeParameters: Type.ParameterDeclaration.t option;
    superTypeParameters: Type.ParameterInstantiation.t option;
    implements: Class.Implements.t list;
    classDecorators: Expression.t list;
  }
end = Class

and Function : sig
  module RestElement : sig
    type t = Loc.t * t'
    and t' = {
      argument: Pattern.t;
    }
  end
  type body =
    | BodyBlock of (Loc.t * Statement.Block.t)
    | BodyExpression of Expression.t
  type t = {
    id: Identifier.t option;
    params: Pattern.t list * RestElement.t option;
    body: body;
    async: bool;
    generator: bool;
    predicate: Type.Predicate.t option;
    expression: bool;
    returnType: Type.annotation option;
    typeParameters: Type.ParameterDeclaration.t option;
  }
end = Function

type program = Loc.t * Statement.t list * Comment.t list
