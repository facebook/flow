(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[%%gen
(*
 * An Ocaml implementation of the SpiderMonkey Parser API
 * https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API
 *)

module rec Syntax : sig
  type ('M, 'internal) t = {
    leading: 'M Comment.t list;
    trailing: 'M Comment.t list;
    internal: 'internal;
  }
  [@@deriving show]
end =
  Syntax

and Identifier : sig
  type ('M, 'T) t = 'T * 'M t'

  and 'M t' = {
    name: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  Identifier

and PrivateName : sig
  type 'M t = 'M * 'M t'

  and 'M t' = {
    id: ('M, 'M) Identifier.t;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  PrivateName

and Literal : sig
  module RegExp : sig
    type t = {
      pattern: string;
      flags: string;
    }
    [@@deriving show]
  end

  (* Literals also carry along their raw value *)
  type 'M t = {
    value: value;
    raw: string;
    comments: ('M, unit) Syntax.t option;
  }

  and value =
    | String of string
    | Boolean of bool
    | Null
    | Number of float
    | BigInt of float
    | RegExp of RegExp.t
  [@@deriving show]
end =
  Literal

and StringLiteral : sig
  type 'M t = {
    value: string;
    raw: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  StringLiteral

and NumberLiteral : sig
  type 'M t = {
    value: float;
    raw: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  NumberLiteral

and BigIntLiteral : sig
  type 'M t = {
    approx_value: float;
    (* Warning! Might lose precision! *)
    bigint: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  BigIntLiteral

and BooleanLiteral : sig
  type 'M t = {
    value: bool;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  BooleanLiteral

and Variance : sig
  type 'M t = 'M * 'M t'

  and kind =
    | Plus
    | Minus

  and 'M t' = {
    kind: kind;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  Variance

and ComputedKey : sig
  type ('M, 'T) t = 'M * ('M, 'T) ComputedKey.t'

  and ('M, 'T) t' = {
    expression: ('M, 'T) Expression.t;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  ComputedKey

and Type : sig
  module Function : sig
    module Param : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        name: ('M, 'T) Identifier.t option;
        annot: ('M, 'T) Type.t;
        optional: bool;
      }
      [@@deriving show]
    end

    module RestParam : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        argument: ('M, 'T) Param.t;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    module Params : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        params: ('M, 'T) Param.t list;
        rest: ('M, 'T) RestParam.t option;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      tparams: ('M, 'T) Type.TypeParams.t option;
      params: ('M, 'T) Params.t;
      return: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Generic : sig
    module Identifier : sig
      type ('M, 'T) t =
        | Unqualified of ('M, 'T) Identifier.t
        | Qualified of ('M, 'T) qualified

      and ('M, 'T) qualified = 'M * ('M, 'T) qualified'

      and ('M, 'T) qualified' = {
        qualification: ('M, 'T) t;
        id: ('M, 'T) Identifier.t;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      targs: ('M, 'T) Type.TypeArgs.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Object : sig
    module Property : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        key: ('M, 'T) Expression.Object.Property.key;
        value: ('M, 'T) value;
        optional: bool;
        static: bool;
        proto: bool;
        _method: bool;
        variance: 'M Variance.t option;
      }

      and ('M, 'T) value =
        | Init of ('M, 'T) Type.t
        | Get of ('M * ('M, 'T) Function.t)
        | Set of ('M * ('M, 'T) Function.t)
      [@@deriving show]
    end

    module SpreadProperty : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        argument: ('M, 'T) Type.t;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    module Indexer : sig
      type ('M, 'T) t' = {
        id: ('M, 'M) Identifier.t option;
        key: ('M, 'T) Type.t;
        value: ('M, 'T) Type.t;
        static: bool;
        variance: 'M Variance.t option;
        comments: ('M, unit) Syntax.t option;
      }

      and ('M, 'T) t = 'M * ('M, 'T) t' [@@deriving show]
    end

    module CallProperty : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        value: 'M * ('M, 'T) Function.t;
        static: bool;
      }
      [@@deriving show]
    end

    module InternalSlot : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        id: ('M, 'M) Identifier.t;
        value: ('M, 'T) Type.t;
        optional: bool;
        static: bool;
        _method: bool;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      exact: bool;
      (* Inexact indicates the presence of ... in the object. It is more
       * easily understood if exact is read as "explicitly exact" and "inexact"
       * is read as "explicitly inexact".
       *
       * This confusion will go away when we get rid of the exact flag in favor
       * of inexact as part of the work to make object types exact by default.
       * *)
      inexact: bool;
      properties: ('M, 'T) property list;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) property =
      | Property of ('M, 'T) Property.t
      | SpreadProperty of ('M, 'T) SpreadProperty.t
      | Indexer of ('M, 'T) Indexer.t
      | CallProperty of ('M, 'T) CallProperty.t
      | InternalSlot of ('M, 'T) InternalSlot.t
    [@@deriving show]
  end

  module Interface : sig
    type ('M, 'T) t = {
      body: 'M * ('M, 'T) Object.t;
      extends: ('M * ('M, 'T) Generic.t) list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Nullable : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Typeof : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Type.t;
      (* TODO T64309494 *)
      internal: bool;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Tuple : sig
    type ('M, 'T) t = {
      types: ('M, 'T) Type.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Array : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Union : sig
    type ('M, 'T) t = {
      types: ('M, 'T) Type.t * ('M, 'T) Type.t * ('M, 'T) Type.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Intersection : sig
    type ('M, 'T) t = {
      types: ('M, 'T) Type.t * ('M, 'T) Type.t * ('M, 'T) Type.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = 'T * ('M, 'T) t'

  (* Yes, we could add a little complexity here to show that Any and Void
   * should never be declared nullable, but that check can happen later *)
  and ('M, 'T) t' =
    | Any of ('M, unit) Syntax.t option
    | Mixed of ('M, unit) Syntax.t option
    | Empty of ('M, unit) Syntax.t option
    | Void of ('M, unit) Syntax.t option
    | Null of ('M, unit) Syntax.t option
    | Number of ('M, unit) Syntax.t option
    | BigInt of ('M, unit) Syntax.t option
    | String of ('M, unit) Syntax.t option
    | Boolean of ('M, unit) Syntax.t option
    | Symbol of ('M, unit) Syntax.t option
    | Exists of ('M, unit) Syntax.t option
    | Nullable of ('M, 'T) Nullable.t
    | Function of ('M, 'T) Function.t
    | Object of ('M, 'T) Object.t
    | Interface of ('M, 'T) Interface.t
    | Array of ('M, 'T) Array.t
    | Generic of ('M, 'T) Generic.t
    | Union of ('M, 'T) Union.t
    | Intersection of ('M, 'T) Intersection.t
    | Typeof of ('M, 'T) Typeof.t
    | Tuple of ('M, 'T) Tuple.t
    | StringLiteral of 'M StringLiteral.t
    | NumberLiteral of 'M NumberLiteral.t
    | BigIntLiteral of 'M BigIntLiteral.t
    | BooleanLiteral of 'M BooleanLiteral.t

  (* Type.annotation is a concrete syntax node with a location that starts at
   * the colon and ends after the type. For example, "var a: number", the
   * identifier a would have a property annot which contains a
   * Type.annotation with a location from column 6-14 *)
  and ('M, 'T) annotation = 'M * ('M, 'T) t

  and ('M, 'T) annotation_or_hint =
    | Missing of 'T
    | Available of ('M, 'T) Type.annotation
  [@@deriving show]

  module TypeParam : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      name: ('M, 'M) Identifier.t;
      bound: ('M, 'T) Type.annotation_or_hint;
      variance: 'M Variance.t option;
      default: ('M, 'T) Type.t option;
    }
    [@@deriving show]
  end

  module TypeParams : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      params: ('M, 'T) TypeParam.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module TypeArgs : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      arguments: ('M, 'T) Type.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Predicate : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      kind: ('M, 'T) kind;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) kind =
      | Declared of ('M, 'T) Expression.t
      | Inferred
    [@@deriving show]
  end
end =
  Type

and Statement : sig
  module Block : sig
    type ('M, 'T) t = {
      body: ('M, 'T) Statement.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module If : sig
    type ('M, 'T) t = {
      test: ('M, 'T) Expression.t;
      consequent: ('M, 'T) Statement.t;
      alternate: ('M, 'T) Statement.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Labeled : sig
    type ('M, 'T) t = {
      label: ('M, 'M) Identifier.t;
      body: ('M, 'T) Statement.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Break : sig
    type 'M t = {
      label: ('M, 'M) Identifier.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Continue : sig
    type 'M t = {
      label: ('M, 'M) Identifier.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Debugger : sig
    type 'M t = { comments: ('M, unit) Syntax.t option } [@@deriving show]
  end

  module With : sig
    type ('M, 'T) t = {
      _object: ('M, 'T) Expression.t;
      body: ('M, 'T) Statement.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module TypeAlias : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      right: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module OpaqueType : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      impltype: ('M, 'T) Type.t option;
      supertype: ('M, 'T) Type.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Switch : sig
    module Case : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        test: ('M, 'T) Expression.t option;
        consequent: ('M, 'T) Statement.t list;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      discriminant: ('M, 'T) Expression.t;
      cases: ('M, 'T) Case.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Return : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Throw : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Try : sig
    module CatchClause : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        param: ('M, 'T) Pattern.t option;
        body: 'M * ('M, 'T) Block.t;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      block: 'M * ('M, 'T) Block.t;
      handler: ('M, 'T) CatchClause.t option;
      finalizer: ('M * ('M, 'T) Block.t) option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module VariableDeclaration : sig
    module Declarator : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        id: ('M, 'T) Pattern.t;
        init: ('M, 'T) Expression.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      declarations: ('M, 'T) Declarator.t list;
      kind: kind;
      comments: ('M, unit) Syntax.t option;
    }

    and kind =
      | Var
      | Let
      | Const
    [@@deriving show]
  end

  module While : sig
    type ('M, 'T) t = {
      test: ('M, 'T) Expression.t;
      body: ('M, 'T) Statement.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DoWhile : sig
    type ('M, 'T) t = {
      body: ('M, 'T) Statement.t;
      test: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module For : sig
    type ('M, 'T) t = {
      init: ('M, 'T) init option;
      test: ('M, 'T) Expression.t option;
      update: ('M, 'T) Expression.t option;
      body: ('M, 'T) Statement.t;
    }

    and ('M, 'T) init =
      | InitDeclaration of ('M * ('M, 'T) VariableDeclaration.t)
      | InitExpression of ('M, 'T) Expression.t
    [@@deriving show]
  end

  module ForIn : sig
    type ('M, 'T) t = {
      left: ('M, 'T) left;
      right: ('M, 'T) Expression.t;
      body: ('M, 'T) Statement.t;
      each: bool;
    }

    and ('M, 'T) left =
      | LeftDeclaration of ('M * ('M, 'T) VariableDeclaration.t)
      | LeftPattern of ('M, 'T) Pattern.t
    [@@deriving show]
  end

  module ForOf : sig
    type ('M, 'T) t = {
      left: ('M, 'T) left;
      right: ('M, 'T) Expression.t;
      body: ('M, 'T) Statement.t;
      await: bool;
    }

    and ('M, 'T) left =
      | LeftDeclaration of ('M * ('M, 'T) VariableDeclaration.t)
      | LeftPattern of ('M, 'T) Pattern.t
    [@@deriving show]
  end

  module EnumDeclaration : sig
    module DefaultedMember : sig
      type 'M t = 'M * 'M t'

      and 'M t' = { id: ('M, 'M) Identifier.t } [@@deriving show]
    end

    module InitializedMember : sig
      type ('I, 'M) t = 'M * ('I, 'M) t'

      and ('I, 'M) t' = {
        id: ('M, 'M) Identifier.t;
        init: 'M * 'I;
      }
      [@@deriving show]
    end

    module BooleanBody : sig
      type 'M t = {
        members: ('M BooleanLiteral.t, 'M) InitializedMember.t list;
        explicitType: bool;
      }
      [@@deriving show]
    end

    module NumberBody : sig
      type 'M t = {
        members: ('M NumberLiteral.t, 'M) InitializedMember.t list;
        explicitType: bool;
      }
      [@@deriving show]
    end

    module StringBody : sig
      type 'M t = {
        members: ('M StringLiteral.t, 'M) members;
        explicitType: bool;
      }

      and ('I, 'M) members =
        | Defaulted of 'M DefaultedMember.t list
        | Initialized of ('I, 'M) InitializedMember.t list
      [@@deriving show]
    end

    module SymbolBody : sig
      type 'M t = { members: 'M DefaultedMember.t list } [@@deriving show]
    end

    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      body: 'M body;
    }

    and 'M body = 'M * 'M body'

    and 'M body' =
      | BooleanBody of 'M BooleanBody.t
      | NumberBody of 'M NumberBody.t
      | StringBody of 'M StringBody.t
      | SymbolBody of 'M SymbolBody.t
    [@@deriving show]
  end

  module Interface : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      extends: ('M * ('M, 'T) Type.Generic.t) list;
      body: 'M * ('M, 'T) Type.Object.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareClass : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      body: 'M * ('M, 'T) Type.Object.t;
      extends: ('M * ('M, 'T) Type.Generic.t) option;
      mixins: ('M * ('M, 'T) Type.Generic.t) list;
      implements: ('M, 'T) Class.Implements.t option;
    }
    [@@deriving show]
  end

  module DeclareVariable : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      annot: ('M, 'T) Type.annotation_or_hint;
    }
    [@@deriving show]
  end

  module DeclareFunction : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      annot: ('M, 'T) Type.annotation;
      predicate: ('M, 'T) Type.Predicate.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareModule : sig
    type ('M, 'T) id =
      | Identifier of ('M, 'T) Identifier.t
      | Literal of ('T * 'M StringLiteral.t)

    and 'M module_kind =
      | CommonJS of 'M
      | ES of 'M

    and ('M, 'T) t = {
      id: ('M, 'T) id;
      body: 'M * ('M, 'T) Block.t;
      kind: 'M module_kind;
    }
    [@@deriving show]
  end

  module ExportNamedDeclaration : sig
    module ExportSpecifier : sig
      type 'M t = 'M * 'M t'

      and 'M t' = {
        local: ('M, 'M) Identifier.t;
        exported: ('M, 'M) Identifier.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      declaration: ('M, 'T) Statement.t option;
      specifiers: 'M specifier option;
      source: ('M * 'M StringLiteral.t) option;
      exportKind: Statement.exportKind;
      comments: ('M, unit) Syntax.t option;
    }

    and 'M specifier =
      | ExportSpecifiers of 'M ExportSpecifier.t list
      | ExportBatchSpecifier of 'M * ('M, 'M) Identifier.t option
    [@@deriving show]
  end

  module ExportDefaultDeclaration : sig
    type ('M, 'T) t = {
      default: 'M;
      declaration: ('M, 'T) declaration;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) declaration =
      | Declaration of ('M, 'T) Statement.t
      | Expression of ('M, 'T) Expression.t
    [@@deriving show]
  end

  module DeclareExportDeclaration : sig
    type ('M, 'T) declaration =
      (* declare export var *)
      | Variable of ('M * ('M, 'T) DeclareVariable.t)
      (* declare export function *)
      | Function of ('M * ('M, 'T) DeclareFunction.t)
      (* declare export class *)
      | Class of ('M * ('M, 'T) DeclareClass.t)
      (* declare export default [type]
       * this corresponds to things like
       * export default 1+1; *)
      | DefaultType of ('M, 'T) Type.t
      (* declare export type *)
      | NamedType of ('M * ('M, 'T) TypeAlias.t)
      (* declare export opaque type *)
      | NamedOpaqueType of ('M * ('M, 'T) OpaqueType.t)
      (* declare export interface *)
      | Interface of ('M * ('M, 'T) Interface.t)

    and ('M, 'T) t = {
      default: 'M option;
      declaration: ('M, 'T) declaration option;
      specifiers: 'M ExportNamedDeclaration.specifier option;
      source: ('M * 'M StringLiteral.t) option;
    }
    [@@deriving show]
  end

  module ImportDeclaration : sig
    type importKind =
      | ImportType
      | ImportTypeof
      | ImportValue

    and ('M, 'T) specifier =
      | ImportNamedSpecifiers of ('M, 'T) named_specifier list
      | ImportNamespaceSpecifier of ('M * ('M, 'T) Identifier.t)

    and ('M, 'T) named_specifier = {
      kind: importKind option;
      local: ('M, 'T) Identifier.t option;
      remote: ('M, 'T) Identifier.t;
    }

    and ('M, 'T) t = {
      importKind: importKind;
      source: 'M * 'M StringLiteral.t;
      default: ('M, 'T) Identifier.t option;
      specifiers: ('M, 'T) specifier option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Expression : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) Expression.t;
      directive: string option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Empty : sig
    type 'M t = { comments: ('M, unit) Syntax.t option } [@@deriving show]
  end

  type exportKind =
    | ExportType
    | ExportValue

  and ('M, 'T) t = 'M * ('M, 'T) t'

  and ('M, 'T) t' =
    | Block of ('M, 'T) Block.t
    | Break of 'M Break.t
    | ClassDeclaration of ('M, 'T) Class.t
    | Continue of 'M Continue.t
    | Debugger of 'M Debugger.t
    | DeclareClass of ('M, 'T) DeclareClass.t
    | DeclareExportDeclaration of ('M, 'T) DeclareExportDeclaration.t
    | DeclareFunction of ('M, 'T) DeclareFunction.t
    | DeclareInterface of ('M, 'T) Interface.t
    | DeclareModule of ('M, 'T) DeclareModule.t
    | DeclareModuleExports of ('M, 'T) Type.annotation
    | DeclareTypeAlias of ('M, 'T) TypeAlias.t
    | DeclareOpaqueType of ('M, 'T) OpaqueType.t
    | DeclareVariable of ('M, 'T) DeclareVariable.t
    | DoWhile of ('M, 'T) DoWhile.t
    | Empty of 'M Empty.t
    | EnumDeclaration of ('M, 'T) EnumDeclaration.t
    | ExportDefaultDeclaration of ('M, 'T) ExportDefaultDeclaration.t
    | ExportNamedDeclaration of ('M, 'T) ExportNamedDeclaration.t
    | Expression of ('M, 'T) Expression.t
    | For of ('M, 'T) For.t
    | ForIn of ('M, 'T) ForIn.t
    | ForOf of ('M, 'T) ForOf.t
    | FunctionDeclaration of ('M, 'T) Function.t
    | If of ('M, 'T) If.t
    | ImportDeclaration of ('M, 'T) ImportDeclaration.t
    | InterfaceDeclaration of ('M, 'T) Interface.t
    | Labeled of ('M, 'T) Labeled.t
    | Return of ('M, 'T) Return.t
    | Switch of ('M, 'T) Switch.t
    | Throw of ('M, 'T) Throw.t
    | Try of ('M, 'T) Try.t
    | TypeAlias of ('M, 'T) TypeAlias.t
    | OpaqueType of ('M, 'T) OpaqueType.t
    | VariableDeclaration of ('M, 'T) VariableDeclaration.t
    | While of ('M, 'T) While.t
    | With of ('M, 'T) With.t
  [@@deriving show]
end =
  Statement

and Expression : sig
  module CallTypeArg : sig
    module Implicit : sig
      type ('M, 'T) t = 'T * 'M t'

      and 'M t' = { comments: ('M, unit) Syntax.t option } [@@deriving show]
    end

    type ('M, 'T) t =
      | Explicit of ('M, 'T) Type.t
      | Implicit of ('M, 'T) Implicit.t
    [@@deriving show]
  end

  module CallTypeArgs : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = ('M, 'T) CallTypeArg.t list [@@deriving show]
  end

  module SpreadElement : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) expression_or_spread =
    | Expression of ('M, 'T) Expression.t
    | Spread of ('M, 'T) SpreadElement.t
  [@@deriving show]

  module Array : sig
    type ('M, 'T) t = {
      elements: ('M, 'T) expression_or_spread option list;
      comments: ('M, unit) Syntax.t option;
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

    type ('M, 'T) t = {
      quasis: 'M Element.t list;
      expressions: ('M, 'T) Expression.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module TaggedTemplate : sig
    type ('M, 'T) t = {
      tag: ('M, 'T) Expression.t;
      quasi: 'M * ('M, 'T) TemplateLiteral.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Object : sig
    module Property : sig
      type ('M, 'T) key =
        | Literal of ('T * 'M Literal.t)
        | Identifier of ('M, 'T) Identifier.t
        | PrivateName of 'M PrivateName.t
        | Computed of ('M, 'T) ComputedKey.t

      and ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' =
        | Init of {
            key: ('M, 'T) key;
            value: ('M, 'T) Expression.t;
            shorthand: bool;
          }
        | Method of {
            key: ('M, 'T) key;
            value: 'M * ('M, 'T) Function.t;
          }
        | Get of {
            key: ('M, 'T) key;
            value: 'M * ('M, 'T) Function.t;
            comments: ('M, unit) Syntax.t option;
          }
        | Set of {
            key: ('M, 'T) key;
            value: 'M * ('M, 'T) Function.t;
            comments: ('M, unit) Syntax.t option;
          }
      [@@deriving show]
    end

    module SpreadProperty : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        argument: ('M, 'T) Expression.t;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) property =
      | Property of ('M, 'T) Property.t
      | SpreadProperty of ('M, 'T) SpreadProperty.t

    and ('M, 'T) t = {
      properties: ('M, 'T) property list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Sequence : sig
    type ('M, 'T) t = {
      expressions: ('M, 'T) Expression.t list;
      comments: ('M, unit) Syntax.t option;
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

    and ('M, 'T) t = {
      operator: operator;
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
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

    and ('M, 'T) t = {
      operator: operator;
      left: ('M, 'T) Expression.t;
      right: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Assignment : sig
    type operator =
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

    and ('M, 'T) t = {
      operator: operator option;
      left: ('M, 'T) Pattern.t;
      right: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Update : sig
    type operator =
      | Increment
      | Decrement

    and ('M, 'T) t = {
      operator: operator;
      argument: ('M, 'T) Expression.t;
      prefix: bool;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Logical : sig
    type operator =
      | Or
      | And
      | NullishCoalesce

    and ('M, 'T) t = {
      operator: operator;
      left: ('M, 'T) Expression.t;
      right: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Conditional : sig
    type ('M, 'T) t = {
      test: ('M, 'T) Expression.t;
      consequent: ('M, 'T) Expression.t;
      alternate: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module ArgList : sig
    type ('M, 'T) t = 'M * ('M, 'T) expression_or_spread list [@@deriving show]
  end

  module New : sig
    type ('M, 'T) t = {
      callee: ('M, 'T) Expression.t;
      targs: ('M, 'T) Expression.CallTypeArgs.t option;
      arguments: ('M, 'T) ArgList.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Call : sig
    type ('M, 'T) t = {
      callee: ('M, 'T) Expression.t;
      targs: ('M, 'T) Expression.CallTypeArgs.t option;
      arguments: ('M, 'T) ArgList.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module OptionalCall : sig
    type ('M, 'T) t = {
      call: ('M, 'T) Call.t;
      optional: bool;
    }
    [@@deriving show]
  end

  module Member : sig
    type ('M, 'T) property =
      | PropertyIdentifier of ('M, 'T) Identifier.t
      | PropertyPrivateName of 'M PrivateName.t
      | PropertyExpression of ('M, 'T) Expression.t

    and ('M, 'T) t = {
      _object: ('M, 'T) Expression.t;
      property: ('M, 'T) property;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module OptionalMember : sig
    type ('M, 'T) t = {
      member: ('M, 'T) Member.t;
      optional: bool;
    }
    [@@deriving show]
  end

  module Yield : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t option;
      comments: ('M, unit) Syntax.t option;
      delegate: bool;
    }
    [@@deriving show]
  end

  module Comprehension : sig
    module Block : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        left: ('M, 'T) Pattern.t;
        right: ('M, 'T) Expression.t;
        each: bool;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      blocks: ('M, 'T) Block.t list;
      filter: ('M, 'T) Expression.t option;
    }
    [@@deriving show]
  end

  module Generator : sig
    type ('M, 'T) t = {
      blocks: ('M, 'T) Comprehension.Block.t list;
      filter: ('M, 'T) Expression.t option;
    }
    [@@deriving show]
  end

  module TypeCast : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) Expression.t;
      annot: ('M, 'T) Type.annotation;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module MetaProperty : sig
    type 'M t = {
      meta: ('M, 'M) Identifier.t;
      property: ('M, 'M) Identifier.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module This : sig
    type 'M t = { comments: ('M, unit) Syntax.t option } [@@deriving show]
  end

  module Super : sig
    type 'M t = { comments: ('M, unit) Syntax.t option } [@@deriving show]
  end

  module Import : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = 'T * ('M, 'T) t'

  and ('M, 'T) t' =
    | Array of ('M, 'T) Array.t
    | ArrowFunction of ('M, 'T) Function.t
    | Assignment of ('M, 'T) Assignment.t
    | Binary of ('M, 'T) Binary.t
    | Call of ('M, 'T) Call.t
    | Class of ('M, 'T) Class.t
    | Comprehension of ('M, 'T) Comprehension.t
    | Conditional of ('M, 'T) Conditional.t
    | Function of ('M, 'T) Function.t
    | Generator of ('M, 'T) Generator.t
    | Identifier of ('M, 'T) Identifier.t
    | Import of ('M, 'T) Import.t
    | JSXElement of ('M, 'T) JSX.element
    | JSXFragment of ('M, 'T) JSX.fragment
    | Literal of 'M Literal.t
    | Logical of ('M, 'T) Logical.t
    | Member of ('M, 'T) Member.t
    | MetaProperty of 'M MetaProperty.t
    | New of ('M, 'T) New.t
    | Object of ('M, 'T) Object.t
    | OptionalCall of ('M, 'T) OptionalCall.t
    | OptionalMember of ('M, 'T) OptionalMember.t
    | Sequence of ('M, 'T) Sequence.t
    | Super of 'M Super.t
    | TaggedTemplate of ('M, 'T) TaggedTemplate.t
    | TemplateLiteral of ('M, 'T) TemplateLiteral.t
    | This of 'M This.t
    | TypeCast of ('M, 'T) TypeCast.t
    | Unary of ('M, 'T) Unary.t
    | Update of ('M, 'T) Update.t
    | Yield of ('M, 'T) Yield.t
  [@@deriving show]
end =
  Expression

and JSX : sig
  module Identifier : sig
    type ('M, 'T) t = 'T * 'M t'

    and 'M t' = {
      name: string;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module NamespacedName : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      namespace: ('M, 'T) Identifier.t;
      name: ('M, 'T) Identifier.t;
    }
    [@@deriving show]
  end

  module ExpressionContainer : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) expression;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) expression =
      | Expression of ('M, 'T) Expression.t
      | EmptyExpression
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
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) name =
      | Identifier of ('M, 'T) Identifier.t
      | NamespacedName of ('M, 'T) NamespacedName.t

    and ('M, 'T) value =
      | Literal of 'T * 'M Literal.t
      | ExpressionContainer of 'T * ('M, 'T) ExpressionContainer.t

    and ('M, 'T) t' = {
      name: ('M, 'T) name;
      value: ('M, 'T) value option;
    }
    [@@deriving show]
  end

  module SpreadAttribute : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module MemberExpression : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) _object =
      | Identifier of ('M, 'T) Identifier.t
      | MemberExpression of ('M, 'T) t

    and ('M, 'T) t' = {
      _object: ('M, 'T) _object;
      property: ('M, 'T) Identifier.t;
    }
    [@@deriving show]
  end

  type ('M, 'T) name =
    | Identifier of ('M, 'T) Identifier.t
    | NamespacedName of ('M, 'T) NamespacedName.t
    | MemberExpression of ('M, 'T) MemberExpression.t
  [@@deriving show]

  module Opening : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) attribute =
      | Attribute of ('M, 'T) Attribute.t
      | SpreadAttribute of ('M, 'T) SpreadAttribute.t

    and ('M, 'T) t' = {
      name: ('M, 'T) name;
      selfClosing: bool;
      attributes: ('M, 'T) attribute list;
    }
    [@@deriving show]
  end

  module Closing : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = { name: ('M, 'T) name } [@@deriving show]
  end

  module SpreadChild : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) child = 'M * ('M, 'T) child'

  and ('M, 'T) child' =
    | Element of ('M, 'T) element
    | Fragment of ('M, 'T) fragment
    | ExpressionContainer of ('M, 'T) ExpressionContainer.t
    | SpreadChild of ('M, 'T) SpreadChild.t
    | Text of Text.t

  and ('M, 'T) element = {
    openingElement: ('M, 'T) Opening.t;
    closingElement: ('M, 'T) Closing.t option;
    children: 'M * ('M, 'T) child list;
    comments: ('M, unit) Syntax.t option;
  }

  and ('M, 'T) fragment = {
    frag_openingElement: 'M;
    frag_closingElement: 'M;
    frag_children: 'M * ('M, 'T) child list;
    frag_comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  JSX

and Pattern : sig
  module RestElement : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Pattern.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Object : sig
    module Property : sig
      type ('M, 'T) key =
        | Literal of ('M * 'M Literal.t)
        | Identifier of ('M, 'T) Identifier.t
        | Computed of ('M, 'T) ComputedKey.t

      and ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        key: ('M, 'T) key;
        pattern: ('M, 'T) Pattern.t;
        default: ('M, 'T) Expression.t option;
        shorthand: bool;
      }
      [@@deriving show]
    end

    type ('M, 'T) property =
      | Property of ('M, 'T) Property.t
      | RestElement of ('M, 'T) RestElement.t

    and ('M, 'T) t = {
      properties: ('M, 'T) property list;
      annot: ('M, 'T) Type.annotation_or_hint;
    }
    [@@deriving show]
  end

  module Array : sig
    module Element : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        argument: ('M, 'T) Pattern.t;
        default: ('M, 'T) Expression.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) element =
      | Element of ('M, 'T) Element.t
      | RestElement of ('M, 'T) RestElement.t

    and ('M, 'T) t = {
      elements: ('M, 'T) element option list;
      annot: ('M, 'T) Type.annotation_or_hint;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Identifier : sig
    type ('M, 'T) t = {
      name: ('M, 'T) Identifier.t;
      annot: ('M, 'T) Type.annotation_or_hint;
      optional: bool;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = 'T * ('M, 'T) t'

  and ('M, 'T) t' =
    | Object of ('M, 'T) Object.t
    | Array of ('M, 'T) Array.t
    | Identifier of ('M, 'T) Identifier.t
    | Expression of ('M, 'T) Expression.t
  [@@deriving show]
end =
  Pattern

and Comment : sig
  type 'M t = 'M * t'

  and t' =
    | Block of string
    | Line of string
  [@@deriving show]
end =
  Comment

and Class : sig
  module Method : sig
    type ('M, 'T) t = 'T * ('M, 'T) t'

    and kind =
      | Constructor
      | Method
      | Get
      | Set

    and ('M, 'T) t' = {
      kind: kind;
      key: ('M, 'T) Expression.Object.Property.key;
      value: 'M * ('M, 'T) Function.t;
      static: bool;
      decorators: ('M, 'T) Class.Decorator.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Property : sig
    type ('M, 'T) t = 'T * ('M, 'T) t'

    and ('M, 'T) t' = {
      key: ('M, 'T) Expression.Object.Property.key;
      value: ('M, 'T) value;
      annot: ('M, 'T) Type.annotation_or_hint;
      static: bool;
      variance: 'M Variance.t option;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) value =
      | Declared
      | Uninitialized
      | Initialized of ('M, 'T) Expression.t
    [@@deriving show]
  end

  module PrivateField : sig
    type ('M, 'T) t = 'T * ('M, 'T) t'

    and ('M, 'T) t' = {
      key: 'M PrivateName.t;
      value: ('M, 'T) Class.Property.value;
      annot: ('M, 'T) Type.annotation_or_hint;
      static: bool;
      variance: 'M Variance.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Extends : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      expr: ('M, 'T) Expression.t;
      targs: ('M, 'T) Type.TypeArgs.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Implements : sig
    module Interface : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        id: ('M, 'T) Identifier.t;
        targs: ('M, 'T) Type.TypeArgs.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      interfaces: ('M, 'T) Interface.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Body : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      body: ('M, 'T) element list;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) element =
      | Method of ('M, 'T) Method.t
      | Property of ('M, 'T) Property.t
      | PrivateField of ('M, 'T) PrivateField.t
    [@@deriving show]
  end

  module Decorator : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = { expression: ('M, 'T) Expression.t } [@@deriving show]
  end

  type ('M, 'T) t = {
    id: ('M, 'T) Identifier.t option;
    body: ('M, 'T) Class.Body.t;
    tparams: ('M, 'T) Type.TypeParams.t option;
    extends: ('M, 'T) Extends.t option;
    implements: ('M, 'T) Implements.t option;
    classDecorators: ('M, 'T) Decorator.t list;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  Class

and Function : sig
  module RestParam : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Pattern.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Param : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Pattern.t;
      default: ('M, 'T) Expression.t option;
    }
    [@@deriving show]
  end

  module Params : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      params: ('M, 'T) Param.t list;
      rest: ('M, 'T) RestParam.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = {
    id: ('M, 'T) Identifier.t option;
    params: ('M, 'T) Params.t;
    body: ('M, 'T) body;
    async: bool;
    generator: bool;
    predicate: ('M, 'T) Type.Predicate.t option;
    return: ('M, 'T) Type.annotation_or_hint;
    tparams: ('M, 'T) Type.TypeParams.t option;
    comments: ('M, unit) Syntax.t option;
    (* Location of the signature portion of a function, e.g.
     * function foo(): void {}
     * ^^^^^^^^^^^^^^^^^^^^
     *)
    sig_loc: 'M;
  }

  and ('M, 'T) body =
    | BodyBlock of ('M * ('M, 'T) Statement.Block.t)
    | BodyExpression of ('M, 'T) Expression.t
  [@@deriving show]
end =
  Function]

type ('M, 'T) program = 'M * ('M, 'T) Statement.t list * 'M Comment.t list [@@deriving show]
