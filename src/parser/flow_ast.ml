(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[%%gen
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
    name: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  PrivateName

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
    (* This will be None if we couldn't parse `raw`. That could be if the number is out of range or invalid (like a float) *)
    value: int64 option;
    raw: string;
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

and RegExpLiteral : sig
  type 'M t = {
    pattern: string;
    flags: string;
    raw: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  RegExpLiteral

and ModuleRefLiteral : sig
  type ('M, 'T) t = {
    value: string;
    require_loc: 'M;
    def_loc_opt: 'M option;
    prefix_len: int;
    raw: string;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  ModuleRefLiteral

and Variance : sig
  type 'M t = 'M * 'M t'

  and kind =
    | Plus
    | Minus
    | Readonly
    | In
    | Out
    | InOut

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

and Variable : sig
  type kind =
    | Var
    | Let
    | Const
  [@@deriving show]
end =
  Variable

and Type : sig
  module Conditional : sig
    type ('M, 'T) t = {
      check_type: ('M, 'T) Type.t;
      extends_type: ('M, 'T) Type.t;
      true_type: ('M, 'T) Type.t;
      false_type: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Infer : sig
    type ('M, 'T) t = {
      tparam: ('M, 'T) Type.TypeParam.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

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

    module ThisParam : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        annot: ('M, 'T) Type.annotation;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    module Params : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        this_: ('M, 'T) ThisParam.t option;
        params: ('M, 'T) Param.t list;
        rest: ('M, 'T) RestParam.t option;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      tparams: ('M, 'T) Type.TypeParams.t option;
      params: ('M, 'T) Params.t;
      return: ('M, 'T) return_annotation;
      comments: ('M, unit) Syntax.t option;
      effect_: Function.effect_;
    }

    and ('M, 'T) return_annotation =
      | TypeAnnotation of ('M, 'T) Type.t
      | TypeGuard of ('M, 'T) Type.TypeGuard.t
    [@@deriving show]
  end

  module Component : sig
    module Param : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        name: ('M, 'T) Statement.ComponentDeclaration.Param.param_name;
        annot: ('M, 'T) Type.annotation;
        optional: bool;
      }
      [@@deriving show]
    end

    module RestParam : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        argument: ('M, 'T) Identifier.t option;
        annot: ('M, 'T) Type.t;
        optional: bool;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    module Params : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        params: ('M, 'T) Param.t list;
        rest: ('M, 'T) RestParam.t option;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      tparams: ('M, 'T) Type.TypeParams.t option;
      params: ('M, 'T) Params.t;
      renders: ('M, 'T) Type.component_renders_annotation;
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

  module IndexedAccess : sig
    type ('M, 'T) t = {
      _object: ('M, 'T) Type.t;
      index: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module OptionalIndexedAccess : sig
    type ('M, 'T) t = {
      indexed_access: ('M, 'T) IndexedAccess.t;
      optional: bool;
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
        comments: ('M, unit) Syntax.t option;
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

    module MappedType : sig
      (* PlusOptional = +?, MinusOptional = -?, Optional = ?, NoOptionalFlag = blank *)
      type optional_flag =
        | PlusOptional
        | MinusOptional
        | Optional
        | NoOptionalFlag
      [@@deriving show]

      type ('M, 'T) t' = {
        key_tparam: ('M, 'T) Type.TypeParam.t;
        prop_type: ('M, 'T) Type.t;
        source_type: ('M, 'T) Type.t;
        variance: 'M Variance.t option;
        optional: optional_flag;
        comments: ('M, unit) Syntax.t option;
      }

      and ('M, 'T) t = 'M * ('M, 'T) t' [@@deriving show]
    end

    module CallProperty : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        value: 'M * ('M, 'T) Function.t;
        static: bool;
        comments: ('M, unit) Syntax.t option;
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
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }

    and ('M, 'T) property =
      | Property of ('M, 'T) Property.t
      | SpreadProperty of ('M, 'T) SpreadProperty.t
      | Indexer of ('M, 'T) Indexer.t
      | CallProperty of ('M, 'T) CallProperty.t
      | InternalSlot of ('M, 'T) InternalSlot.t
      | MappedType of ('M, 'T) MappedType.t
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
    module Target : sig
      type ('M, 'T) t =
        | Unqualified of ('M, 'T) Identifier.t
        | Qualified of ('M, 'T) qualified

      and ('M, 'T) qualified' = {
        qualification: ('M, 'T) t;
        id: ('M, 'T) Identifier.t;
      }

      and ('M, 'T) qualified = 'T * ('M, 'T) qualified' [@@deriving show]
    end

    type ('M, 'T) t = {
      argument: ('M, 'T) Target.t;
      targs: ('M, 'T) Type.TypeArgs.t option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Keyof : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Renders : sig
    type variant =
      | Normal
      | Maybe
      | Star
    [@@deriving show]

    type ('M, 'T) t = {
      operator_loc: 'M;
      argument: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
      variant: variant;
    }
    [@@deriving show]
  end

  module ReadOnly : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Type.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Tuple : sig
    module LabeledElement : sig
      type ('M, 'T) t = {
        name: ('M, 'T) Identifier.t;
        annot: ('M, 'T) Type.t;
        variance: 'M Variance.t option;
        optional: bool;
      }
      [@@deriving show]
    end

    module SpreadElement : sig
      type ('M, 'T) t = {
        name: ('M, 'T) Identifier.t option;
        annot: ('M, 'T) Type.t;
      }
      [@@deriving show]
    end

    type ('M, 'T) element = 'M * ('M, 'T) element' [@@deriving show]

    and ('M, 'T) element' =
      | UnlabeledElement of ('M, 'T) Type.t
      | LabeledElement of ('M, 'T) LabeledElement.t
      | SpreadElement of ('M, 'T) SpreadElement.t
    [@@deriving show]

    and ('M, 'T) t = {
      elements: ('M, 'T) element list;
      inexact: bool;
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
    | Boolean of {
        raw: [ `Boolean | `Bool ];
        comments: ('M, unit) Syntax.t option;
      }
    | Symbol of ('M, unit) Syntax.t option
    | Exists of ('M, unit) Syntax.t option
    | Nullable of ('M, 'T) Nullable.t
    | Function of ('M, 'T) Function.t
    | Component of ('M, 'T) Component.t
    | Object of ('M, 'T) Object.t
    | Interface of ('M, 'T) Interface.t
    | Array of ('M, 'T) Array.t
    | Conditional of ('M, 'T) Conditional.t
    | Infer of ('M, 'T) Infer.t
    | Generic of ('M, 'T) Generic.t
    | IndexedAccess of ('M, 'T) IndexedAccess.t
    | OptionalIndexedAccess of ('M, 'T) OptionalIndexedAccess.t
    | Union of ('M, 'T) Union.t
    | Intersection of ('M, 'T) Intersection.t
    | Typeof of ('M, 'T) Typeof.t
    | Keyof of ('M, 'T) Keyof.t
    | Renders of ('M, 'T) Renders.t
    | ReadOnly of ('M, 'T) ReadOnly.t
    | Tuple of ('M, 'T) Tuple.t
    | StringLiteral of 'M StringLiteral.t
    | NumberLiteral of 'M NumberLiteral.t
    | BigIntLiteral of 'M BigIntLiteral.t
    | BooleanLiteral of 'M BooleanLiteral.t
    | Unknown of ('M, unit) Syntax.t option
    | Never of ('M, unit) Syntax.t option
    | Undefined of ('M, unit) Syntax.t option

  (* Type.annotation is a concrete syntax node with a location that starts at
   * the colon and ends after the type. For example, "var a: number", the
   * identifier a would have a property annot which contains a
   * Type.annotation with a location from column 6-14 *)
  and ('M, 'T) annotation = 'M * ('M, 'T) t

  (* Same convention about the colon holds for type guards. *)
  and ('M, 'T) type_guard_annotation = 'M * ('M, 'T) Type.TypeGuard.t

  and ('M, 'T) annotation_or_hint =
    | Missing of 'T
    | Available of ('M, 'T) Type.annotation
  [@@deriving show]

  and ('M, 'T) component_renders_annotation =
    | MissingRenders of 'T
    | AvailableRenders of 'M * ('M, 'T) Type.Renders.t
  [@@deriving show]

  module TypeParam : sig
    type bound_kind =
      | Colon
      | Extends
    [@@deriving show]

    module ConstModifier : sig
      type 'M t = 'M * ('M, unit) Syntax.t option [@@deriving show]
    end

    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      name: ('M, 'M) Identifier.t;
      bound: ('M, 'T) Type.annotation_or_hint;
      bound_kind: bound_kind;
      variance: 'M Variance.t option;
      default: ('M, 'T) Type.t option;
      const: 'M ConstModifier.t option;
    }
    [@@deriving show]
  end

  module TypeParams : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      params: ('M, 'T) TypeParam.t list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module TypeArgs : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      arguments: ('M, 'T) Type.t list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
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

  module TypeGuard : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and kind =
      | Default
      | Asserts
      | Implies

    and ('M, 'T) t' = {
      kind: kind;
      guard: ('M, 'M) Identifier.t * ('M, 'T) Type.t option;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end
end =
  Type

and Statement : sig
  module Block : sig
    type ('M, 'T) t = {
      body: ('M, 'T) Statement.t list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module If : sig
    module Alternate : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        body: ('M, 'T) Statement.t;
        comments: ('M, unit) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      test: ('M, 'T) Expression.t;
      consequent: ('M, 'T) Statement.t;
      alternate: ('M, 'T) Alternate.t option;
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

  type ('M, 'T) match_statement = ('M, 'T, ('M, 'T) Statement.t) Match.t [@@deriving show]

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
      exhaustive_out: 'T;
    }
    [@@deriving show]
  end

  module Return : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t option;
      comments: ('M, unit) Syntax.t option;
      return_out: 'T;
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
      kind: Variable.kind;
      comments: ('M, unit) Syntax.t option;
    }
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
      comments: ('M, unit) Syntax.t option;
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
      comments: ('M, unit) Syntax.t option;
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
      comments: ('M, unit) Syntax.t option;
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
        explicit_type: bool;
        has_unknown_members: bool;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    module NumberBody : sig
      type 'M t = {
        members: ('M NumberLiteral.t, 'M) InitializedMember.t list;
        explicit_type: bool;
        has_unknown_members: bool;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    module StringBody : sig
      type 'M t = {
        members: ('M StringLiteral.t, 'M) members;
        explicit_type: bool;
        has_unknown_members: bool;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }

      and ('I, 'M) members =
        | Defaulted of 'M DefaultedMember.t list
        | Initialized of ('I, 'M) InitializedMember.t list
      [@@deriving show]
    end

    module SymbolBody : sig
      type 'M t = {
        members: 'M DefaultedMember.t list;
        has_unknown_members: bool;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    module BigIntBody : sig
      type 'M t = {
        members: ('M BigIntLiteral.t, 'M) InitializedMember.t list;
        explicit_type: bool;
        has_unknown_members: bool;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      body: 'M body;
      comments: ('M, unit) Syntax.t option;
    }

    and 'M body = 'M * 'M body'

    and 'M body' =
      | BooleanBody of 'M BooleanBody.t
      | NumberBody of 'M NumberBody.t
      | StringBody of 'M StringBody.t
      | SymbolBody of 'M SymbolBody.t
      | BigIntBody of 'M BigIntBody.t
    [@@deriving show]
  end

  module ComponentDeclaration : sig
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
        (* Name should only be an Identifier or StringLiteral. However, we allow parsing
           it as an option to have better error messages. *)
        name: ('M, 'T) param_name;
        local: ('M, 'T) Pattern.t;
        default: ('M, 'T) Expression.t option;
        shorthand: bool;
      }

      and ('M, 'T) param_name =
        | Identifier of ('M, 'T) Identifier.t
        | StringLiteral of ('M * 'M StringLiteral.t)
      [@@deriving show]
    end

    module Params : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        params: ('M, 'T) Param.t list;
        rest: ('M, 'T) RestParam.t option;
        comments: ('M, 'M Comment.t list) Syntax.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      params: ('M, 'T) Params.t;
      renders: ('M, 'T) Type.component_renders_annotation;
      body: 'M * ('M, 'T) Statement.Block.t;
      comments: ('M, unit) Syntax.t option;
      (* Location of the signature portion of a component, e.g.
       * component Foo(): void {}
       * ^^^^^^^^^^^^^^^^^^^^
       *)
      sig_loc: 'M;
    }
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
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareComponent : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      tparams: ('M, 'T) Type.TypeParams.t option;
      params: ('M, 'T) Type.Component.Params.t;
      renders: ('M, 'T) Type.component_renders_annotation;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareVariable : sig
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      annot: ('M, 'T) Type.annotation;
      kind: Variable.kind;
      comments: ('M, unit) Syntax.t option;
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

    and ('M, 'T) t = {
      id: ('M, 'T) id;
      body: 'M * ('M, 'T) Block.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareModuleExports : sig
    type ('M, 'T) t = {
      annot: ('M, 'T) Type.annotation;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module DeclareNamespace : sig
    type ('M, 'T) id =
      | Global of ('M, 'M) Identifier.t
      | Local of ('M, 'T) Identifier.t
    [@@deriving show]

    type ('M, 'T) t = {
      id: ('M, 'T) id;
      body: 'M * ('M, 'T) Block.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module ExportNamedDeclaration : sig
    module ExportSpecifier : sig
      type ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' = {
        local: ('M, 'T) Identifier.t;
        exported: ('M, 'T) Identifier.t option;
        from_remote: bool;
        (* Imported name's definition location. It will be populated only in typed AST for `export {foo} from '...'`. *)
        imported_name_def_loc: 'M option;
      }
      [@@deriving show]
    end

    module ExportBatchSpecifier : sig
      type ('M, 'T) t = 'M * ('M, 'T) Identifier.t option [@@deriving show]
    end

    type ('M, 'T) t = {
      declaration: ('M, 'T) Statement.t option;
      specifiers: ('M, 'T) specifier option;
      source: ('T * 'M StringLiteral.t) option;
      export_kind: Statement.export_kind;
      comments: ('M, unit) Syntax.t option;
    }

    and ('M, 'T) specifier =
      | ExportSpecifiers of ('M, 'T) ExportSpecifier.t list
      | ExportBatchSpecifier of ('M, 'T) ExportBatchSpecifier.t
    [@@deriving show]
  end

  module ExportDefaultDeclaration : sig
    type ('M, 'T) t = {
      default: 'T;
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
      (* declare export component *)
      | Component of ('M * ('M, 'T) DeclareComponent.t)
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
      (* declare export enum *)
      | Enum of ('M * ('M, 'T) EnumDeclaration.t)

    and ('M, 'T) t = {
      default: 'M option;
      declaration: ('M, 'T) declaration option;
      specifiers: ('M, 'T) ExportNamedDeclaration.specifier option;
      source: ('T * 'M StringLiteral.t) option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module ImportDeclaration : sig
    type import_kind =
      | ImportType
      | ImportTypeof
      | ImportValue

    and ('M, 'T) specifier =
      | ImportNamedSpecifiers of ('M, 'T) named_specifier list
      | ImportNamespaceSpecifier of ('M * ('M, 'T) Identifier.t)

    and ('M, 'T) named_specifier = {
      kind: import_kind option;
      local: ('M, 'T) Identifier.t option;
      remote: ('M, 'T) Identifier.t;
      (* Remote name's definition location. It will be populated only in typed AST. *)
      remote_name_def_loc: 'M option;
    }

    and ('M, 'T) default_identifier = {
      identifier: ('M, 'T) Identifier.t;
      (* Remote name's definition location. It will be populated only in typed AST. *)
      remote_default_name_def_loc: 'M option;
    }

    and ('M, 'T) t = {
      import_kind: import_kind;
      source: 'T * 'M StringLiteral.t;
      default: ('M, 'T) default_identifier option;
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

  type export_kind =
    | ExportType
    | ExportValue

  and ('M, 'T) t = 'M * ('M, 'T) t'

  and ('M, 'T) t' =
    | Block of ('M, 'T) Block.t
    | Break of 'M Break.t
    | ClassDeclaration of ('M, 'T) Class.t
    | ComponentDeclaration of ('M, 'T) ComponentDeclaration.t
    | Continue of 'M Continue.t
    | Debugger of 'M Debugger.t
    | DeclareClass of ('M, 'T) DeclareClass.t
    | DeclareComponent of ('M, 'T) DeclareComponent.t
    | DeclareEnum of ('M, 'T) EnumDeclaration.t
    | DeclareExportDeclaration of ('M, 'T) DeclareExportDeclaration.t
    | DeclareFunction of ('M, 'T) DeclareFunction.t
    | DeclareInterface of ('M, 'T) Interface.t
    | DeclareModule of ('M, 'T) DeclareModule.t
    | DeclareModuleExports of ('M, 'T) DeclareModuleExports.t
    | DeclareNamespace of ('M, 'T) DeclareNamespace.t
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
    | Match of ('M, 'T) match_statement
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

    and ('M, 'T) t' = {
      arguments: ('M, 'T) CallTypeArg.t list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module SpreadElement : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Array : sig
    type ('M, 'T) element =
      | Expression of ('M, 'T) Expression.t
      | Spread of ('M, 'T) SpreadElement.t
      | Hole of 'M
    [@@deriving show]

    type ('M, 'T) t = {
      elements: ('M, 'T) element list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
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
        | StringLiteral of ('T * 'M StringLiteral.t)
        | NumberLiteral of ('T * 'M NumberLiteral.t)
        | BigIntLiteral of ('T * 'M BigIntLiteral.t)
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
      comments: ('M, 'M Comment.t list) Syntax.t option;
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
      | Nonnull

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
      | NullishAssign
      | AndAssign
      | OrAssign

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

  type ('M, 'T) expression_or_spread =
    | Expression of ('M, 'T) Expression.t
    | Spread of ('M, 'T) SpreadElement.t
  [@@deriving show]

  module ArgList : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      arguments: ('M, 'T) expression_or_spread list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
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
    type kind =
      | Optional
      | NonOptional
      | AssertNonnull
    [@@deriving show]

    type ('M, 'T) t = {
      call: ('M, 'T) Call.t;
      filtered_out: 'T;
      optional: kind;
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
    type kind =
      | Optional
      | NonOptional
      | AssertNonnull
    [@@deriving show]

    type ('M, 'T) t = {
      member: ('M, 'T) Member.t;
      filtered_out: 'T;
      optional: kind;
    }
    [@@deriving show]
  end

  module Yield : sig
    type ('M, 'T) t = {
      argument: ('M, 'T) Expression.t option;
      comments: ('M, unit) Syntax.t option;
      delegate: bool;
      result_out: 'T;
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

  module AsExpression : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) Expression.t;
      annot: ('M, 'T) Type.annotation;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module AsConstExpression : sig
    type ('M, 'T) t = {
      expression: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module TSSatisfies : sig
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

  type ('M, 'T) match_expression = ('M, 'T, ('M, 'T) Expression.t) Match.t [@@deriving show]

  type ('M, 'T) t = 'T * ('M, 'T) t'

  and ('M, 'T) t' =
    | Array of ('M, 'T) Array.t
    | ArrowFunction of ('M, 'T) Function.t
    | AsConstExpression of ('M, 'T) AsConstExpression.t
    | AsExpression of ('M, 'T) AsExpression.t
    | Assignment of ('M, 'T) Assignment.t
    | Binary of ('M, 'T) Binary.t
    | Call of ('M, 'T) Call.t
    | Class of ('M, 'T) Class.t
    | Conditional of ('M, 'T) Conditional.t
    | Function of ('M, 'T) Function.t
    | Identifier of ('M, 'T) Identifier.t
    | Import of ('M, 'T) Import.t
    | JSXElement of ('M, 'T) JSX.element
    | JSXFragment of ('M, 'T) JSX.fragment
    | StringLiteral of 'M StringLiteral.t
    | BooleanLiteral of 'M BooleanLiteral.t
    | NullLiteral of ('M, unit) Syntax.t option
    | NumberLiteral of 'M NumberLiteral.t
    | BigIntLiteral of 'M BigIntLiteral.t
    | RegExpLiteral of 'M RegExpLiteral.t
    | ModuleRefLiteral of ('M, 'T) ModuleRefLiteral.t
    | Logical of ('M, 'T) Logical.t
    | Match of ('M, 'T) match_expression
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
    | TSSatisfies of ('M, 'T) TSSatisfies.t
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
      comments: ('M, 'M Comment.t list) Syntax.t option;
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
      | StringLiteral of ('T * 'M StringLiteral.t)
      | ExpressionContainer of ('T * ('M, 'T) ExpressionContainer.t)

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
      targs: ('M, 'T) Expression.CallTypeArgs.t option;
      self_closing: bool;
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

  type ('M, 'T) child = 'T * ('M, 'T) child'

  and ('M, 'T) child' =
    | Element of ('M, 'T) element
    | Fragment of ('M, 'T) fragment
    | ExpressionContainer of ('M, 'T) ExpressionContainer.t
    | SpreadChild of ('M, 'T) SpreadChild.t
    | Text of Text.t

  and ('M, 'T) element = {
    opening_element: ('M, 'T) Opening.t;
    closing_element: ('M, 'T) Closing.t option;
    children: 'M * ('M, 'T) child list;
    comments: ('M, unit) Syntax.t option;
  }

  and ('M, 'T) fragment = {
    frag_opening_element: 'M;
    frag_closing_element: 'M;
    frag_children: 'M * ('M, 'T) child list;
    frag_comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  JSX

and Match : sig
  module Case : sig
    module InvalidSyntax : sig
      type 'M t = {
        invalid_prefix_case: 'M option;
        invalid_infix_colon: 'M option;
        invalid_suffix_semicolon: 'M option;
      }
      [@@deriving show]
    end

    type ('M, 'T, 'B) t = 'M * ('M, 'T, 'B) t'

    and ('M, 'T, 'B) t' = {
      pattern: ('M, 'T) MatchPattern.t;
      body: 'B;
      guard: ('M, 'T) Expression.t option;
      comments: ('M, unit) Syntax.t option;
      invalid_syntax: 'M InvalidSyntax.t;
    }
    [@@deriving show]
  end

  type ('M, 'T, 'B) t = {
    arg: ('M, 'T) Expression.t;
    cases: ('M, 'T, 'B) Case.t list;
    (* The type here is used to store the resulting type after the patterns
       refine the arg type. *)
    match_keyword_loc: 'T;
    comments: ('M, unit) Syntax.t option;
  }
  [@@deriving show]
end =
  Match

and MatchPattern : sig
  module UnaryPattern : sig
    type operator =
      | Plus
      | Minus

    and 'M argument =
      | NumberLiteral of 'M NumberLiteral.t
      | BigIntLiteral of 'M BigIntLiteral.t

    and 'M t = {
      operator: operator;
      argument: 'M * 'M argument;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module MemberPattern : sig
    type ('M, 'T) base =
      | BaseIdentifier of ('M, 'T) Identifier.t
      | BaseMember of ('M, 'T) t

    and ('M, 'T) property =
      | PropertyString of ('M * 'M StringLiteral.t)
      | PropertyNumber of ('M * 'M NumberLiteral.t)
      | PropertyBigInt of ('M * 'M BigIntLiteral.t)
      | PropertyIdentifier of ('M, 'T) Identifier.t

    and ('M, 'T) t = 'T * ('M, 'T) t'

    and ('M, 'T) t' = {
      base: ('M, 'T) base;
      property: ('M, 'T) property;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module BindingPattern : sig
    type ('M, 'T) t = {
      kind: Variable.kind;
      id: ('M, 'T) Identifier.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module RestPattern : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      argument: ('M * ('M, 'T) BindingPattern.t) option;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module ObjectPattern : sig
    module Property : sig
      type ('M, 'T) key =
        | StringLiteral of ('M * 'M StringLiteral.t)
        | NumberLiteral of ('M * 'M NumberLiteral.t)
        | BigIntLiteral of ('M * 'M BigIntLiteral.t)
        | Identifier of ('M, 'T) Identifier.t

      and ('M, 'T) property = {
        key: ('M, 'T) key;
        pattern: ('M, 'T) MatchPattern.t;
        shorthand: bool;
        comments: ('M, unit) Syntax.t option;
      }

      and ('M, 'T) t = 'M * ('M, 'T) t'

      and ('M, 'T) t' =
        | Valid of ('M, 'T) property
        (* Invalid code parsed so we can error with quick-fix. *)
        | InvalidShorthand of ('M, 'M) Identifier.t
      [@@deriving show]
    end

    type ('M, 'T) t = {
      properties: ('M, 'T) Property.t list;
      rest: ('M, 'T) RestPattern.t option;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module ArrayPattern : sig
    module Element : sig
      type ('M, 'T) t = {
        index: 'M;
        pattern: ('M, 'T) MatchPattern.t;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      elements: ('M, 'T) Element.t list;
      rest: ('M, 'T) RestPattern.t option;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module OrPattern : sig
    type ('M, 'T) t = {
      patterns: ('M, 'T) MatchPattern.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module AsPattern : sig
    type ('M, 'T) target =
      | Identifier of ('M, 'T) Identifier.t
      | Binding of 'M * ('M, 'T) BindingPattern.t

    and ('M, 'T) t = {
      pattern: ('M, 'T) MatchPattern.t;
      target: ('M, 'T) target;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module WildcardPattern : sig
    type 'M t = {
      comments: ('M, unit) Syntax.t option;
      invalid_syntax_default_keyword: bool;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = 'M * ('M, 'T) t'

  and ('M, 'T) t' =
    | WildcardPattern of 'M WildcardPattern.t
    | NumberPattern of 'M NumberLiteral.t
    | BigIntPattern of 'M BigIntLiteral.t
    | StringPattern of 'M StringLiteral.t
    | BooleanPattern of 'M BooleanLiteral.t
    | NullPattern of ('M, unit) Syntax.t option
    | UnaryPattern of 'M UnaryPattern.t
    | BindingPattern of ('M, 'T) BindingPattern.t
    | IdentifierPattern of ('M, 'T) Identifier.t
    | MemberPattern of ('M, 'T) MemberPattern.t
    | ObjectPattern of ('M, 'T) ObjectPattern.t
    | ArrayPattern of ('M, 'T) ArrayPattern.t
    | OrPattern of ('M, 'T) OrPattern.t
    | AsPattern of ('M, 'T) AsPattern.t
  [@@deriving show]
end =
  MatchPattern

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
        | StringLiteral of ('M * 'M StringLiteral.t)
        | NumberLiteral of ('M * 'M NumberLiteral.t)
        | BigIntLiteral of ('M * 'M BigIntLiteral.t)
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
      comments: ('M, 'M Comment.t list) Syntax.t option;
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
      | Hole of 'M

    and ('M, 'T) t = {
      elements: ('M, 'T) element list;
      annot: ('M, 'T) Type.annotation_or_hint;
      comments: ('M, 'M Comment.t list) Syntax.t option;
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

  and kind =
    | Block
    | Line

  and t' = {
    kind: kind;
    text: string;
    on_newline: bool;
  }
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
      decorators: ('M, 'T) Class.Decorator.t list;
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
      decorators: ('M, 'T) Class.Decorator.t list;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module StaticBlock : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      body: ('M, 'T) Statement.t list;
      comments: ('M, 'M Comment.t list) Syntax.t option;
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
      | StaticBlock of ('M, 'T) StaticBlock.t
    [@@deriving show]
  end

  module Decorator : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      expression: ('M, 'T) Expression.t;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = {
    id: ('M, 'T) Identifier.t option;
    body: ('M, 'T) Class.Body.t;
    tparams: ('M, 'T) Type.TypeParams.t option;
    extends: ('M, 'T) Extends.t option;
    implements: ('M, 'T) Implements.t option;
    class_decorators: ('M, 'T) Decorator.t list;
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

  module ThisParam : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      annot: ('M, 'T) Type.annotation;
      comments: ('M, unit) Syntax.t option;
    }
    [@@deriving show]
  end

  module Params : sig
    type ('M, 'T) t = 'M * ('M, 'T) t'

    and ('M, 'T) t' = {
      this_: ('M, 'T) ThisParam.t option;
      params: ('M, 'T) Param.t list;
      rest: ('M, 'T) RestParam.t option;
      comments: ('M, 'M Comment.t list) Syntax.t option;
    }
    [@@deriving show]
  end

  module ReturnAnnot : sig
    type ('M, 'T) t =
      | Missing of 'T
      | Available of ('M, 'T) Type.annotation
      | TypeGuard of ('M, 'T) Type.type_guard_annotation
    [@@deriving show]
  end

  type effect_ =
    | Hook
    | Arbitrary
  [@@deriving show]

  type ('M, 'T) t = {
    id: ('M, 'T) Identifier.t option;
    params: ('M, 'T) Params.t;
    body: ('M, 'T) body;
    async: bool;
    generator: bool;
    effect_: effect_;
    predicate: ('M, 'T) Type.Predicate.t option;
    return: ('M, 'T) ReturnAnnot.t;
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
  Function

and Program : sig
  type ('M, 'T) t = 'M * ('M, 'T) t'

  and ('M, 'T) t' = {
    statements: ('M, 'T) Statement.t list;
    interpreter: ('M * string) option;  (** interpreter directive / shebang *)
    comments: ('M, unit) Syntax.t option;
    all_comments: 'M Comment.t list;
  }
  [@@deriving show]
end =
  Program]
