(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module Types = struct
  type 'T ast = (ALoc.t, 'T) Ast.Function.Params.t

  type 'T param_ast = (ALoc.t, 'T) Ast.Function.Param.t

  type 'T rest_ast = (ALoc.t, 'T) Ast.Function.RestParam.t

  type 'T this_ast = (ALoc.t, 'T) Ast.Function.ThisParam.t

  type pattern =
    | Id of (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t
    | Object of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        properties: (ALoc.t, ALoc.t) Ast.Pattern.Object.property list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }
    | Array of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        elements: (ALoc.t, ALoc.t) Ast.Pattern.Array.element list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }

  type param =
    | Param of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        pattern: pattern;
        default: (ALoc.t, ALoc.t) Ast.Expression.t option;
        has_anno: bool;
      }

  type rest =
    | Rest of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        id: (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t;
        has_anno: bool;
      }

  type this_param =
    | This of {
        t: Type.t;
        loc: ALoc.t;
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation;
      }
end
