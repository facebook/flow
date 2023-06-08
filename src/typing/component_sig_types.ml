(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(* These definitions are defined outside of component_sig because they are used in
 * statement_sig.ml and we need to avoid a cycle *)

(* This interface allows us to re-use the component_sig module for component
 * declarations, expressions, declared components, and types. The ParamConfig defines the
 * types used in the AST for the component parameters. We use the ParamConfig in the Param functor,
 * defined below, to create a common representation to be used by component_sig to verify parameter
 * types and component bodies *)
module ParamConfig = struct
  module type S = sig
    type 'T ast

    type 'T param_ast

    type 'T rest_ast

    type param

    type rest

    type pattern
  end
end

module BodyConfig = struct
  module type S = sig
    type 'T body
  end
end

module ParamTypes = struct
  module type S = sig
    module Config : ParamConfig.S

    type reconstruct =
      (ALoc.t * Type.t) Config.param_ast list ->
      (ALoc.t * Type.t) Config.rest_ast option ->
      (ALoc.t * Type.t) Config.ast

    type t = {
      params_rev: Config.param list;
      rest: Config.rest option;
      reconstruct: reconstruct;
    }
  end

  module Make (C : ParamConfig.S) : S with module Config := C = struct
    open C

    type reconstruct =
      (ALoc.t * Type.t) param_ast list -> (ALoc.t * Type.t) rest_ast option -> (ALoc.t * Type.t) ast

    type t = {
      params_rev: param list;
      rest: rest option;
      reconstruct: reconstruct;
    }
  end
end

module DeclarationBodyConfig = struct
  type 'T body = ALoc.t * (ALoc.t, 'T) Ast.Statement.Block.t
end

module DeclarationParamConfig = struct
  type 'T ast = (ALoc.t, 'T) Ast.Statement.ComponentDeclaration.Params.t

  type 'T param_ast = (ALoc.t, 'T) Ast.Statement.ComponentDeclaration.Param.t

  type 'T rest_ast = (ALoc.t, 'T) Ast.Statement.ComponentDeclaration.RestParam.t

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
        shorthand: bool;
        name: (ALoc.t, ALoc.t) Ast.Statement.ComponentDeclaration.Param.param_name option;
      }

  type rest =
    | Rest of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        id: (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t;
        has_anno: bool;
      }
end

(* A common interfaces for the full signature of a component, including the common
 * representation for parameters. Component_sig.Make, takes in a module
 * matching this signature as an input. The functions in that functor operate over this
 * data structure and use it to extract a signature, check the body, etc.
 *
 * It is a functor so that any ParamConfig can be given, so this module can be re-used
 * to support declared components, component types, etc. *)
module ComponentSig = struct
  module type S = sig
    module Body : BodyConfig.S

    module Config : ParamConfig.S

    module Param : ParamTypes.S with module Config := Config

    type 'T body = 'T Body.body

    type component_params = Param.t

    type component_params_tast = (ALoc.t * Type.t) Config.ast

    type t = {
      reason: Reason.t;
      tparams: Type.typeparams;
      cparams: component_params;
      body: ALoc.t body;
      renders_t: Type.t;
      ret_annot_loc: ALoc.t;
    }
  end

  module Make
      (Body : BodyConfig.S)
      (Config : ParamConfig.S)
      (Param : ParamTypes.S with module Config := Config) :
    S with module Config := Config and module Param := Param and module Body := Body = struct
    type 'T body = 'T Body.body

    type component_params = Param.t

    type component_params_tast = (ALoc.t * Type.t) Config.ast

    type t = {
      reason: Reason.t;
      tparams: Type.typeparams;
      cparams: component_params;
      body: ALoc.t body;
      renders_t: Type.t;
      ret_annot_loc: ALoc.t;
    }
  end
end

module Component_declaration_params_types :
  ParamTypes.S with module Config := DeclarationParamConfig =
  ParamTypes.Make (DeclarationParamConfig)

module Component_declaration_sig_types :
  ComponentSig.S
    with module Config := DeclarationParamConfig
     and module Param := Component_declaration_params_types
     and module Body := DeclarationBodyConfig =
  ComponentSig.Make (DeclarationBodyConfig) (DeclarationParamConfig)
    (Component_declaration_params_types)
