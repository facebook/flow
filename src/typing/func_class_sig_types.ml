(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason

module Config = struct
  module type S = sig
    type 'T ast

    type 'T param_ast

    type 'T rest_ast

    type 'T this_ast

    type param

    type rest

    type this_param

    type pattern
  end
end

module Param = struct
  module type S = sig
    module Config : Config.S

    type reconstruct =
      (ALoc.t * Type.t) Config.param_ast list ->
      (ALoc.t * Type.t) Config.rest_ast option ->
      (ALoc.t * Type.t) Config.this_ast option ->
      (ALoc.t * Type.t) Config.ast option

    type t = {
      params_rev: Config.param list;
      rest: Config.rest option;
      this_: Config.this_param option;
      reconstruct: reconstruct;
    }
  end

  module Make (C : Config.S) : S with module Config := C = struct
    open C

    type reconstruct =
      (ALoc.t * Type.t) param_ast list ->
      (ALoc.t * Type.t) rest_ast option ->
      (ALoc.t * Type.t) this_ast option ->
      (ALoc.t * Type.t) ast option

    type t = {
      params_rev: param list;
      rest: rest option;
      this_: this_param option;
      reconstruct: reconstruct;
    }
  end
end

module Func = struct
  type kind =
    | Ordinary
    | Async
    | Generator of { return_loc: ALoc.t }
    | AsyncGenerator of { return_loc: ALoc.t }
    | FieldInit of (ALoc.t, ALoc.t) Flow_ast.Expression.t
    | Predicate
    | Ctor

  module type S = sig
    module Config : Config.S

    module Param : Param.S with module Config := Config

    type func_params = Param.t

    type func_params_tast = (ALoc.t * Type.t) Config.ast

    type t = {
      reason: Reason.t;
      kind: kind;
      tparams: Type.typeparams;
      tparams_map: Type.t Subst_name.Map.t;
      fparams: func_params;
      body: (ALoc.t, ALoc.t) Flow_ast.Function.body option;
      return_t: Type.annotated_or_inferred;
      statics: Type.t option;
    }
  end

  module Make (Config : Config.S) (Param : Param.S with module Config := Config) :
    S with module Config := Config and module Param := Param = struct
    type func_params = Param.t

    type func_params_tast = (ALoc.t * Type.t) Config.ast

    type t = {
      reason: Reason.t;
      kind: kind;
      tparams: Type.typeparams;
      tparams_map: Type.t Subst_name.Map.t;
      fparams: func_params;
      body: (ALoc.t, ALoc.t) Ast.Function.body option;
      return_t: Type.annotated_or_inferred;
      statics: Type.t option;
    }
  end
end

module Class = struct
  module type S = sig
    module Config : Config.S

    module Param : Param.S with module Config := Config

    module Func : Func.S with module Config := Config and module Param := Param

    type func_sig = Func.t

    type func_params_tast = (ALoc.t * Type.t) Config.ast

    type set_asts =
      func_params_tast option
      * (ALoc.t, ALoc.t * Type.t) Flow_ast.Function.body option
      * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t option ->
      unit

    type set_type = Type.t -> unit

    type field =
      | Annot of Type.t
      | Infer of func_sig * set_asts

    type field' = ALoc.t option * Polarity.t * field

    type typeapp = ALoc.t * Type.t * Type.t list option

    type extends =
      | Explicit of typeapp
      | Implicit of { null: bool }

    type class_super = {
      extends: extends;
      mixins: typeapp list;
      (* declare class only *)
      implements: typeapp list;
      this_tparam: Type.typeparam;
      this_t: Type.t;
    }

    type interface_super = {
      inline: bool;
      extends: typeapp list;
      callable: bool;
    }

    type super =
      | Interface of interface_super
      | Class of class_super

    type func_info = {
      id_loc: ALoc.t option;
      this_write_loc: ALoc.t option;
      func_sig: func_sig;
      set_asts: set_asts;
      set_type: set_type;
    }

    type signature = {
      reason: Reason.t;
      fields: field' SMap.t;
      private_fields: field' SMap.t;
      proto_fields: field' SMap.t;
      (* Multiple function signatures indicates an overloaded method. Note that
         function signatures are stored in reverse definition order. *)
      methods: func_info Nel.t SMap.t;
      private_methods: func_info SMap.t;
      getters: func_info SMap.t;
      setters: func_info SMap.t;
      calls: Type.t list;
    }

    type t = {
      id: ALoc.id;
      class_loc: ALoc.t;
      tparams: Type.typeparams;
      tparams_map: Type.t Subst_name.Map.t;
      super: super;
      (* Multiple function signatures indicates an overloaded constructor. Note that
         function signatures are stored in reverse definition order. *)
      constructor: func_info list;
      static: signature;
      instance: signature;
    }
  end

  module Make
      (Config : Config.S)
      (P : Param.S with module Config := Config)
      (F : Func.S with module Config := Config and module Param := P) :
    S with module Config := Config and module Param := P and module Func := F = struct
    type func_sig = F.t

    type func_params_tast = F.func_params_tast

    type set_asts =
      func_params_tast option
      * (ALoc.t, ALoc.t * Type.t) Ast.Function.body option
      * (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option ->
      unit

    type set_type = Type.t -> unit

    type field =
      | Annot of Type.t
      | Infer of func_sig * set_asts

    type field' = ALoc.t option * Polarity.t * field

    type typeapp = ALoc.t * Type.t * Type.t list option

    type extends =
      | Explicit of typeapp
      | Implicit of { null: bool }

    type class_super = {
      extends: extends;
      mixins: typeapp list;
      (* declare class only *)
      implements: typeapp list;
      this_tparam: Type.typeparam;
      this_t: Type.t;
    }

    type interface_super = {
      inline: bool;
      (* Anonymous interface, can appear anywhere inside a type *)
      extends: typeapp list;
      callable: bool;
    }

    type super =
      | Interface of interface_super
      | Class of class_super

    type func_info = {
      id_loc: ALoc.t option;
      this_write_loc: ALoc.t option;
      func_sig: func_sig;
      set_asts: set_asts;
      set_type: set_type;
    }

    type signature = {
      reason: reason;
      fields: field' SMap.t;
      private_fields: field' SMap.t;
      proto_fields: field' SMap.t;
      (* Multiple function signatures indicates an overloaded method. Note that
         function signatures are stored in reverse definition order. *)
      methods: func_info Nel.t SMap.t;
      private_methods: func_info SMap.t;
      getters: func_info SMap.t;
      setters: func_info SMap.t;
      calls: Type.t list;
    }

    type t = {
      id: ALoc.id;
      class_loc: ALoc.t;
      tparams: Type.typeparams;
      tparams_map: Type.t Subst_name.Map.t;
      super: super;
      (* Multiple function signatures indicates an overloaded constructor. Note that
         function signatures are stored in reverse definition order. *)
      constructor: func_info list;
      static: signature;
      instance: signature;
    }
  end
end

module Func_stmt_params_types : Param.S with module Config := Func_stmt_config_types.Types =
  Param.Make (Func_stmt_config_types.Types)

module Func_stmt_sig_types :
  Func.S
    with module Config := Func_stmt_config_types.Types
     and module Param := Func_stmt_params_types =
  Func.Make (Func_stmt_config_types.Types) (Func_stmt_params_types)

module Class_stmt_sig_types :
  Class.S
    with module Config := Func_stmt_config_types.Types
     and module Param := Func_stmt_params_types
     and module Func := Func_stmt_sig_types =
  Class.Make (Func_stmt_config_types.Types) (Func_stmt_params_types) (Func_stmt_sig_types)
