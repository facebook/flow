(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Hint_api
open Reason
open Loc_collections

type cond_context =
  | NonConditionalContext
  | OtherConditionalTest

type scope_kind =
  | Ordinary (* function or module *)
  | Async (* async function *)
  | Generator (* generator function *)
  | AsyncGenerator (* async generator function *)
  | Module (* module scope *)
  | Global (* global scope *)
  | Predicate (* predicate function *)
  | Ctor
(* constructor function *)

type class_stack = ALoc.t list

type for_kind =
  | In
  | Of of { await: bool }

(* A map from location of tparam to name. *)
type tparams_map = string ALocMap.t

type hint_node =
  | AnnotationHint of tparams_map * (ALoc.t, ALoc.t) Ast.Type.annotation
  | ValueHint of (ALoc.t, ALoc.t) Ast.Expression.t
  | ProvidersHint of ALoc.t Nel.t
  | WriteLocHint of Env_api.With_ALoc.def_loc_type * ALoc.t
  | StringLiteralType of string
  | BuiltinType of string

type ast_hints =
  ( hint_node,
    (ALoc.t, ALoc.t) Ast.Expression.CallTypeArgs.t option,
    (ALoc.t, ALoc.t) Ast.Expression.ArgList.t,
    (ALoc.t, ALoc.t) Ast.JSX.Opening.attribute list,
    ALoc.t * (ALoc.t, ALoc.t) Ast.JSX.child list
  )
  hint
  list

type function_synth_kind =
  | FunctionSynthesizable
  | FunctionPredicateSynthesizable of ALoc.t * (ALoc.t, ALoc.t) Ast.Expression.t
  | MissingReturn of ALoc.t

type object_missing_annot =
  | FuncMissingAnnot of ALoc.t
  | OtherMissingAnnot of ALoc.t

type object_synth_kind =
  | ObjectSynthesizable of {
      (* A set of this write locations that can be resolved by resolving the object. *)
      this_write_locs: Env_api.EnvSet.t;
    }
  | MissingMemberAnnots of { locs: object_missing_annot Nel.t }
  | Unsynthesizable

type root =
  | Annotation of {
      tparams_map: tparams_map;
      optional: bool;
      has_default_expression: bool;
      param_loc: ALoc.t option;
      annot: (ALoc.t, ALoc.t) Ast.Type.annotation;
    }
  | Value of {
      hints: ast_hints;
      expr: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | FunctionValue of {
      hints: ast_hints;
      synthesizable_from_annotation: function_synth_kind;
      function_loc: ALoc.t;
      function_: (ALoc.t, ALoc.t) Ast.Function.t;
      statics: Env_api.EnvKey.t SMap.t;
      arrow: bool;
      tparams_map: tparams_map;
    }
  | ObjectValue of {
      synthesizable: object_synth_kind;
      obj_loc: ALoc.t;
      obj: (ALoc.t, ALoc.t) Ast.Expression.Object.t;
    }
  | EmptyArray of {
      array_providers: ALocSet.t;
      arr_loc: ALoc.t;
    }
  | Contextual of {
      reason: Reason.reason;
      hints: ast_hints;
      optional: bool;
      default_expression: (ALoc.t, ALoc.t) Ast.Expression.t option;
    }
  | CatchUnannotated
  | For of for_kind * (ALoc.t, ALoc.t) Ast.Expression.t

type selector =
  | Elem of {
      index: int;
      has_default: bool;
    }
  | Prop of {
      prop: string;
      prop_loc: ALoc.t;
      has_default: bool;
    }
  | Computed of {
      expression: (ALoc.t, ALoc.t) Ast.Expression.t;
      has_default: bool;
    }
  | ObjRest of {
      used_props: string list;
      after_computed: bool;
    }
  | ArrRest of int
  | Default

type binding =
  | Root of root
  | Select of {
      selector: selector;
      parent: ALoc.t * binding;
    }

type import =
  | Named of {
      kind: Ast.Statement.ImportDeclaration.import_kind option;
      remote: string;
      remote_loc: ALoc.t;
      local: string;
    }
  | Namespace
  | Default of string

type generator_annot = {
  tparams_map: tparams_map;
  return_annot: (ALoc.t, ALoc.t) Ast.Type.annotation;
  async: bool;
}

type class_implicit_this_tparam = {
  tparams_map: tparams_map;
  class_tparams_loc: ALoc.t option;
}

type expression_def = {
  cond_context: cond_context;
  chain: bool;
  expr: (ALoc.t, ALoc.t) Ast.Expression.t;
  hints: ast_hints;
}

type def =
  | Binding of binding
  | ExpressionDef of expression_def
  | MemberAssign of {
      member_loc: ALoc.t;
      member: (ALoc.t, ALoc.t) Ast.Expression.Member.t;
      rhs: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | OpAssign of {
      exp_loc: ALoc.t;
      lhs: (ALoc.t, ALoc.t) Ast.Pattern.t;
      op: Ast.Expression.Assignment.operator;
      rhs: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | Update of {
      exp_loc: ALoc.t;
      op: Ast.Expression.Update.operator;
    }
  | Function of {
      hints: ast_hints;
      synthesizable_from_annotation: function_synth_kind;
      arrow: bool;
      has_this_def: bool;
      function_loc: ALoc.t;
      function_: (ALoc.t, ALoc.t) Ast.Function.t;
      statics: Env_api.EnvKey.t SMap.t;
      tparams_map: tparams_map;
    }
  | Class of {
      class_implicit_this_tparam: class_implicit_this_tparam;
      class_: (ALoc.t, ALoc.t) Ast.Class.t;
      class_loc: ALoc.t;
      (* A set of this and super write locations that can be resolved by resolving the class. *)
      this_super_write_locs: Env_api.EnvSet.t;
    }
  | DeclaredClass of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.DeclareClass.t
  | TypeAlias of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.TypeAlias.t
  | OpaqueType of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.OpaqueType.t
  | TypeParam of tparams_map * (ALoc.t, ALoc.t) Ast.Type.TypeParam.t
  | Interface of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.Interface.t
  | Enum of ALoc.t * ALoc.t Ast.Statement.EnumDeclaration.body
  | Import of {
      import_kind: Ast.Statement.ImportDeclaration.import_kind;
      import: import;
      source: string;
      source_loc: ALoc.t;
    }
  | GeneratorNext of generator_annot option
  | DeclaredModule of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.DeclareModule.t
  | NonBindingParam
  | MissingThisAnnot

module Print = struct
  open Utils_js

  let string_of_root = function
    | Contextual _ -> "contextual"
    | EmptyArray _ -> "[]"
    | CatchUnannotated -> "unannotated catch param"
    | Annotation { annot = (loc, _); _ } -> spf "annot %s" (ALoc.debug_to_string loc)
    | Value { expr = (loc, _); _ } -> spf "val %s" (ALoc.debug_to_string loc)
    | FunctionValue { function_loc; _ } -> spf "function val %s" (ALoc.debug_to_string function_loc)
    | ObjectValue _ -> "object"
    | For (In, (loc, _)) -> spf "for in %s" (ALoc.debug_to_string loc)
    | For (Of _, (loc, _)) -> spf "for of %s" (ALoc.debug_to_string loc)

  let string_of_selector = function
    | Elem { index; _ } -> spf "[%d]" index
    | Prop { prop; _ } -> spf ".%s" prop
    | Computed _ -> ".[computed]"
    | ObjRest _ -> "{ ... }"
    | ArrRest _ -> "[...]"
    | Default -> "<with default>"

  let rec string_of_binding = function
    | Root r -> string_of_root r
    | Select { selector; parent = (_, binding); _ } ->
      spf "(%s)%s" (string_of_binding binding) (string_of_selector selector)

  let string_of_import_kind =
    let open Ast.Statement.ImportDeclaration in
    function
    | ImportTypeof -> "typeof "
    | ImportType -> "type "
    | ImportValue -> ""

  let string_of_import = function
    | Named { kind; remote; local = _; remote_loc = _ } ->
      spf "%s%s" (Base.Option.value_map ~f:string_of_import_kind ~default:"" kind) remote
    | Namespace -> "namespace"
    | Default _ -> "default"

  let on_hint = function
    | AnnotationHint _ -> "annot hint"
    | ValueHint _ -> "value hint"
    | ProvidersHint _ -> "providers hint"
    | WriteLocHint _ -> "write loc hint"
    | StringLiteralType s -> "string literal hint: " ^ s
    | BuiltinType _ -> "builtin type hint"

  let string_of_source = function
    | Binding b -> string_of_binding b
    | ExpressionDef { expr = (expr_loc, _); hints; _ } ->
      spf "exp %s (hint = %s)" (ALoc.debug_to_string expr_loc) (string_of_hints ~on_hint hints)
    | Update _ -> "[in/de]crement"
    | MemberAssign _ -> "member_assign"
    | OpAssign _ -> "opassign"
    | Function { function_ = { Ast.Function.id; _ }; _ } ->
      spf
        "fun %s"
        (Base.Option.value_map
           ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
           ~default:"<anonymous>"
           id
        )
    | DeclaredClass (_, { Ast.Statement.DeclareClass.id = (_, { Ast.Identifier.name; _ }); _ }) ->
      spf "declared class %s" name
    | Class
        {
          class_ = { Ast.Class.id; _ };
          class_implicit_this_tparam = _;
          class_loc = _;
          this_super_write_locs = _;
        } ->
      spf
        "class %s"
        (Base.Option.value_map
           ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
           ~default:"<anonymous>"
           id
        )
    | TypeAlias (_, { Ast.Statement.TypeAlias.right = (loc, _); _ }) ->
      spf "alias %s" (ALoc.debug_to_string loc)
    | OpaqueType (_, { Ast.Statement.OpaqueType.id = (loc, _); _ }) ->
      spf "opaque %s" (ALoc.debug_to_string loc)
    | TypeParam (_, (loc, _)) -> spf "tparam %s" (ALoc.debug_to_string loc)
    | Enum (loc, _) -> spf "enum %s" (ALoc.debug_to_string loc)
    | Interface _ -> "interface"
    | DeclaredModule _ -> "module"
    | GeneratorNext _ -> "next"
    | Import { import_kind; source; import; source_loc = _ } ->
      spf "import %s%s from %s" (string_of_import_kind import_kind) (string_of_import import) source
    | NonBindingParam -> "nonbinding_param"
    | MissingThisAnnot -> "this (missing)"
end

type env_entries_map = (def * scope_kind * class_stack * ALoc.t virtual_reason) Env_api.EnvMap.t

type hint_map = ast_hints ALocMap.t
