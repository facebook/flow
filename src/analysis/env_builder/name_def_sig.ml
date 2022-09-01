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

type root =
  | Annotation of {
      tparams_map: tparams_map;
      optional: bool;
      default_expression: (ALoc.t, ALoc.t) Ast.Expression.t option;
      is_assignment: bool;
      annot: (ALoc.t, ALoc.t) Ast.Type.annotation;
    }
  | Value of {
      hint: hint_node hint;
      expr: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | EmptyArray of {
      array_providers: ALocSet.t;
      arr_loc: ALoc.t;
    }
  | Contextual of {
      reason: Reason.reason;
      hint: hint_node hint;
      optional: bool;
      default_expression: (ALoc.t, ALoc.t) Ast.Expression.t option;
    }
  | Catch
  | For of for_kind * (ALoc.t, ALoc.t) Ast.Expression.t

type selector =
  | Elem of int
  | Prop of {
      prop: string;
      prop_loc: ALoc.t;
      has_default: bool;
    }
  | Computed of (ALoc.t, ALoc.t) Ast.Expression.t
  | ObjRest of {
      used_props: string list;
      after_computed: bool;
    }
  | ArrRest of int
  | Default

type default =
  | DefaultExpr of (ALoc.t, ALoc.t) Ast.Expression.t
  | DefaultCons of (ALoc.t, ALoc.t) Ast.Expression.t * default
  | DefaultSelector of default * selector

type binding =
  | Root of root
  | Select of {
      selector: selector;
      default: default option;
      binding: binding;
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

type def =
  | Binding of binding
  | ChainExpression of cond_context * (ALoc.t, ALoc.t) Ast.Expression.t
  | RefiExpression of (ALoc.t, ALoc.t) Ast.Expression.t
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
      hint: hint_node hint;
      synthesizable_from_annotation: bool;
      has_this_def: bool;
      function_loc: ALoc.t;
      function_: (ALoc.t, ALoc.t) Ast.Function.t;
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

module Print = struct
  open Utils_js

  let string_of_root = function
    | Contextual _ -> "contextual"
    | EmptyArray _ -> "[]"
    | Catch -> "catch"
    | Annotation { annot = (loc, _); _ } -> spf "annot %s" (ALoc.debug_to_string loc)
    | Value { expr = (loc, _); _ } -> spf "val %s" (ALoc.debug_to_string loc)
    | For (In, (loc, _)) -> spf "for in %s" (ALoc.debug_to_string loc)
    | For (Of _, (loc, _)) -> spf "for of %s" (ALoc.debug_to_string loc)

  let string_of_selector = function
    | Elem n -> spf "[%d]" n
    | Prop { prop; _ } -> spf ".%s" prop
    | Computed _ -> ".[computed]"
    | ObjRest _ -> "{ ... }"
    | ArrRest _ -> "[...]"
    | Default -> "<with default>"

  let rec string_of_binding = function
    | Root r -> string_of_root r
    | Select { selector; binding; _ } ->
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

  let string_of_source = function
    | Binding b -> string_of_binding b
    | ChainExpression _ -> spf "heap"
    | RefiExpression _ -> spf "exp"
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
end

type map = (def * scope_kind * class_stack * ALoc.t virtual_reason) Env_api.EnvMap.t
