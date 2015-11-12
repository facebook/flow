(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Various constants and the like; normally copied from HHVM *)

open Core
open Utils

(* the enum values from hphp/runtime/base/header-kind.h;
 * XXX: this is expedient but we will need to do something better to
 * keep this synced eventually *)
let header_kinds = [
  "Packed"; "Struct"; "Mixed"; "Empty"; "Apc"; "Globals"; "Proxy"; "String";
  "Resource"; "Ref"; "Object"; "WaitHandle"; "ResumableObj"; "AwaitAllWH";
  "Vector"; "Map"; "Set"; "Pair"; "ImmVector"; "ImmMap"; "ImmSet";
  "ResumableFrame"; "NativeData";
  "SmallMalloc"; "BigMalloc"; "BigObj"; "Free"; "Hole"
]
let header_kind_values = List.foldi
  ~f:(fun i m kind -> SMap.add kind i m) ~init:SMap.empty header_kinds

(* HHVM has a lot of automatic aliases for types, functions, and classes.
 * Most are just automatically pulling things in from HH. *)
(* XXX: this is expedient but we will need to do something better to
 * keep this synced eventually *)
let aliases_to_hh = [
  (* From Parser::onCall in hphp/compiler/parser/parser.cpp *)
  "fun";
  "meth_caller";
  "class_meth";
  "inst_meth";
  "invariant_callback_register";
  "invariant";
  "invariant_violation";
  "idx";
  "asio_get_current_context_idx";
  "asio_get_running_in_context";
  "asio_get_running";
  "xenon_get_data";
  "objprof_get_strings";
  "objprof_get_data";
  "objprof_get_paths";
  "objprof_start";
  "server_warmup_status";
  (* From Parser::getAutoAliasedClassName in hphp/compiler/parser/parser.cpp *)
  "AsyncIterator";
  "AsyncKeyedIterator";
  "Traversable";
  "Container";
  "KeyedTraversable";
  "KeyedContainer";
  "Iterator";
  "KeyedIterator";
  "Iterable";
  "KeyedIterable";
  "Collection";
  "Vector";
  "Map";
  "Set";
  "Pair";
  "ImmVector";
  "ImmMap";
  "ImmSet";
  "InvariantException";
  "IMemoizeParam";
  "Shapes";
  "Awaitable";
  "AsyncGenerator";
  "WaitHandle";
  "StaticWaitHandle";
  "WaitableWaitHandle";
  "ResumableWaitHandle";
  "AsyncFunctionWaitHandle";
  "AsyncGeneratorWaitHandle";
  "AwaitAllWaitHandle";
  "GenArrayWaitHandle";
  "GenMapWaitHandle";
  "GenVectorWaitHandle";
  "ConditionWaitHandle";
  "RescheduleWaitHandle";
  "SleepWaitHandle";
  "bool";
  "int";
  "float";
  "num";
  "arraykey";
  "string";
  "resource";
  "mixed";
  "noreturn";
  "void";
  "this";
]

let aliases =
  map_of_list @@
  List.map ~f:(fun x -> x, "HH\\" ^ x) aliases_to_hh @ [
  (* From Parser::getAutoAliasedClassName in hphp/compiler/parser/parser.cpp *)
  "classname", "HH\\string";
  "boolean", "HH\\bool";
  "integer", "HH\\int";
  "double", "HH\\float";
  "real", "HH\\float";
  (* These are not defined as aliases in hhvm but I think might as well be.
   * From EmitterVisitor::emitSystemLibVarEnvFunc
   * in hphp/compiler/analysis/emitter.cpp *)
  "extract", "__SystemLib\\extract";
  "assert", "__SystemLib\\assert";
  "parse_str", "__SystemLib\\parse_str";
  "compact", "__SystemLib\\compact_sl";
  "get_defined_vars", "__SystemLib\\get_defined_vars";
  "func_get_args", "__SystemLib\\func_get_args_sl";
  "func_get_arg", "__SystemLib\\func_get_arg_sl";
  "func_num_args", "__SystemLib\\func_num_arg_";
  ]


module XHPConsts = struct
  (* From https://github.com/facebook/xhp-lib/blob/master/src/core/ReflectionXHPAttribute.php *)
  let cTYPE_STRING = 1
  let cTYPE_BOOL = 2
  let cTYPE_INTEGER = 3
  let cTYPE_ARRAY = 4
  let cTYPE_OBJECT = 5
  let cTYPE_VAR = 6
  let cTYPE_ENUM = 7
  let cTYPE_FLOAT = 8
  let cTYPE_UNSUPPORTED_LEGACY_CALLABLE = 9
end
