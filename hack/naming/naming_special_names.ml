(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(** Module consisting of the special names known to the typechecker *)

module Classes = struct

  let cParent = "parent"
  let cStatic = "static"
  let cSelf   = "self"
  let cUnknown = "\\*Unknown*" (* Used for dynamic classnames, e.g. new $foo(); *)

  let cAwaitable = "\\Awaitable"
  let cWaitHandle = "\\WaitHandle"
  let cGenerator = "\\Generator"
  let cAsyncGenerator = "\\AsyncGenerator"
  let cFormatString = "\\FormatString"

  let cHH_BuiltinEnum = "\\HH\\BuiltinEnum"

  let cException = "\\Exception"
  let cStdClass = "\\stdClass"

  let cAsyncIterator = "\\AsyncIterator"
  let cAsyncKeyedIterator = "\\AsyncKeyedIterator"

  let cStringish = "\\Stringish"
  let cXHPChild = "\\XHPChild"
end

module Collections = struct

  (* concrete classes *)
  let cVector    = "\\Vector"
  let cImmVector = "\\ImmVector"
  let cSet       = "\\Set"
  let cImmSet    = "\\ImmSet"
  let cMap       = "\\Map"
  let cStableMap = "\\StableMap"
  let cImmMap    = "\\ImmMap"
  let cPair      = "\\Pair"

  (* interfaces *)
  let cIterator         = "\\Iterator"
  let cKeyedIterator    = "\\KeyedIterator"
  let cContainer        = "\\Container"
  let cKeyedContainer   = "\\KeyedContainer"
  let cTraversable      = "\\Traversable"
  let cKeyedTraversable = "\\KeyedTraversable"
  let cIterable         = "\\Iterable"
  let cKeyedIterable    = "\\KeyedIterable"
  let cIndexish         = "\\Indexish"

  let cCollection       = "\\Collection"
  let cConstVector      = "\\ConstVector"
  let cConstMap         = "\\ConstMap"

end

module Members = struct

  let mClass       = "class"

  let __construct  = "__construct"
  let __destruct   = "__destruct"
  let __call       = "__call"
  let __callStatic = "__callStatic"
  let __toString   = "__toString"
  let __set        = "__set"
  let __isset      = "__isset"
  let __get        = "__get"
  let __unset      = "__unset"

end

module UserAttributes = struct

  let uaOverride            = "__Override"
  let uaConsistentConstruct = "__ConsistentConstruct"
  let uaUnsafeConstruct    = "__UNSAFE_Construct"

end

(* Tested before \\-prepending name-canonicalization *)
module SpecialFunctions = struct

  let tuple          = "tuple"          (* pseudo-function *)
  let echo           = "echo"           (* pseudo-function *)
  let assert_        = "assert"

  let invariant      = "invariant"
  let invariant_violation = "invariant_violation"

  let fun_           = "fun"
  let inst_meth      = "inst_meth"
  let class_meth     = "class_meth"
  let meth_caller    = "meth_caller"

  let call_user_func = "call_user_func"

end

module PseudoFunctions = struct

  let empty = "\\empty"
  let isset = "\\isset"
  let unset = "\\unset"
  let exit_ = "\\exit"
  let die = "\\die"
  let hh_show = "\\hh_show"

end

module StdlibFunctions = struct

  let is_int      = "\\is_int"
  let is_bool     = "\\is_bool"
  let is_array    = "\\is_array"
  let is_float    = "\\is_float"
  let is_string   = "\\is_string"
  let is_null     = "\\is_null"
  let is_resource = "\\is_resource"

  let get_class = "\\get_class"

  let array_filter = "\\array_filter"
  let array_map = "\\array_map"

end

module Typehints = struct

  let void     = "void"
  let num      = "num"
  let resource = "resource"
  let arraykey = "arraykey"
  let mixed    = "mixed"
  let this     = "this"

  let int     = "int"
  let bool    = "bool"
  let float   = "float"
  let string  = "string"
  let array   = "array"
  let integer = "integer"
  let boolean = "boolean"
  let double  = "double"
  let real    = "real"

  let shape = "shape"

  let object_cast = "object"
  let unset_cast = "unset"

end

module PseudoConsts = struct

  let g__LINE__      = "__LINE__"
  let g__CLASS__     = "__CLASS__"
  let g__TRAIT__     = "__TRAIT__"
  let g__FILE__      = "__FILE__"
  let g__DIR__       = "__DIR__"
  let g__FUNCTION__  = "__FUNCTION__"
  let g__METHOD__    = "__METHOD__"
  let g__NAMESPACE__ = "__NAMESPACE__"

end

module FB = struct

  let cEnum                  = "\\Enum"
  let cUncheckedEnum         = "\\UncheckedEnum"

  let cGenReadApi            = "\\GenReadApi"
  let cGenReadIdxApi         = "\\GenReadIdxApi"

  let cPrivacyPolicy         = "\\PrivacyPolicy"
  let cPrivacyPolicyBase     = "\\PrivacyPolicyBase"
  let cPrivacyPolicyBaseBase = "\\PrivacyPolicyBaseBase"
  let cDataType              = "\\DataType"
  let cDataTypeImplProvider  = "\\DataTypeImplProvider"

  let cDynamicYield          = "\\DynamicYield"
  let cIUseDynamicYield      = "\\IUseDynamicYield"

  let fgena                        = "gena"
  let fgenva                       = "genva"
  let fgen_array_rec               = "gen_array_rec"
  let fgen_array_va_rec_DEPRECATED = "gen_array_va_rec_DEPRECATED"

end
