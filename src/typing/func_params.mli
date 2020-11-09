(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Func_params_intf

module Make (C : Config) :
  S
    with type 'T ast = 'T C.ast
     and type 'T param_ast = 'T C.param_ast
     and type 'T rest_ast = 'T C.rest_ast
     and type 'T this_ast = 'T C.this_ast
     and type param = C.param
     and type rest = C.rest
     and type this_param = C.this_param
