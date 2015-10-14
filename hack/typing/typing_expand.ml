(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Gets rid of all the type variables,
 * this is only useful when declaring class constants.
 * The thing is, we don't want any type variable left in
 * type declarations, (it would force us to maintain a global
 * substitution, which would be way too big).
 *)
(*****************************************************************************)

let visitor = object
  inherit Type_mapper.deep_type_mapper
  inherit! Type_mapper.tvar_expanding_type_mapper
end

(*****************************************************************************)
(* External API *)
(*****************************************************************************)

let fully_expand env ty =
  snd (visitor#on_type (Type_mapper.fresh_env env) ty)
