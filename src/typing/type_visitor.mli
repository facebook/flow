(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class ['a] t: object
  (* Only exposing a few methods for now. *)
  method type_ : Context.t -> 'a -> Type.t -> 'a
  method def_type : Context.t -> 'a -> Type.def_t -> 'a
  method use_type_ : Context.t -> 'a -> Type.use_t -> 'a
  method tvar : Context.t -> 'a -> Reason.reason -> Constraint.ident -> 'a
  method props : Context.t -> 'a -> Type.Properties.id -> 'a
  method exports : Context.t -> 'a -> Type.Exports.id -> 'a
  method eval_id : Context.t -> 'a -> int -> 'a
  method fun_type : Context.t -> 'a -> Type.funtype -> 'a
  method dict_type : Context.t -> 'a -> Type.dicttype -> 'a
end
