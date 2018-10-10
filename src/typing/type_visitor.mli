(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class ['a] t: object
  (* Only exposing a few methods for now. *)
  method type_ : Context.t -> Type.polarity -> 'a -> Type.t -> 'a
  method def_type : Context.t -> Type.polarity -> 'a -> Type.def_t -> 'a
  method targ : Context.t -> Type.polarity -> 'a -> Type.targ -> 'a
  method use_type_ : Context.t -> 'a -> Type.use_t -> 'a
  method tvar : Context.t -> Type.polarity -> 'a -> Reason.reason -> Constraint.ident -> 'a
  method props : Context.t -> Type.polarity -> 'a -> Type.Properties.id -> 'a
  method prop : Context.t -> Type.polarity -> 'a -> Type.property -> 'a
  method call_prop : Context.t -> Type.polarity -> 'a -> int -> 'a
  method exports : Context.t -> Type.polarity -> 'a -> Type.Exports.id -> 'a
  method eval_id : Context.t -> Type.polarity -> 'a -> int -> 'a
  method fun_type : Context.t -> Type.polarity -> 'a -> Type.funtype -> 'a
  method dict_type : Context.t -> Type.polarity -> 'a -> Type.dicttype -> 'a
  method destructor: Context.t -> 'a -> Type.destructor -> 'a
end
