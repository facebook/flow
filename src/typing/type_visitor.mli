(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class ['a] t :
  object
    (* Only exposing a few methods for now. *)
    method type_ : Context.t -> Polarity.t -> 'a -> Type.t -> 'a

    method def_type : Context.t -> Polarity.t -> 'a -> Type.def_t -> 'a

    method targ : Context.t -> Polarity.t -> 'a -> Type.targ -> 'a

    method use_type_ : Context.t -> 'a -> Type.use_t -> 'a

    method tvar : Context.t -> Polarity.t -> 'a -> Reason.reason -> Constraint.ident -> 'a

    method props : Context.t -> Polarity.t -> 'a -> Type.Properties.id -> 'a

    method prop : Context.t -> Polarity.t -> 'a -> Type.property -> 'a

    method call_prop : Context.t -> Polarity.t -> 'a -> int -> 'a

    method exports : Context.t -> Polarity.t -> 'a -> Type.Exports.id -> 'a

    method eval_id : Context.t -> Polarity.t -> 'a -> int -> 'a

    method fun_type : Context.t -> Polarity.t -> 'a -> Type.funtype -> 'a

    method dict_type : Context.t -> Polarity.t -> 'a -> Type.dicttype -> 'a

    method destructor : Context.t -> 'a -> Type.destructor -> 'a
  end
