(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class ['a, 'phase] t :
  object
    (* Only exposing a few methods for now. *)
    method type_ : 'phase Context.t_ -> Polarity.t -> 'a -> Type.t -> 'a

    method def_type : 'phase Context.t_ -> Polarity.t -> 'a -> Type.def_t -> 'a

    method targ : 'phase Context.t_ -> Polarity.t -> 'a -> Type.targ -> 'a

    method use_type_ : 'phase Context.t_ -> 'a -> Type.use_t -> 'a

    method tvar : 'phase Context.t_ -> Polarity.t -> 'a -> Reason.reason -> Type.ident -> 'a

    method props : 'phase Context.t_ -> Polarity.t -> 'a -> Type.Properties.id -> 'a

    method prop : 'phase Context.t_ -> Polarity.t -> 'a -> Type.property -> 'a

    method call_prop : 'phase Context.t_ -> Polarity.t -> 'a -> int -> 'a

    method exports : 'phase Context.t_ -> Polarity.t -> 'a -> Type.Exports.id -> 'a

    method eval_id : 'phase Context.t_ -> Polarity.t -> 'a -> Type.Eval.id -> 'a

    method fun_type : 'phase Context.t_ -> Polarity.t -> 'a -> Type.funtype -> 'a

    method dict_type : 'phase Context.t_ -> Polarity.t -> 'a -> Type.dicttype -> 'a

    method destructor : 'phase Context.t_ -> 'a -> Type.destructor -> 'a
  end
