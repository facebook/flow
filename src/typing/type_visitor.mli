(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

class ['a] t: object
  (* Only exposing a few methods for now. *)
  method type_ : Context.t -> 'a -> Type.t -> 'a
  method tvar : Context.t -> 'a -> Reason.reason -> Constraint.ident -> 'a
  method props : Context.t -> 'a -> Type.Properties.id -> 'a
  method exports : Context.t -> 'a -> Type.Exports.id -> 'a
  method fun_type : Context.t -> 'a -> Type.funtype -> 'a
end
