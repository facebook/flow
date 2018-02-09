(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t
  val empty: t
  val append: t -> t -> t
end

module Unit: S with type t = unit = struct
  type t = unit
  let empty = ()
  let append _ _ = ()
end

module Any: S with type t = bool = struct
  type t = bool
  let empty = false
  let append = (||)
end
