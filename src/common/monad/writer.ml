(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type m

  type 'a t = 'a * m

  include Monad.S with type 'a t := 'a t

  val tell : m -> unit t

  val listen : 'a t -> ('a * m) t
end

module Make (M : Monoid.S) : S with type m := M.t = struct
  type m = M.t

  type 'a t = 'a * m

  let map w ~f =
    let (x, m) = w in
    (f x, m)

  include Monad.Make (struct
    type 'a z = 'a t (* 4.02.1 doesn't have nonrec *)

    type 'a t = 'a z

    let return x = (x, M.empty)

    let bind w f =
      let (x, m) = w in
      let (x, m') = f x in
      (x, M.append m m')

    let map = `Custom map
  end)

  let tell m = ((), m)

  let listen (x, s) = ((x, s), s)
end
