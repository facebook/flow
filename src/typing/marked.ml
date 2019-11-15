(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Polarity

module type S = sig
  type t

  type key

  val empty : t

  val add : key -> Polarity.t -> t -> (Polarity.t * t) option

  val get : key -> t -> Polarity.t option

  val mem : key -> Polarity.t -> t -> bool

  val exclude : key -> t -> t
end

module Make (Key : Map.OrderedType) : S with type key = Key.t = struct
  module Map = WrappedMap.Make (Key)

  type t = Polarity.t Map.t

  type key = Map.key

  let empty = Map.empty

  let add id p x =
    match Map.find_opt id x with
    | None -> Some (p, Map.add id p x)
    | Some p' ->
      (match (p, p') with
      | (Positive, Negative)
      | (Negative, Positive) ->
        Some (p, Map.add id Neutral x)
      | (Neutral, Negative) -> Some (Positive, Map.add id p x)
      | (Neutral, Positive) -> Some (Negative, Map.add id p x)
      | _ -> None)

  let get = Map.find_opt

  let mem id p x =
    match Map.find_opt id x with
    | None -> false
    | Some p' -> Polarity.compat (p', p)

  let exclude id x = Map.add id Neutral x
end

module IdMarked = Make (IntKey)
