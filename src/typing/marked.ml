(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Type

type t = polarity IMap.t

let empty = IMap.empty

let add id p x =
  match IMap.get id x with
  | None -> Some (p, IMap.add id p x)
  | Some p' ->
      match p, p' with
    | Positive, Negative
    | Negative, Positive -> Some (p, IMap.add id Neutral x)
    | Neutral, Negative -> Some (Positive, IMap.add id p x)
    | Neutral, Positive -> Some (Negative, IMap.add id p x)
    | _ -> None

let get = IMap.get

let mem id p x =
  match IMap.get id x with
  | None -> false
  | Some p' -> Polarity.compat (p', p)

let exclude id x = IMap.add id Neutral x
