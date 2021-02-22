(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Trust

type ident = int

type node =
  | TrustGoto of ident
  | TrustRoot of root

and root = {
  rank: int;
  constraints: constraints;
}

and constraints =
  | TrustResolved of trust_qualifier
  | TrustUnresolved of bounds

and bounds = {
  mutable trust: trust_qualifier;
  mutable lowervars: ISet.t;
  mutable uppervars: ISet.t;
}

let get_constraints { constraints; _ } = constraints

let get_bounds { lowervars; uppervars; _ } = (lowervars, uppervars)

let get_trust { trust; _ } = trust

let resolved_trust_constraint _ trust = TrustResolved trust

let new_unresolved_root initial =
  TrustRoot
    {
      rank = 0;
      constraints =
        TrustUnresolved { trust = initial; lowervars = ISet.empty; uppervars = ISet.empty };
    }

let new_resolved_root trust = TrustRoot { rank = 0; constraints = TrustResolved trust }

let new_goto id = TrustGoto id

let set_trust bounds trust = bounds.trust <- trust

let extend_uppervars bounds new_uppervars =
  bounds.uppervars <- ISet.union bounds.uppervars new_uppervars

let extend_lowervars bounds new_lowervars =
  bounds.lowervars <- ISet.union bounds.lowervars new_lowervars
