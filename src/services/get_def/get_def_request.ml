(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('M, 'T) object_source =
  | ObjectType of 'T
  | ObjectRequireLoc of 'M

(* source loc *)

type ('M, 'T) member_info = {
  prop_name: string;
  object_source: ('M, 'T) object_source;
}

type ('M, 'T) t =
  | Identifier of {
      name: string;
      loc: 'T;
    }
  | Type of 'T
  | Typeof of 'T
  | Member of ('M, 'T) member_info
  | Require of ('M * string) * 'M
  | JsxAttribute of {
      component_t: 'T;
      name: string;
      loc: 'M;
    }
