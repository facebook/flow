(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type object_source =
  | ObjectType of Type.t
  | ObjectRequireLoc of ALoc.t

(* source loc *)

type member_info = {
  prop_name: string;
  object_source: object_source;
}

type t =
  | Identifier of {
      name: string;
      loc: ALoc.t;
      type_: Type.t;
    }
  | Type of Type.t
  | Typeof of Type.t
  | Member of member_info
  | Require of (ALoc.t * string) * ALoc.t
  | JsxAttribute of {
      component_t: Type.t;
      name: string;
      loc: ALoc.t;
    }
