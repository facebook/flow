(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | PartialClassicDepGraph of Utils_js.FilenameSet.t Utils_js.FilenameMap.t
  | PartialTypesFirstDepGraph of
      (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Utils_js.FilenameMap.t
