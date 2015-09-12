(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result =
  (Pos.absolute, HackSearchService.search_result_type) SearchUtils.term list

let go query type_ =
  let results = HackSearchService.MasterApi.query query type_ in
  List.map SearchUtils.to_absolute results
