(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include Str

let _memo_compiled_regexp = Hashtbl.create 101

let match_func s re =
  let compile_re =
    Fun.memoized _memo_compiled_regexp re (fun () -> Str.regexp re) in
  Str.string_match compile_re s 0

let (=~) s re =
  match_func s re
