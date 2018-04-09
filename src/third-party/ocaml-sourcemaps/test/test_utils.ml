(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
(* open Sourcemaps *)

let assert_equal_sourcemaps ~ctxt expected actual =
  assert_equal ~ctxt ~msg:"Versions not equal"
    (Sourcemap.version expected) (Sourcemap.version actual);
  assert_equal ~ctxt ~msg:"Sources not equal" ~printer:(String.concat "; ")
    (Sourcemap.sources expected) (Sourcemap.sources actual);
  assert_equal ~ctxt ~msg:"Source root not equal"
    (Sourcemap.source_root expected) (Sourcemap.source_root actual);
  assert_equal ~ctxt ~msg:"Names not equal"
    (Sourcemap.names expected) (Sourcemap.names actual);
  assert_equal ~ctxt ~msg:"Mappings not equal" ~printer:(fun x -> x)
    (Sourcemap.string_of_mappings expected) (Sourcemap.string_of_mappings actual);
  assert_equal ~ctxt ~msg:"Source content not equal"
    (Sourcemap.sources_contents expected) (Sourcemap.sources_contents actual);
  assert_equal ~ctxt expected actual
