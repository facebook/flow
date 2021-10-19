(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * From ocaml 4.12, libraries with any .ml modules will no longer
 * generates libfoo.a files.
 * Buck v1 is still expecting these, so the easiest workaround until
 * Buck v2 is to provide an empty module to trigger the generation
 * of libfoo.a
 *)
let () = ()
