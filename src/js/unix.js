/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//Provides: caml_unix_getpid const
function caml_unix_getpid() {
  return 0;
}

//Provides: caml_unix_sleep const
function caml_unix_sleep() {
  return;
}

// TODO: remove once we fully migrate to OCaml 5.2
//Provides: unix_getpid const
function unix_getpid() {
  return;
}

// TODO: remove once we fully migrate to OCaml 5.2
//Provides: unix_sleep const
function unix_sleep() {
  return;
}
