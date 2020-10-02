/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/fail.h>

void raise_assertion_failure(char *msg) {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("c_assertion_failure");
  caml_raise_with_string(*exn, msg);
}
