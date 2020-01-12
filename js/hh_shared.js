/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//Provides: hh_counter_next
var __hh_counter = 0;
function hh_counter_next() {
  return ++__hh_counter;
}

//Provides: hh_check_should_exit
function hh_check_should_exit() {
  return;
}
