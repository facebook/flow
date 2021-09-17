/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */


//Provides: caml_fast_generic_compare
//Requires: caml_compare
function caml_fast_generic_compare(x,y){
    return caml_compare(x,y);
  }