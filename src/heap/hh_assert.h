/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef HH_ASSERT_H
#define HH_ASSERT_H

/**
 * Concatenate the __LINE__ and __FILE__ strings in a macro.
 */
#define S1(x) #x
#define S2(x) S1(x)
#define LOCATION __FILE__ " : " S2(__LINE__)
#define assert(f) ((f) ? 0 : caml_failwith(LOCATION))
#endif
