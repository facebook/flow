/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

function foo(numVal?) {}
function foo(numVal? = 2) {}
function foo(numVal: number) {}
function foo(numVal?: number) {}
function foo(numVal: number = 2) {}
function foo(numVal?: number = 2) {}
