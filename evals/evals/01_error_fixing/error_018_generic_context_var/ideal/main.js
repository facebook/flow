/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

let latestPayload: unknown;

function dispatch<T>(event: {type: string, payload: T}): void {
  latestPayload = event.payload;
}

dispatch({type: 'click', payload: {x: 10, y: 20}});
dispatch({type: 'key', payload: 'Enter'});

console.log(latestPayload);
