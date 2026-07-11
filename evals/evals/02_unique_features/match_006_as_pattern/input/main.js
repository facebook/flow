/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type OkResponse = {status: 'ok', data: {items: Array<string>, total: number}};
type ErrResponse = {status: 'error', code: number, message: string};
type Response = OkResponse | ErrResponse | {status: 'loading'};

// TODO: Implement
