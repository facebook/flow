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

function formatOk(ok: OkResponse): string {
  return `OK: ${ok.data.total} items`;
}

function formatErr(err: ErrResponse): string {
  return `Error ${err.code}: ${err.message}`;
}

export function summarize(response: Response): string {
  return match (response) {
    {status: 'ok', ...} as const ok => formatOk(ok),
    {status: 'error', ...} as const err => formatErr(err),
    {status: 'loading'} => "Loading...",
  };
}
