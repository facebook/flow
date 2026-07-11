/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ApiError = {code: number, message: string};

type ApiResult<T, E = ApiError> =
  | {ok: true, value: T}
  | {ok: false, error: E};

type Page<TItem = string, TCursor = number> = {
  items: Array<TItem>,
  cursor: TCursor,
};

type StringResult = ApiResult<string>;

type DefaultPage = Page<>;

function describe(result: StringResult): string {
  return result.ok ? result.value : result.error.message;
}

function firstItem(page: DefaultPage): string {
  return page.items[0];
}

const ok: StringResult = {ok: true, value: 'hi'};
const page: DefaultPage = {items: ['a', 'b'], cursor: 0};

console.log(describe(ok), firstItem(page), page.cursor);
