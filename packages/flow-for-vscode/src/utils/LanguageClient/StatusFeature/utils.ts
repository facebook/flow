/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {
  type ShowStatusParams,
  type StatusData,
  LspMessageType,
} from './types';

export function convertToStatus(params: ShowStatusParams): null | StatusData {
  const actions = params.actions || [];
  const buttons = actions.map((action) => action.title);
  switch (params.type) {
    case LspMessageType.Error:
      return {
        kind: 'red',
        message: params.message == null ? '' : params.message,
        buttons,
      };
    case LspMessageType.Warning:
      return {
        kind: 'yellow',
        message: params.message == null ? '' : params.message,
        shortMessage: params.shortMessage,
        progress:
          params.progress == null
            ? undefined
            : {
                numerator: params.progress.numerator,
                denominator: params.progress.denominator,
              },
        buttons,
      };
    case LspMessageType.Info:
      return { kind: 'green', message: params.message };
    default:
      return null;
  }
}

export class Defer<TValue> {
  promise: Promise<TValue>;
  // @ts-expect-error: not definitely initialized
  resolve: (value: TValue) => void;
  // @ts-expect-error: not definitely initialized
  reject: () => void;

  constructor() {
    this.promise = new Promise((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;
    });
  }
}
