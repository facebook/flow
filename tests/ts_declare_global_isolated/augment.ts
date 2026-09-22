/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export {};

declare global {
  interface GlobalBox {
    value: string;
  }

  const globalBox: GlobalBox;
}

declare global {
  interface GlobalBox {
    count: number;
  }

  declare class GlobalClass {
    field: string;
  }

  namespace GlobalNamespace {
    const member: boolean;
  }

  function globalOverload(value: string): string;
}

globalBox.value as string;
