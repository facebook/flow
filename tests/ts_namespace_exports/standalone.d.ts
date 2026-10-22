/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare const implementation: (s: string) => number;
type Local = number;

declare namespace API {
  export { implementation as call };
  export type { Local as Item };
  export { Local as PlainItem };
  export { type Local as InlineItem, implementation as mixedCall };
  export const value: number;
  export let mutable: string;
  export var variable: boolean;
  export class Member {
    readonly field: number;
    method(s: string): number;
    static make(s: string): Member;
  }
  export function helper(s: string): number;
}

declare namespace Types {
  type Local = string;
  export type { Local as Item };
  export { type Local as InlineItem };
}

export { API, Types };

export declare namespace Shadow {
  type Local = string;
  export { type Local as Item };
  export { type Later as ForwardItem, Later as ImplicitForwardItem, laterValue as forwardCall };
  type Later = boolean;
  declare const laterValue: (s: string) => number;
}
