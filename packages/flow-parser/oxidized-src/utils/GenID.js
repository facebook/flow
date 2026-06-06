/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

const genPrefix = '$$gen$';

export default class GenID {
  genN: number = 0;
  +used: Set<string> = new Set();
  +prefix: string;

  constructor(uniqueTransformPrefix: string) {
    this.prefix = `${genPrefix}${uniqueTransformPrefix}`;
  }

  id(): string {
    let name;
    do {
      name = `${this.prefix}${this.genN}`;
      this.genN++;
    } while (this.used.has(name));
    this.used.add(name);
    return name;
  }

  addUsage(name: string): void {
    if (name.startsWith(this.prefix)) {
      this.used.add(name);
    }
  }
}
