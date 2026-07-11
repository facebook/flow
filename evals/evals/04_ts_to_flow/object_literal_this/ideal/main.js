/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

const flags: {
  enabled: {[k: string]: boolean},
  changes: number,
  set(name: string, on: boolean): boolean,
  isOn(name: string): boolean,
  activeCount(): number,
} = {
  enabled: {},
  changes: 0,

  set(name: string, on: boolean): boolean {
    const prev = flags.enabled[name] ?? false;
    if (prev !== on) {
      flags.enabled[name] = on;
      flags.changes += 1;
    }
    return prev;
  },

  isOn(name: string): boolean {
    return flags.enabled[name] ?? false;
  },

  activeCount(): number {
    let n = 0;
    for (const k of Object.keys(flags.enabled)) {
      if (flags.enabled[k]) {
        n += 1;
      }
    }
    return n;
  },
};

flags.set('new_ui', true);
flags.set('beta_search', true);
flags.set('beta_search', false);
console.log(
  `active=${flags.activeCount()} changes=${flags.changes} ui=${String(flags.isOn('new_ui'))}`,
);
