/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type StatusConfig = {
  pending: 'PENDING',
  processing: 'PROCESSING',
  completed: 'COMPLETED',
  failed: 'FAILED',
};

type StatusKey = keyof StatusConfig;
type StatusValue = StatusConfig[keyof StatusConfig];

type Validators<T> = {
  [K in keyof T]: (value: T[K]) => boolean,
};

function makeStatusValidators(): Validators<StatusConfig> {
  return {
    pending: (v) => v === 'PENDING',
    processing: (v) => v === 'PROCESSING',
    completed: (v) => v === 'COMPLETED',
    failed: (v) => v === 'FAILED',
  };
}

class ConfigStore<T extends {readonly [key: string]: unknown}> {
  config: T;

  constructor(config: T) {
    this.config = config;
  }

  get<K extends keyof T>(key: K): T[K] {
    return this.config[key];
  }

  has(key: keyof T): boolean {
    return key in this.config;
  }

  keys(): Array<keyof T> {
    return Object.keys(this.config);
  }
}

const statuses: StatusConfig = {
  pending: 'PENDING',
  processing: 'PROCESSING',
  completed: 'COMPLETED',
  failed: 'FAILED',
};

const store = new ConfigStore<StatusConfig>(statuses);
const pendingValue: StatusValue = store.get('pending');
const allKeys: Array<StatusKey> = store.keys();
const validators = makeStatusValidators();

console.log(pendingValue, allKeys, validators.pending('PENDING'));
