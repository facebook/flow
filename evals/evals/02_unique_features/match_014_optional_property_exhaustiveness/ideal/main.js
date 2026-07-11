/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Config = {mode: 'dev' | 'prod', debug?: boolean};

export function describeConfig(config: Config): string {
  return match (config) {
    {mode: 'dev', debug: true} => 'Development with debugging enabled',
    {mode: 'dev', debug: false} => 'Development with debugging disabled',
    {mode: 'prod', debug: true} => 'Production with debugging enabled',
    {mode: 'prod', debug: false} => 'Production with debugging disabled',
    {mode: 'dev', ...} => 'Development (debug unset)',
    {mode: 'prod', ...} => 'Production (debug unset)',
  };
}
