/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Severity {
  Info,
  Warning,
  Error,
}

export function formatSeverity(value: ?Severity): string {
  if (value == null) {
    return 'none';
  }
  return Severity.getName(value).toLowerCase();
}

export function severityRank(value: Severity | number): number {
  if (typeof value === 'number') {
    return value;
  }
  return Array.from(Severity.members()).indexOf(value);
}
