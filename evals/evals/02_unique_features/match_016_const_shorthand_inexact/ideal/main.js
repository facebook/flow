/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type User = {name: string, role: 'admin' | 'editor' | 'viewer', ...};

export function greet(user: User): string {
  return match (user) {
    {role: 'admin', const name, ...} => `Welcome back, Admin ${name}!`,
    {role: 'editor', const name, ...} => `Hello, Editor ${name}.`,
    {role: 'viewer', const name, ...} => `Hi, ${name}. You have read-only access.`,
  };
}
