/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export opaque type Email = string;

export function parseEmail(raw: string): Email | null {
  const trimmed = raw.trim();
  return /^[^@\s]+@[^@\s]+\.[^@\s]+$/.test(trimmed) ? trimmed : null;
}

export function domainOf(email: Email): string {
  return email.slice(email.indexOf('@') + 1);
}
