/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {parseEmail, domainOf, type Email} from 'Email';

export function isCorporate(email: Email): boolean {
  return domainOf(email) === 'meta.com';
}

export function collectValid(raws: ReadonlyArray<string>): Array<Email> {
  const result: Array<Email> = [];
  for (const raw of raws) {
    const email = parseEmail(raw);
    if (email != null) {
      result.push(email);
    }
  }
  return result;
}
