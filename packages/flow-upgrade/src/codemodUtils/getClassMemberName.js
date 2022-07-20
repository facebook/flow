/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {ClassMember} from 'hermes-estree';

export function getClassMemberName(member: ClassMember): ?string {
  if (member.key.type === 'PrivateIdentifier') {
    return `#${member.key.name}`;
  }

  if (member.computed === true || member.key.type !== 'Identifier') {
    return null;
  }

  return member.key.name;
}
