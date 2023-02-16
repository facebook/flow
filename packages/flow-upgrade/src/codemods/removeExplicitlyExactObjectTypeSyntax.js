/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Codemod} from '../Types';

import {codemod} from '../Types';

export default (codemod({
  title: 'Remove explicitly exact object type syntax',
  description:
    'Convert explicitly exact object type syntax `{| |}` to be just be `{ }`. To be done after you turn on `exact_by_default=true` in your `.flowconfig`.',
  transform: context => {
    return {
      'ObjectTypeAnnotation[exact=true]'(node) {
        context.modifyNodeInPlace(node, {
          exact: false,
        });
      },
    };
  },
}): Codemod);
