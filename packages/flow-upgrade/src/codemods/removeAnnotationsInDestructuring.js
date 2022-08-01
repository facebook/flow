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
  title: 'Remove Annotations In Destructuring',
  description:
    'Remove annotations nested inside of destructuring. These are not valid Flow syntax.',
  transform: context => {
    return {
      ':matches(ArrayPattern, ObjectPattern) > :matches(Identifier, ArrayPattern, ObjectPattern)[typeAnnotation!=null]'(
        node,
      ) {
        context.modifyNodeInPlace(node, {
          typeAnnotation: null,
        });
      },
      ':matches(ArrayPattern, ObjectPattern) > :matches(RestElement, Property) > [typeAnnotation!=null]'(
        node,
      ) {
        context.modifyNodeInPlace(node, {
          typeAnnotation: null,
        });
      },
    };
  },
}): Codemod);
