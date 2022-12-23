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
import {t} from 'hermes-transform';

export default (codemod({
  title: 'Replace `T: {}` with `T: {...}` in type parameter bounds',
  description: 'Replace `T: {}` with `T: {...}` in type parameter bounds',
  transform: context => {
    return {
      TypeParameter(node) {
        if (node.bound != null) {
          const annotation = node.bound.typeAnnotation;
          if (
            annotation.type === 'ObjectTypeAnnotation' &&
            !annotation.inexact &&
            annotation.properties.length === 0 &&
            annotation.callProperties.length === 0 &&
            annotation.indexers.length === 0 &&
            annotation.internalSlots.length === 0
          ) {
            context.replaceNode(
              annotation,
              t.ObjectTypeAnnotation({
                properties: [],
                callProperties: [],
                exact: false,
                /* $FlowExpectedError[incompatible-call]
                 * TODO: hermes-transform mandates that inexact must be
                 * `false`, which is wrong. */
                inexact: true,
                indexers: [],
                internalSlots: [],
              }),
            );
          }
        }
      },
    };
  },
}): Codemod);
