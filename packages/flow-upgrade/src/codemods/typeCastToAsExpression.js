/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 * @oncall flow
 */

import type {CodemodModule} from '../Types';

import {codemod} from '../Types';

import type {AsExpression, TypeCastExpression} from 'hermes-estree';
import type {MaybeDetachedNode} from 'hermes-transform';

import {t} from 'hermes-transform';

function convertTypeCast(
  node: TypeCastExpression,
): MaybeDetachedNode<AsExpression> {
  const {
    expression,
    typeAnnotation: {typeAnnotation},
  } = node;

  return t.AsExpression({
    expression:
      expression.type === 'TypeCastExpression'
        ? convertTypeCast(expression)
        : expression,
    typeAnnotation,
  });
}

export default codemod({
  title: 'Update type casting syntax',
  describe:
    'Convert type cast expressions `(<expr>: <type>)` to as expressions `<expr> as <type>`',
  transform: context => {
    return {
      TypeCastExpression(node) {
        context.replaceNode(node, convertTypeCast(node), {
          keepComments: true,
        });
        context.skipTraversal();
      },
    };
  },
}) as CodemodModule;
