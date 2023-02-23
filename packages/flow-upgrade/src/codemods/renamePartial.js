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
  title: 'Rename `$Partial` to `Partial`',
  description: 'Renames the `$Partial` type utility to `Partial`.',
  transform: context => {
    return {
      'GenericTypeAnnotation[id.name="$Partial"][typeParameters.params.length > 0]'(
        node,
      ) {
        context.modifyNodeInPlace(node.id, {
          name: 'Partial',
        });
      },
    };
  },
}): Codemod);
