/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CodemodModule} from '../Types';

import {codemod} from '../Types';

export default codemod({
  title: 'Make implicitly inexact object types explicitly inexact',
  describe:
    'Convert implicitly inexact object type syntax `{}` to explicitly inexact `{...}`',
  transform: context => {
    return {
      ':not(InterfaceDeclaration, InterfaceTypeAnnotation, DeclareInterface, DeclareClass) > ObjectTypeAnnotation[inexact=false][exact=false]'(
        node,
      ) {
        // $FlowFixMe[incompatible-call] `inexact` can be true, as is the case with `{...}`
        context.modifyNodeInPlace(node, {
          inexact: true,
        });
      },
    };
  },
}) as CodemodModule;
