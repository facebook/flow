/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import {testCodemod} from '../../../testUtils/codemodTestUtils';
import removeAnnotationsInDestructuring from '../removeAnnotationsInDestructuring';

testCodemod(
  'removeAnnotationsInDestructuring',
  removeAnnotationsInDestructuring,
  {
    // IGNORED
    ignored: [
      {
        description: 'annotation on top-level identifier',
        code: `const x: T = f();`,
      },
      {
        description: 'annotation on top-level array pattern',
        code: `const [x, y]: T = f();`,
      },
      {
        description: 'annotation on top-level object pattern',
        code: `const {x, y}: T = f();`,
      },
      {
        description: 'annotation on function within default',
        code: `const {f: f = (x: T) => {}} = f();`,
      },
    ],

    // TRANSFORMED
    transformed: [
      {
        description: 'annotation on nested identifier',
        code: `const [x: T] = f();`,
        output: `const [x] = f();`,
      },
      {
        description: 'annotation on nested array pattern',
        code: `const [[x, y]: T] = f();`,
        output: `const [[x, y]] = f();`,
      },
      {
        description: 'annotation on nested object pattern',
        code: `const [{x, y}: T] = f();`,
        output: `const [{x, y}] = f();`,
      },
      {
        description: 'annotation on rest element',
        code: `const [...xs: T] = f();`,
        output: `const [...xs] = f();`,
      },
      {
        description: 'annotation on nested identifier inside object pattern',
        code: `const {x: y: T} = f();`,
        output: `const {x: y} = f();`,
      },
    ],
  },
);
