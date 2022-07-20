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
import codemod from '../collapseObjectInitialization';

testCodemod('collapseObjectInitialization', codemod, {
  // # IGNORED
  ignored: [
    {
      description: 'assignment expression',
      code: `
        let obj;
        obj = {};
        obj.a = 1;
      `,
    },
    {
      description: 'statements in between',
      code: `
        const obj = {};
        f();
        obj.a = 1;
      `,
    },
  ],
  // # TRANFORMED
  transformed: [
    {
      description: 'basic',
      code: `
        const obj = {};
        obj.a = 1;
      `,
      output: `
        const obj = {a: 1};
      `,
    },
    {
      description: 'more complex',
      code: `
        const obj = {};
        obj.a = 1;
        obj.b = f();
        obj.c = c;
      `,
      output: `
        const obj = {a: 1, b: f(), c};
      `,
    },
    {
      description: 'self referential',
      code: `
        const obj = {};
        obj.a = 1;
        obj.b = obj.a + 2;
        obj.c = 3;
      `,
      output: `
        const obj = {a: 1};
obj.b = obj.a + 2;
obj.c = 3;
      `,
    },
  ],
});
