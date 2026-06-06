/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {AlignmentCase} from '../__test_utils__/alignment-utils';

import {
  expectBabelAlignment,
  expectEspreeAlignment,
} from '../__test_utils__/alignment-utils';

describe('Directive', () => {
  describe('Top level directives', () => {
    const testCase: AlignmentCase = {
      code: `
      'use strict';
      'use strict';
      Foo;
      `,
      espree: {
        expectToFail: false,
      },
      babel: {
        expectToFail: false,
      },
    };

    test('ESTree', () => {
      // ESTree top level directive nodes
      expectEspreeAlignment(testCase);
    });

    test('Babel', () => {
      // Babel top level directive nodes
      expectBabelAlignment(testCase);
    });
  });

  describe('Function body directive', () => {
    const testCase: AlignmentCase = {
      code: `
      function test() {
        'use strict';
        Foo;
      }
      `,
      espree: {
        expectToFail: false,
      },
      babel: {
        expectToFail: false,
      },
    };

    test('ESTree', () => {
      // ESTree top level directive nodes
      expectEspreeAlignment(testCase);
    });

    test('Babel', () => {
      // Babel top level directive nodes
      expectBabelAlignment(testCase);
    });
  });
});
