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
import typeParameterBoundExactEmptyObjectToInexact from '../typeParameterBoundExactEmptyObjectToInexact';

testCodemod(
  'typeParameterBoundExactEmptyObjectToInexact',
  typeParameterBoundExactEmptyObjectToInexact,
  {
    ignored: [],

    transformed: [
      {
        description: 'Exact empty object tparams bound',
        code: `\
function foo<A: {}, B: string, C: {||}, D: boolean, E: {[string]: boolean}, F: {(): void}>() {}
       `,
        output: `\
function foo<
  A: {...},
  B: string,
  C: {...},
  D: boolean,
  E: {[string]: boolean},
  F: {(): void},
>() {}
       `,
      },
    ],
  },
);
