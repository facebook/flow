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

import {DefinitionType, ScopeType} from '../../src';
import {verifyHasScopes} from '../../__test_utils__/verifyHasScopes';

describe('declare function overloads', () => {
  describe('directly used', () => {
    verifyHasScopes(
      `
        declare function foo(): void;
        declare function foo(arg: string): string;
        function foo(arg?: string) {
          return arg;
        }

        foo();

        export {};
      `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'foo',
              type: [
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
              ],
              referenceCount: 1,
            },
          ],
        },
        {
          type: ScopeType.Function,
          variables: [
            {
              name: 'arguments',
              type: null,
              referenceCount: 0,
            },
            {
              name: 'arg',
              type: DefinitionType.Parameter,
              referenceCount: 1,
            },
          ],
        },
      ],
    );
  });

  describe('export named', () => {
    verifyHasScopes(
      `
        declare function foo(): void;
        declare function foo(arg: string): string;
        export function foo(arg?: string) {
          return arg;
        }
      `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'foo',
              type: [
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
              ],
              referenceCount: 0,
              eslintUsed: true,
            },
          ],
        },
        {
          type: ScopeType.Function,
          variables: [
            {
              name: 'arguments',
              type: null,
              referenceCount: 0,
            },
            {
              name: 'arg',
              type: DefinitionType.Parameter,
              referenceCount: 1,
            },
          ],
        },
      ],
    );
  });

  describe('export default', () => {
    verifyHasScopes(
      `
        declare function foo(): void;
        declare function foo(arg: string): string;
        export default function foo(arg?: string) {
          return arg;
        }
      `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'foo',
              type: [
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
                DefinitionType.FunctionName,
              ],
              referenceCount: 0,
              eslintUsed: true,
            },
          ],
        },
        {
          type: ScopeType.Function,
          variables: [
            {
              name: 'arguments',
              type: null,
              referenceCount: 0,
            },
            {
              name: 'arg',
              type: DefinitionType.Parameter,
              referenceCount: 1,
            },
          ],
        },
      ],
    );
  });
});
