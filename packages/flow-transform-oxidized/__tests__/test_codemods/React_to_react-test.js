/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {isStringLiteral} from 'flow-estree-oxidized';
import {t, transform} from './test-utils';

function codemod(code: string) {
  return transform(code, context => ({
    ImportDeclaration(node) {
      if (node.source.value !== 'React') {
        return;
      }

      context.replaceNode(node.source, t.StringLiteral({value: 'react'}));
    },
    CallExpression(node) {
      if (
        node.callee.type !== 'Identifier' ||
        node.callee.name !== 'require' ||
        node.arguments.length !== 1 ||
        !isStringLiteral(node.arguments[0])
      ) {
        return;
      }

      context.replaceNode(node.arguments[0], t.StringLiteral({value: 'react'}));
    },
  }));
}

describe('React to react', () => {
  it('should transform files correctly', async () => {
    const result = await codemod(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

import React1 from 'React';
import type React2 from 'React';
import React3, {useRef1} from 'React';
import type React4, {useRef2} from 'React';
import {useRef3} from 'React';

const React5 = require('React');
const {useRef4} = require('React');

function foo() {
  const state = require('React').useState();
}
`);

    expect(result).toBe(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

import React1 from 'react';
import type React2 from 'react';
import React3, {useRef1} from 'react';
import type React4, {useRef2} from 'react';
import {useRef3} from 'react';

const React5 = require('react');
const {useRef4} = require('react');

function foo() {
  const state = require('react').useState();
}
`);
  });
});
