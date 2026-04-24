/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {AnyTypeAnnotation} from 'flow-estree-oxidized';

import {SafeEmitter} from '../../src/traverse/SafeEmitter';

const NODE: AnyTypeAnnotation = {
  type: 'AnyTypeAnnotation',
  range: [0, 0],
  loc: {
    start: {line: 0, column: 0},
    end: {line: 0, column: 0},
  },
  parent: (null: $FlowFixMe),
};

describe('SafeEmitter', () => {
  describe('emit() and on()', () => {
    it('allows listeners to be registered calls them when emitted', () => {
      const emitter = new SafeEmitter();
      const colors = [];

      emitter.on('foo', () => {
        colors.push('red');
      });
      emitter.on('foo', () => {
        colors.push('blue');
      });
      emitter.on('bar', () => {
        colors.push('green');
      });

      emitter.emit('foo', NODE);
      expect(colors).toEqual(['red', 'blue']);
      emitter.emit('bar', NODE);
      expect(colors).toEqual(['red', 'blue', 'green']);
    });

    it('passes the nodes to listeners when emitted', () => {
      const emitter = new SafeEmitter();

      emitter.on('foo', node => {
        expect(node).toBe(NODE);
      });

      emitter.emit('foo', NODE);
    });
  });
});
