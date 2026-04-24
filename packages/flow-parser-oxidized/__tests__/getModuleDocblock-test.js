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

import {parseDocblockString} from '../src/getModuleDocblock';

describe('parseDocblockString', () => {
  it('handles empty docblocks', () => {
    expect(parseDocblockString('')).toEqual({});
  });

  it('parses empty directives', () => {
    expect(
      parseDocblockString(`*
 * @flow
 * @format
`),
    ).toEqual({
      flow: [''],
      format: [''],
    });
  });

  it('parses directives with values', () => {
    expect(
      parseDocblockString(`*
 * @flow strict-local
 * @directive value
`),
    ).toEqual({
      flow: ['strict-local'],
      directive: ['value'],
    });
  });

  it('trims whitespace from the value', () => {
    expect(
      parseDocblockString(`*
 * @flow                  strict-local
 * @directive value${'             '}
`),
    ).toEqual({
      flow: ['strict-local'],
      directive: ['value'],
    });
  });

  it('handles repeated directives', () => {
    expect(
      parseDocblockString(`*
 * @multi one
 * @multi two
 * @multi three
 * @other_multi other_one
 * @other_multi other_two
 * @other_multi other_three
`),
    ).toEqual({
      multi: ['one', 'two', 'three'],
      other_multi: ['other_one', 'other_two', 'other_three'],
    });
  });

  it('ignores free text and copyright headers', () => {
    expect(
      parseDocblockString(`*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
`),
    ).toEqual({
      flow: ['strict-local'],
      format: [''],
    });
  });
});
