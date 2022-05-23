/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

jest.mock('fs');
jest.mock('fs/promises');

import {vol} from 'memfs';
import upgrade from '../src/upgrade';

describe('upgrade', () => {
  beforeEach(() => {
    vol.reset();
  });

  const TEST_ROOT_DIR = '/test';
  const SOURCE_ROOT_DIR = `${TEST_ROOT_DIR}/src`;
  const TEST_FLOW_FILE = `${SOURCE_ROOT_DIR}/class-flow.js`;
  const FLOW_FILE_CONTENTS = `\
/**
 * @format
 * @flow
 */
class Foo {
  duplicate: string = '';
  duplicate: string = '';
}
`;
  const FLOW_FILE_CONTENTS_AFTER_TRANSFORM = `\
/**
 * @format
 * @flow
 */
class Foo {
  duplicate: string = '';
}
`;
  const TEST_NON_FLOW_FILE = `${SOURCE_ROOT_DIR}/class-noflow.js`;
  const NON_FLOW_FILE_CONTENTS = `\
/**
 * @format
 */
class Foo {
  duplicate: string = '';
  duplicate: string = '';
}
`;
  const NON_FLOW_FILE_CONTENTS_AFTER_TRANSFORM = `\
/**
 * @format
 */
class Foo {
  duplicate: string = '';
}
`;
  function prepVfs() {
    vol.mkdirSync(TEST_ROOT_DIR);
    vol.mkdirSync(SOURCE_ROOT_DIR);
    vol.writeFileSync(TEST_FLOW_FILE, FLOW_FILE_CONTENTS, {encoding: 'utf8'});
    vol.writeFileSync(TEST_NON_FLOW_FILE, NON_FLOW_FILE_CONTENTS, {
      encoding: 'utf8',
    });
  }

  it('should ignore non-@flow files if `all` is not supplied', async () => {
    prepVfs();

    // perform the upgrade
    await upgrade(TEST_ROOT_DIR, '0.169.0', '0.170.0', {
      all: false,
      prettierOptions: {
        singleQuote: true,
      },
      silent: true,
      yes: true,
    });

    // ensure the changes were made
    expect(
      vol.readFileSync(TEST_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(FLOW_FILE_CONTENTS_AFTER_TRANSFORM);
    expect(
      vol.readFileSync(TEST_NON_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(NON_FLOW_FILE_CONTENTS);
  });

  it('should not ignore non-@flow files if `all` is supplied', async () => {
    prepVfs();

    // perform the upgrade
    await upgrade(TEST_ROOT_DIR, '0.169.0', '0.170.0', {
      all: true,
      prettierOptions: {
        singleQuote: true,
      },
      silent: true,
      yes: true,
    });

    // ensure the changes were made
    expect(
      vol.readFileSync(TEST_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(FLOW_FILE_CONTENTS_AFTER_TRANSFORM);
    expect(
      vol.readFileSync(TEST_NON_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(NON_FLOW_FILE_CONTENTS_AFTER_TRANSFORM);
  });

  it('should not not do upgrades if the version is outside the range', async () => {
    prepVfs();

    // perform the upgrade
    await upgrade(TEST_ROOT_DIR, '0.170.0', '0.171.0', {
      all: true,
      prettierOptions: {
        singleQuote: true,
      },
      silent: true,
      yes: true,
    });

    // ensure the changes were NOT made
    expect(
      vol.readFileSync(TEST_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(FLOW_FILE_CONTENTS);
    expect(
      vol.readFileSync(TEST_NON_FLOW_FILE, {
        encoding: 'utf8',
      }),
    ).toBe(NON_FLOW_FILE_CONTENTS);
  });
});
