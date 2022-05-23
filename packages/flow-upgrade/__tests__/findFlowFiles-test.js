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
import {findFlowFiles} from '../src/findFlowFiles';

const FLOW_DOCBLOCK = `\
/**
 * @format
 * @flow
 */
`;
const NON_FLOW_DOCBLOCK = `\
/**
 * @format
 */
`;
const TEST_ROOT_DIR = '/test';
const SOURCE_ROOT_DIR = `${TEST_ROOT_DIR}/src`;
const FLOW_FILE = 'flow.js';
const NON_FLOW_FILE = 'noflow.js';
const NON_JS_FILE = 'notjs.ts';

describe('findFlowFiles', () => {
  beforeEach(() => {
    vol.reset();
  });

  function writeFile(contents: string, filename: string): void {
    vol.writeFileSync(`${SOURCE_ROOT_DIR}/${filename}`, contents, {
      encoding: 'utf8',
    });
  }
  function prepVfs(): void {
    vol.mkdirSync(TEST_ROOT_DIR);
    vol.mkdirSync(SOURCE_ROOT_DIR);
  }
  function writeCommonTestFiles(): void {
    writeFile(FLOW_DOCBLOCK, FLOW_FILE);
    writeFile(NON_FLOW_DOCBLOCK, NON_FLOW_FILE);
    writeFile(FLOW_DOCBLOCK, NON_JS_FILE);
  }
  function removeRoot(files: $ReadOnlyArray<string>): $ReadOnlyArray<string> {
    return files.map(f => f.replace(`${SOURCE_ROOT_DIR}/`, '')).sort();
  }

  it('picks up all js files when `all=true`', async () => {
    prepVfs();
    writeCommonTestFiles();

    const result = removeRoot(
      await findFlowFiles({
        rootDirectory: TEST_ROOT_DIR,
        includeNonAtFlow: true,
      }),
    );

    expect(result).toEqual([FLOW_FILE, NON_FLOW_FILE]);
    expect(result).not.toContain(NON_JS_FILE);
  });

  it('picks up all just @flow files when `all=false`', async () => {
    prepVfs();
    writeCommonTestFiles();

    const result = removeRoot(
      await findFlowFiles({
        rootDirectory: TEST_ROOT_DIR,
        includeNonAtFlow: false,
      }),
    );

    expect(result).toEqual([FLOW_FILE]);
    expect(result).not.toContain(NON_FLOW_FILE);
    expect(result).not.toContain(NON_JS_FILE);
  });

  const MASSIVE_FILE_COUNT = 100_000;
  it('handles huge filesystems', async () => {
    prepVfs();

    const expectedFilenames = [];
    for (let i = 0; i < MASSIVE_FILE_COUNT; i += 1) {
      const filename = `${i}.js`;
      writeFile('', filename);
      expectedFilenames.push(filename);
    }
    const result = removeRoot(
      await findFlowFiles({
        rootDirectory: TEST_ROOT_DIR,
        includeNonAtFlow: true,
      }),
    );

    expect(result).toHaveLength(expectedFilenames.length);
    expect(result).toEqual(removeRoot(expectedFilenames));
  });

  const MASSIVE_FOLDER_DEPTH = 100;
  it('handles deeply nested filesystems', async () => {
    prepVfs();

    let filePath = SOURCE_ROOT_DIR;
    const expectedFilenames = [];
    for (let i = 0; i < MASSIVE_FOLDER_DEPTH; i += 1) {
      filePath += `/${i}`;
      vol.mkdirSync(filePath);

      const filename = `${filePath}/file.js`;
      vol.writeFileSync(filename, '', {encoding: 'utf8'});
      expectedFilenames.push(filename);
    }

    const result = removeRoot(
      await findFlowFiles({
        rootDirectory: TEST_ROOT_DIR,
        includeNonAtFlow: true,
      }),
    );

    expect(result).toHaveLength(expectedFilenames.length);
    expect(result).toEqual(removeRoot(expectedFilenames));
  });
});
