/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import fs from 'fs-extra';
import os from 'os';
import path from 'path';

import convertLegacyUtilityTypes from '../codemods/convertLegacyUtilityTypes';
import runCodemods from '../runCodemods';

// hermes-transform is not safe to run concurrently (see the comment in
// runCodemods.js), so runCodemods must process files one at a time. Track the
// maximum number of in-flight transform calls to prove that they never overlap.
const state = {
  active: 0,
  maxActive: 0,
};

jest.mock('hermes-transform', () => ({
  transform: async (code: string) => {
    state.active += 1;
    state.maxActive = Math.max(state.maxActive, state.active);
    // Keep the transform pending long enough that a concurrent caller would
    // observe overlapping invocations.
    await new Promise(resolve => setTimeout(resolve, 25));
    state.active -= 1;
    return code;
  },
}));

describe('runCodemods', () => {
  it('transforms files sequentially', async () => {
    const dir = fs.mkdtempSync(
      path.join(os.tmpdir(), 'flow-upgrade-run-codemods-'),
    );

    const filePaths: Array<string> = [];
    try {
      for (const name of ['A.js', 'B.js', 'C.js']) {
        const filePath = path.join(dir, name);
        await fs.writeFile(filePath, '// @flow\n', 'utf8');
        filePaths.push(filePath);
      }

      await runCodemods(
        // The transform of the codemod is never invoked by our mocked
        // hermes-transform, so its contents are irrelevant.
        [convertLegacyUtilityTypes],
        filePaths,
        {
          all: true,
          prettierOptions: {},
          silent: true,
          yes: true,
        },
      );

      expect(state.maxActive).toBe(1);
    } finally {
      await fs.remove(dir);
    }
  });
});
