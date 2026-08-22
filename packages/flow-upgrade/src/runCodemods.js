/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CliOptions, CodemodModule} from './Types';

import fs from 'fs-extra';

export default async function runCodemods(
  codemods: $ReadOnlyArray<CodemodModule>,
  filePaths: $ReadOnlyArray<string>,
  options: CliOptions,
): Promise<void> {
  // Files must be transformed sequentially, not concurrently.
  //
  // WORKAROUND: hermes-transform is not safe to run in parallel. Prettier
  // memoizes loaded parser plugins keyed by the parser name, and hermes-
  // transform's printer wires the AST of the current file up through that
  // parser. When multiple files are transformed concurrently the plugin cache
  // is shared, so a file can end up being printed with a different file's AST.
  // Clearing the require cache below resets that state, but it is only
  // reliable when each file is processed one at a time. See facebook/flow#9407.
  for (const filePath of filePaths) {
    try {
      // WORKAROUND: Clear hermes-transform and prettier from require cache
      // to avoid state persistence bug where transformation results are cached
      // and reused across multiple files. This is a known issue in hermes-transform.
      if (typeof require !== 'undefined' && require.cache) {
        Object.keys(require.cache).forEach(key => {
          if (key.includes('hermes-transform') || key.includes('prettier')) {
            delete require.cache[key];
          }
        });
      }

      const originalContents = await fs.readFile(filePath, 'utf8');
      let contents: string = originalContents;
      for (const codemod of codemods) {
        if (typeof jest !== 'undefined') {
          jest.resetModules();
        }
        const {transform} = require('hermes-transform');
        contents = await transform(
          contents,
          codemod.transform,
          options.prettierOptions,
        );
      }

      if (originalContents !== contents) {
        await fs.writeFile(filePath, contents, 'utf8');
      }
    } catch (error) {
      // Preserve the previous Promise.allSettled behavior: a failure in one
      // file should not abort the transformation of the remaining files.
    }
  }
}
