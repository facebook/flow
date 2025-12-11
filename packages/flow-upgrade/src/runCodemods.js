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
import {transform} from 'hermes-transform';

export default async function runCodemods(
  codemods: $ReadOnlyArray<CodemodModule>,
  filePaths: $ReadOnlyArray<string>,
  options: CliOptions,
): Promise<void> {
  const results = await Promise.allSettled(
    filePaths.map(async filePath => {
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
        contents = await transform(
          contents,
          codemod.transform,
          options.prettierOptions,
        );
      }

      if (originalContents !== contents) {
        await fs.writeFile(filePath, contents, 'utf8');
      }
    }),
  );
}
