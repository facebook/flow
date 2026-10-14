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
import {transform} from 'flow-transform';

export default async function runCodemods(
  codemods: $ReadOnlyArray<CodemodModule>,
  filePaths: $ReadOnlyArray<string>,
  options: CliOptions,
): Promise<void> {
  for (const filePath of filePaths) {
    try {
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
    } catch (error) {
      // Preserve the previous Promise.allSettled behavior: a failure in one
      // file should not abort the transformation of the remaining files.
    }
  }
}
