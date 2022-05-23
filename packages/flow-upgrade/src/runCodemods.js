/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CliOptions, Codemod} from './Types';

import fs from 'fs-extra';
import {transform} from 'hermes-transform';

export default async function runCodemods(
  codemods: $ReadOnlyArray<Codemod>,
  filePaths: $ReadOnlyArray<string>,
  options: CliOptions,
): Promise<void> {
  const results = await Promise.allSettled(
    filePaths.map(async filePath => {
      const originalContents = await fs.readFile(filePath, 'utf8');
      let contents: string = originalContents;
      for (const codemod of codemods) {
        contents = transform(
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
