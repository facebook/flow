/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import path from 'path';
import fs from 'fs-extra';

export default async function findFlowconfig(
  flowconfigName: string,
  startDir: string,
  endDir: string,
): Promise<null | string> {
  const dir = path.resolve(startDir);
  const configPath = path.join(dir, flowconfigName);

  const found = await checkFileExists(configPath);
  if (found) {
    return configPath;
  }

  if (dir === endDir) {
    return null;
  }

  return findFlowconfig(flowconfigName, path.dirname(dir), endDir);
}

function checkFileExists(filepath: string): Promise<boolean> {
  return new Promise((resolve) => {
    fs.access(filepath, (err) => {
      resolve(!err);
    });
  });
}
