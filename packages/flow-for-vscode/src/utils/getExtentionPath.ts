/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';

const THIS_EXSTENSION_ID = 'flowtype.flow-for-vscode';

export default function getExtensionPath(): string {
  const thisExtension = vscode.extensions.getExtension(THIS_EXSTENSION_ID);
  if (!thisExtension) {
    throw new Error('Failed to find extensionPath');
  }
  return thisExtension.extensionPath;
}
