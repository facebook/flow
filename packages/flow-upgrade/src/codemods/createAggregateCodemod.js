/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

/**
 * Creates a transform function that can be used with `jscodeshift` that is the
 * aggregate of the transform paths.
 *
 * This file is not required directly when we run `flow-upgrade`. Instead it is
 * initialized in a `jscodeshift` worker.
 */

type TransformFn = (file: {source: string}, api: any) => string | null;

module.exports = (transformPaths: Array<string>): TransformFn => {
  return (file: {source: string}, api: any): string | null => {
    // Get the jscodeshift API and parse our source file.
    const j = api.jscodeshift;
    // Parse ths source file.
    const root = j(file.source);
    // Iterate through all of our transform paths so that we can apply them.
    const skipped = transformPaths.reduce(
      (skipped, transformPath) => {
        // Require the transform path.
        const transform = (require: any)(transformPath);
        // Use the transform to codemod the file.
        const notSkipped = transform(j, root);
        // If all of the transforms return true then skipped will be true.
        return skipped && !notSkipped;
      },
      // If we have no codemods then skipped should be true.
      true,
    );
    // Either return null or the new source file depending on whether or all the
    // transforms skipped.
    return skipped ? null : root.toSource();
  };
};
