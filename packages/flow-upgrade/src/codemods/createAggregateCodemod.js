/**
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
module.exports = (transformPaths: Array<string>) => {
  return (file: {source: string}, api: any) => {
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
