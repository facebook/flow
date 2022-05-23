/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CliOptions} from './Types';

import path from 'path';
import fs from 'fs-extra';
import ora from 'ora';
import chalk from 'chalk';

/**
 * How many bytes we should look at for the Flow pragma.
 */
const PRAGMA_BYTES = 5000;

/**
 * Finds all of the Flow files in the provided directory as efficiently as
 * possible.
 */
export async function findFlowFiles({
  includeNonAtFlow,
  rootDirectory,
}: {
  includeNonAtFlow: boolean,
  rootDirectory: string,
}): Promise<$ReadOnlyArray<string>> {
  // All the valid file paths that we have found.
  const filePaths = [];

  // Begin the recursion!
  await processDirectory(rootDirectory);

  return filePaths;

  /**
   * Process a directory by looking at all of its entries and recursing
   * through child directories as is appropriate.
   */
  async function processDirectory(directory: string) {
    // Read the directory...
    const fileNames = await fs.readdir(directory);
    // Process every file name that we got from reading the directory.
    await Promise.all(
      fileNames.map(fileName => processFilePath(directory, fileName)),
    );
  }

  /**
   * Process a directory file path by seeing if it is a directory and either
   * recursing or adding it to filePaths.
   */
  async function processFilePath(directory: string, fileName: string) {
    // Get the file file path for this file.
    const filePath = path.join(directory, fileName);
    // Get the stats for the file.
    const stats = await fs.lstat(filePath);

    // If this is a directory...
    if (stats.isDirectory()) {
      // ...and it is not an ignored directory...
      if (
        fileName !== 'node_modules' &&
        fileName !== 'flow-typed' &&
        fileName !== '__flowtests__'
      ) {
        // ...then recursively process the directory.
        await processDirectory(filePath);
      }
    } else if (stats.isFile()) {
      // Otherwise if this is a JavaScript/JSX file and it is not an ignored
      // JavaScript file...
      const fileIsJsOrJsx = /\.jsx?$/.test(fileName);
      const fileIsIgnored = fileName.endsWith('-flowtest.js');
      if (fileIsJsOrJsx && !fileIsIgnored) {
        // Then process the file path as JavaScript.
        await processJavaScriptFilePath(filePath, stats.size);
      }
      // If this is a Flow file then we don't need to check the file pragma
      // and can add the file to our paths immediately.
      if (fileName.endsWith('.flow')) {
        filePaths.push(filePath);
      }
    }
  }

  /**
   * Check if a file path really is a Flow file by looking for the @flow
   * header pragma.
   */
  async function processJavaScriptFilePath(
    filePath: string,
    fileByteSize: number,
  ) {
    // If `all` was configured then we don't need to check for the Flow
    // header pragma.
    if (includeNonAtFlow) {
      filePaths.push(filePath);
      return;
    }
    // Open the file path.
    const file = await fs.open(filePath, 'r');

    // Get the smaller of our pragma chars constant and the file byte size.
    const bytes = Math.min(PRAGMA_BYTES, fileByteSize);
    // Create the buffer we will read to.
    const buffer = Buffer.alloc(bytes);
    // Read a set number of bytes from the file.
    await fs.read(file, buffer, 0, bytes, 0);

    // If the buffer has the @flow pragma then add the file path to our
    // final file paths array.
    if (buffer.includes('@flow')) {
      filePaths.push(filePath);
    }
    // Close the file.
    await fs.close(file);
  }
}

export async function findFlowFilesWithSpinner(
  rootDirectory: string,
  options: CliOptions,
): Promise<$ReadOnlyArray<string>> {
  // Create a new spinner.
  const spinner = ora({
    text: chalk.italic.cyan('Finding all the Flow files to be upgraded...'),
    color: 'cyan',
    isSilent: options.silent,
  });

  // Start the spinner.
  spinner.start();

  // Find all of the Flow files in the directory we are upgrading.
  const filePaths = await (() => {
    try {
      return findFlowFiles({
        rootDirectory,
        includeNonAtFlow: options.all,
      });
    } catch (error) {
      // Stop the spinner if we get an error.
      spinner.stop();
      throw error;
    }
  })();

  // Stop the spinner.
  spinner.stop();

  // Log the number of Flow files that we found.
  if (!options.silent) {
    console.log(`Found ${chalk.bold.cyan(filePaths.length)} Flow files.`);
    console.log();
  }

  return filePaths;
}
