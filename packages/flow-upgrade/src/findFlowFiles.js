/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

const path = require('path');
const fs = require('graceful-fs');

/**
 * How many bytes we should look at for the Flow pragma.
 */
const PRAGMA_BYTES = 5000;

/**
 * Finds all of the Flow files in the provided directory as efficiently as
 * possible.
 */
// If we use promises then Node.js will quickly run out of memory on
// large codebases. Instead we use the callback API.
module.exports = function findFlowFiles(
  rootDirectory: string,
  options: {+all: boolean},
): Promise<Array<string>> {
  return new Promise((_resolve, _reject) => {
    // Tracks whether or not we have rejected our promise.
    let rejected = false;
    // How many asynchronous tasks are waiting at the moment.
    let waiting = 0;
    // All the valid file paths that we have found.
    const filePaths = [];

    // Begin the recursion!
    processDirectory(rootDirectory);

    /**
     * Process a directory by looking at all of its entries and recursing
     * through child directories as is appropriate.
     */
    function processDirectory(directory: string) {
      // If we were rejected then we should not continue.
      if (rejected === true) {
        return;
      }
      // We are now waiting on this asynchronous task.
      waiting++;
      // Read the directory...
      fs.readdir(directory, (error, fileNames) => {
        if (error) {
          return reject(error);
        }
        // Process every file name that we got from reading the directory.
        for (let i = 0; i < fileNames.length; i++) {
          processFilePath(directory, fileNames[i]);
        }
        // We are done with this async task.
        done();
      });
    }

    /**
     * Process a directory file path by seeing if it is a directory and either
     * recursing or adding it to filePaths.
     */
    function processFilePath(directory: string, fileName: string) {
      // If we were rejected then we should not continue.
      if (rejected === true) {
        return;
      }
      // We are now waiting on this asynchronous task.
      waiting++;
      // Get the file file path for this file.
      const filePath = path.join(directory, fileName);
      // Get the stats for the file.
      fs.lstat(filePath, (error, stats) => {
        if (error) {
          return reject(error);
        }
        // If this is a directory...
        if (stats.isDirectory()) {
          // ...and it is not an ignored directory...
          if (
            fileName !== 'node_modules' &&
            fileName !== 'flow-typed' &&
            fileName !== '__flowtests__'
          ) {
            // ...then recursively process the directory.
            processDirectory(filePath);
          }
        } else if (stats.isFile()) {
          // Otherwise if this is a JavaScript/JSX file and it is not an ignored
          // JavaScript file...
          const fileIsJsOrJsx = /\.jsx?$/.test(fileName);
          const fileIsIgnored = fileName.endsWith('-flowtest.js');
          if (fileIsJsOrJsx && !fileIsIgnored) {
            // Then process the file path as JavaScript.
            processJavaScriptFilePath(filePath, stats.size);
          }
          // If this is a Flow file then we don't need to check the file pragma
          // and can add the file to our paths immediately.
          if (fileName.endsWith('.flow')) {
            filePaths.push(filePath);
          }
        }
        // We are done with this async task
        done();
      });
    }

    /**
     * Check if a file path really is a Flow file by looking for the @flow
     * header pragma.
     */
    function processJavaScriptFilePath(filePath: string, fileByteSize: number) {
      // If `all` was configured then we don't need to check for the Flow
      // header pragma.
      if (options.all) {
        filePaths.push(filePath);
        return;
      }
      // If we were rejected then we should not continue.
      if (rejected === true) {
        return;
      }
      // We are now waiting on this asynchronous task.
      waiting++;
      // Open the file path.
      fs.open(filePath, 'r', (error, file) => {
        if (error) {
          return reject(error);
        }
        // Get the smaller of our pragma chars constant and the file byte size.
        const bytes = Math.min(PRAGMA_BYTES, fileByteSize);
        // Create the buffer we will read to.
        const buffer = new Buffer(bytes);
        // Read a set number of bytes from the file.
        fs.read(file, buffer, 0, bytes, 0, error => {
          if (error) {
            return reject(error);
          }
          // If the buffer has the @flow pragma then add the file path to our
          // final file paths array.
          if (buffer.includes('@flow')) {
            filePaths.push(filePath);
          }
          // Close the file.
          fs.close(file, error => {
            if (error) {
              return reject(error);
            }
            // We are done with this async task
            done();
          });
        });
      });
    }

    /**
     * Our implementation of resolve that will only actually resolve if we are
     * done waiting everywhere.
     */
    function done() {
      // We don't care if we were rejected.
      if (rejected === true) {
        return;
      }
      // Decrement the number of async tasks we are waiting on.
      waiting--;
      // If we are finished waiting then we want to resolve our promise.
      if (waiting <= 0) {
        if (waiting === 0) {
          _resolve(filePaths);
        } else {
          reject(new Error(`Expected a positive number: ${waiting}`));
        }
      }
    }

    /**
     * Our implementation of reject that also sets `rejected` to false.
     */
    function reject(error) {
      rejected = true;
      _reject(error);
    }
  });
};
