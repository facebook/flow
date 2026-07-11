/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

declare class FileEntry {
  name: string;
  byteSize: number;
}

declare class DirEntry {
  name: string;
  children: Array<Entry>;
}

declare class Entry {
  name: string;
  // TODO: declare the methods `totalSize` relies on.
}

export function totalSize(entry: Entry): number {
  if (entry.isFile()) {
    return entry.byteSize;
  } else if (entry.isDir()) {
    return entry.children.reduce(
      (sum, child) => sum + totalSize(child),
      0,
    );
  }
  return 0;
}
