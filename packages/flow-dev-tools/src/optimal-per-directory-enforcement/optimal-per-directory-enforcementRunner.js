/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const path = require('path');
const {format} = require('util');
const {getFlowErrors} = require('../errors');
const {prettyPrintError} = require('../flowResult');
const {execManual} = require('../utils/async');
import type {Args} from './optimal-per-directory-enforcementCommand';

async function flowLS(
  bin: string,
  root: string,
  flowconfigName: string,
): Promise<Array<string>> {
  const flowconfigNameFlag = '--flowconfig-name ' + flowconfigName;
  const cmd = format('%s ls %s %s', bin, flowconfigNameFlag, root);
  const [_err, stdout, _stderr] = await execManual(cmd, {
    cwd: root,
    maxBuffer: Infinity,
  });

  return stdout.toString().trim().split('\n');
}

type TrieNode = Map<string, TrieNode>;

class Trie {
  root: TrieNode = new Map();

  constructor(paths: Iterable<string>) {
    for (const path of paths) this.add(path);
  }

  add(path: string) {
    let node = this.root;
    path.split('/').forEach(segment => {
      const nextNode = node.get(segment);
      if (nextNode != null) {
        node = nextNode;
      } else {
        const newNode: TrieNode = new Map();
        node.set(segment, newNode);
        node = newNode;
      }
    });
    // In a normal trie, we need to do extra bookkeeping to answer exact match questions.
    // However, this is not needed for this problem.
  }

  startsWith(segments: $ReadOnlyArray<string>): boolean {
    let node = this.root;
    for (const segment of segments) {
      const nextNode = node.get(segment);
      if (nextNode == null) return false;
      node = nextNode;
    }
    return true;
  }
}

class Queue<T> {
  elements: Array<T> = [];
  startIndex: number = 0;

  enqueue(e: T) {
    this.elements.push(e);
  }

  pop(): ?T {
    return this.elements[this.startIndex++];
  }
}

async function runner(args: Args): Promise<void> {
  function relativize(absolutePath: string) {
    return path.relative(path.resolve(args.root), absolutePath);
  }
  const erroredFileSet = new Set<string>();
  (
    await getFlowErrors(
      args.bin,
      args.errorCheckCommand,
      args.root,
      args.flowconfigName,
    )
  ).errors.forEach(error => {
    error.message.forEach(msg => {
      let file = msg.loc?.source;
      if (file == null) return;
      erroredFileSet.add(relativize(file));
    });
  });
  const allFlowFiles = await flowLS(args.bin, args.root, args.flowconfigName);
  const noErrorFiles = allFlowFiles
    .map(relativize)
    .filter(file => !erroredFileSet.has(file));

  const erroredFileTrie = new Trie(erroredFileSet);
  const noErrorFileTrie = new Trie(noErrorFiles);

  // We want to find the largest no error directory possible,
  // so we start with the root of the no-error trie, and bfs down if the directory has errors.
  const bfsQueue = new Queue<{node: TrieNode, path: $ReadOnlyArray<string>}>();
  bfsQueue.enqueue({node: noErrorFileTrie.root, path: []});
  const solution = new Set<string>();
  while (true) {
    const nodeAndPath = bfsQueue.pop();
    if (nodeAndPath == null) break;
    const {node, path} = nodeAndPath;
    if (!erroredFileTrie.startsWith(path)) {
      if (path.length <= args.maxDepth) {
        solution.add(path.join('/'));
      }
    } else {
      node.forEach((child, segment) => {
        bfsQueue.enqueue({node: child, path: [...path, segment]});
      });
    }
  }

  [...solution]
    .sort((a, b) => a.localeCompare(b))
    .forEach(it => console.log(it));
}

module.exports = {
  default: runner,
};
