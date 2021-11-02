/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noformat
 * @flow
 */

const chalk = require('chalk');

exports.divider = function divider(): string {
  return chalk.grey(
    Array(80)
      .fill('=')
      .join(''),
  );
};

exports.sectionHeader = function sectionHeader(header: string): string {
  return chalk.bold(`# ${header}`);
};

exports.upgradeTitle = function upgradeTitle(title: string, n: number): string {
  return chalk.bold(`${chalk.grey(`${n}.`)} ${chalk.blue(title)}`);
};

exports.codeblock = function codeblock(code: string): string {
  return (
    code
      // Add indentation to the beginning of every line of code.
      .split('\n')
      .map(line => `    ${line}`)
      .join('\n')
      // Add highlighting for puncuation. We need to do this first otherwise it
      // breaks the other colors.
      .replace(/([={}()\[\];<>,:.*/])/g, chalk.grey('$1'))
      // Add highlighting for reserved keywords.
      .replace(
        /(import|from|typeof|type|class|extends|static|return|function)/g,
        chalk.magenta('$1'),
      )
      // Add highlighting for strings.
      .replace(/('[^\n]*')/g, chalk.green('$1'))
  );
};

exports.list = function list(items: Array<string>): string {
  return (
    items
      .map(item =>
        // Indent every item and add a `- ` to the beginning of each line.
        item
          .split('\n')
          .map((line, i) => `${i === 0 ? chalk.grey('- ') : '  '}${line}`)
          .join('\n'),
      )
      // Combine all of our items into one list string.
      .join('\n')
  );
};
