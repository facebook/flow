/*
 * @flow
 * @format
 */

import type {TestStepFirstStage} from 'flow-dev-tools/src/test/TestStep';
import type {StepList} from 'flow-dev-tools/src/test/Tester';
const fs = require('fs');
const path = require('path');

function generateSimpleTests(
  {
    addFile,
    lspIgnoreStatusAndCancellation,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
  }: {
    addFile: TestStepFirstStage['addFile'],
    lspIgnoreStatusAndCancellation: TestStepFirstStage['lspIgnoreStatusAndCancellation'],
    lspStartAndConnect: TestStepFirstStage['lspStartAndConnect'],
    lspRequestAndWaitUntilResponse: TestStepFirstStage['lspRequestAndWaitUntilResponse'],
  },
  testDir: string,
  filename: string,
  snapshotName: string,
): StepList {
  const steps: StepList = [];
  const filenameWithIgnore = filename + '.ignored';
  steps.push(addFile(filenameWithIgnore, filename), lspStartAndConnect());

  const cursors = [];
  fs.readFileSync(path.join(testDir, filenameWithIgnore), 'utf8')
    .toString()
    .split('\n')
    .forEach((line, lineIndex) => {
      if (line.startsWith('//')) {
        const index = line.indexOf('^');
        if (index === -1) {
          return;
        }
        cursors.push({line: lineIndex - 1, character: index});
      }
    });

  if (cursors.length === 0) {
    throw 'No cursors found!';
  }

  cursors.forEach(({line, character}, i) => {
    const snapshotNameWithPossbileIndex =
      cursors.length === 1 ? snapshotName : `${snapshotName}-${i + 1}`;

    steps.push(
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {uri: `<PLACEHOLDER_PROJECT_URL>/${filename}`},
        range: {start: {line, character}, end: {line, character}},
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          testDir,
          '__snapshots__',
          snapshotNameWithPossbileIndex + '.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    );
  });

  return steps;
}

module.exports = {
  generateSimpleTests,
};
