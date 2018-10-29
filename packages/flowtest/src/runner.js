/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @flow
 * @format
 */
'use strict';

const child_process = require('child_process');
const colors = require('colors/safe');
const fs = require('fs');
const path = require('path');
const util = require('util');

export const PASSED = 0;
export const FAILED = 1;
export const ERRORED = 2;

export function run(bin: string, roots: Array<string>): typeof PASSED | typeof FAILED | typeof ERRORED {
  roots = roots.map(root => path.resolve(root));
  roots.forEach(root => {
    if (!fs.existsSync(root)) {
      process.stderr.write(util.format('Cannot find directory %s\n', root));
      return ERRORED;
    }
    if (!fs.existsSync(path.join(root, '.flowconfig'))) {
      process.stderr.write(util.format('Cannot find .flowconfig in %s\n', root));
      return ERRORED;
    }
  });

  const flowInfo = JSON.parse(
    child_process.execSync(util.format('%s version --json', bin)).toString(),
  );

  console.log('Using Flow v%s (%s)', flowInfo.semver, flowInfo.binary);

  let failed = false;
  roots.forEach(root => {
    if (FAILED === testRoot(bin, root)) {
      failed = true;
    }
  });

  if (failed) {
    return FAILED;
  } else {
    return PASSED;
  }
}

function testRoot(bin, root): typeof PASSED | typeof FAILED | typeof ERRORED {
  console.log('Discovering flowtests in root `%s`', root);
  const rawFiles = JSON.parse(
    child_process
      .execSync(util.format("%s ls --root '%s' --explain --json", bin, root))
      .toString(),
  );

  const files = [];
  const fileErrors = new Map();
  for (const file in rawFiles) {
    const relative_file = path.relative(root, file);
    if (
      rawFiles[file].explanation.match(/Included/) &&
      !relative_file.match(/node_modules[/\\]/) &&
      (relative_file.match(/__flowtests__[/\\]/) ||
      relative_file.match(/-flowtest\.js$/))
    ) {
      files.push(file);
      fileErrors.set(file, []);
    }
  }

  console.log(
    'Found %d flowtest%s in root `%s`',
    files.length,
    files.length == 1 ? '' : 's',
    root,
  );
  if (files.length === 0) {
    return PASSED;
  }

  const flowServerCheck = child_process.spawnSync(bin, [
    'status',
    '--no-auto-start',
    '--timeout',
    '1',
    root,
  ]);
  const flowServerExists = flowServerCheck.status != 6; // 6 means no server

  let flowErrors;
  if (flowServerExists) {
    console.log('Using the existing flow server and running flow status');
    const statusResult = child_process.spawnSync(bin, [
      'status',
      '--include-warnings',
      '--json',
      files[0],
    ]);

    if (statusResult.status !== 0 && statusResult.status !== 2) {
      console.log(
        'flow status command exited with abnormal status %d.' +
          '\nstdout:\n%s\nstderr:\n%s\n',
        statusResult.status,
        statusResult.stdout.toString(),
        statusResult.stderr.toString(),
      );
      return ERRORED;
    }

    flowErrors = JSON.parse(statusResult.stdout.toString()).errors;
  } else {
    console.log('No flow server is running, so falling back to focus-check');
    const focusCheckResult = child_process.spawnSync(
      bin,
      ['focus-check', '--include-warnings', '--input-file', '-', '--json'],
      {input: files.join('\n')},
    );

    if (focusCheckResult.status !== 0 && focusCheckResult.status !== 2) {
      console.log(
        'flow focus-check command exited with abnormal status %d.' +
          '\nstdout:\n%s\nstderr:\n%s\n',
        focusCheckResult.status,
        focusCheckResult.stdout.toString(),
        focusCheckResult.stderr.toString(),
      );
      return ERRORED;
    }

    flowErrors = JSON.parse(focusCheckResult.stdout.toString()).errors;
  }

  flowErrors = flowErrors.filter(error => (
    error.message[0].descr === "Error suppressing comment" &&
    error.message[1].descr === "Unused suppression"
  ) || (
    error.message[0].descr === "Unused suppression comment."
  ));
  flowErrors.forEach(error => {
    const mentionedFiles = new Set();
    error.message.forEach(
      f => f.loc && f.loc.source && mentionedFiles.add(f.loc.source),
    );
    mentionedFiles.forEach(f => {
      const list = fileErrors.get(f);
      list && list.push(error);
    });
  });

  let passed = 0;
  let failed = 0;
  fileErrors.forEach((errors, file) => {
    let status;
    if (errors.length === 0) {
      status = colors.green.bold('[\u2713] PASS');
      passed++;
    } else {
      status = colors.red.bold('[\u2717] FAIL');
      failed++;
    }
    console.log('%s: %s', status, file);
  });

  console.log('%d tests passed, %d tests failed', passed, failed);
  return failed === 0 ? PASSED : FAILED;
}
