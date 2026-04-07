/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const fs = require('fs');
const {join, relative, sep} = require('path');
const {diffLines} = require('diff');

const {execFilePromise, splitShellArgs} = require('./checkExecFilePromise');

// Recursively find files matching patterns, sorted with LC_ALL=C semantics.
// Uses async fs to avoid blocking the event loop during file discovery.
// Limits concurrency to avoid exhausting file descriptors.
const FIND_CONCURRENCY = 32;

async function findFiles(
  dir: string,
  patterns: Array<RegExp>,
  baseDir: string,
): Promise<Array<string>> {
  const results: Array<string> = [];
  const entries = await fs.promises.readdir(dir);
  for (let i = 0; i < entries.length; i += FIND_CONCURRENCY) {
    const batch = entries.slice(i, i + FIND_CONCURRENCY);
    await Promise.all(
      batch.map(async entry => {
        const fullPath = join(dir, entry);
        try {
          const stat = await fs.promises.stat(fullPath);
          if (stat.isDirectory()) {
            results.push(...(await findFiles(fullPath, patterns, baseDir)));
          } else if (stat.isFile()) {
            const relPath =
              './' + relative(baseDir, fullPath).split(sep).join('/');
            if (patterns.some(p => p.test(relPath))) {
              results.push(relPath);
            }
          }
        } catch (e) {
          // Skip inaccessible entries
        }
      }),
    );
  }
  return results;
}

// Generate normal diff format (matching `diff` without -u flag) using
// the `diff` npm package.  Normal format uses `<` / `>` / `---` markers
// and `N,NcN,N` style range headers.
function normalDiff(oldText: string, newText: string): string {
  // Normalize \r\n and \r → \n to match bash's `diff --strip-trailing-cr`
  const oldNorm = oldText.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
  const newNorm = newText.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
  const changes = diffLines(oldNorm, newNorm);
  const result: Array<string> = [];
  let oldLine = 1;
  let newLine = 1;

  for (let i = 0; i < changes.length; i++) {
    const change = changes[i];
    const lines = change.value.replace(/\n$/, '').split('\n');
    const count = lines.length;
    const endsWithNewline = change.value.endsWith('\n');

    if (change.removed) {
      // Look ahead for corresponding addition (change vs delete)
      const next = changes[i + 1];
      if (next && next.added) {
        const addedLines = next.value.replace(/\n$/, '').split('\n');
        const addedCount = addedLines.length;
        const addedEndsWithNewline = next.value.endsWith('\n');
        const oldRange =
          count === 1 ? String(oldLine) : oldLine + ',' + (oldLine + count - 1);
        const newRange =
          addedCount === 1
            ? String(newLine)
            : newLine + ',' + (newLine + addedCount - 1);
        result.push(oldRange + 'c' + newRange);
        for (const l of lines) {
          result.push('< ' + l);
        }
        if (!endsWithNewline) {
          result.push('\\ No newline at end of file');
        }
        result.push('---');
        for (const l of addedLines) {
          result.push('> ' + l);
        }
        if (!addedEndsWithNewline) {
          result.push('\\ No newline at end of file');
        }
        oldLine += count;
        newLine += addedCount;
        i++; // skip the added change
      } else {
        // Pure deletion
        const oldRange =
          count === 1 ? String(oldLine) : oldLine + ',' + (oldLine + count - 1);
        result.push(oldRange + 'd' + (newLine - 1));
        for (const l of lines) {
          result.push('< ' + l);
        }
        if (!endsWithNewline) {
          result.push('\\ No newline at end of file');
        }
        oldLine += count;
      }
    } else if (change.added) {
      // Pure addition (no preceding removal)
      const newRange =
        count === 1 ? String(newLine) : newLine + ',' + (newLine + count - 1);
      result.push(oldLine - 1 + 'a' + newRange);
      for (const l of lines) {
        result.push('> ' + l);
      }
      if (!endsWithNewline) {
        result.push('\\ No newline at end of file');
      }
      newLine += count;
    } else {
      // Unchanged
      oldLine += count;
      newLine += count;
    }
  }

  return result.join('\n');
}

async function runAnnotateExports(opts: {
  flowBin: string,
  testDir: string,
  noFlowlib: boolean,
  cmdArgs: string,
  logFile: string,
  monitorLogFile: string,
  waitForRecheck: string,
  fileWatcher: string,
  longLivedWorkers: string,
  env: {[string]: string | void},
}): Promise<{output: string, stderr: string, errorCode: number}> {
  const {
    flowBin,
    testDir,
    noFlowlib,
    cmdArgs,
    logFile,
    monitorLogFile,
    waitForRecheck,
    fileWatcher,
    longLivedWorkers,
    env,
  } = opts;

  let output = '';
  let stderrOutput = '';
  let errorCode = 0;

  // Find all .js and .js.flow files, sorted with C locale (byte order)
  const files = await findFiles(testDir, [/\.js$/, /\.js\.flow$/], testDir);
  files.sort((a, b) => {
    // C locale comparison (byte order)
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  });

  // Write input file
  const inputFile = join(testDir, 'input.txt');
  await fs.promises.writeFile(inputFile, files.join('\n') + '\n');

  output += '\n=== Codemod annotate-exports ===\n\n';

  // Keep copies of original files
  for (const file of files) {
    await fs.promises.copyFile(
      join(testDir, file),
      join(testDir, file + '.orig'),
    );
  }

  // Run codemod annotate-exports
  const flowlibArgs = noFlowlib ? ['--no-flowlib'] : [];
  const extraArgs = cmdArgs.trim() ? splitShellArgs(cmdArgs.trim()) : [];

  const codemodResult = await execFilePromise(
    flowBin,
    [
      'codemod',
      'annotate-exports',
      ...flowlibArgs,
      ...extraArgs,
      '--strip-root',
      '--quiet',
      '--input-file',
      inputFile,
      '--write',
      '.',
    ],
    {
      cwd: testDir,
      env,
      maxBuffer: 100 * 1024 * 1024,
    },
  );
  stderrOutput += codemodResult.stderr;
  const codemodStCode = codemodResult.code;

  // Keep copies of codemod-ed files
  for (const file of files) {
    await fs.promises.copyFile(
      join(testDir, file),
      join(testDir, file + '.codemod'),
    );
  }

  // Compare codemod-ed with original
  for (const file of files) {
    const orig = await fs.promises.readFile(
      join(testDir, file + '.orig'),
      'utf8',
    );
    const current = await fs.promises.readFile(join(testDir, file), 'utf8');
    if (orig.replace(/\r/g, '') !== current.replace(/\r/g, '')) {
      output += '>>> ' + file + '\n';
      output += current.replace(/\r/g, '') + '\n';
    }
  }

  // Match bash `(echo "$codemod_out"; echo "")` semantics: command
  // substitution strips trailing newlines, echo adds one, echo "" adds another.
  output += codemodResult.stdout.replace(/\n+$/, '') + '\n\n';

  output += '\n=== Autofix exports ===\n\n';

  // Restore original versions
  for (const file of files) {
    await fs.promises.copyFile(
      join(testDir, file + '.orig'),
      join(testDir, file),
    );
  }

  // Start flow for autofix (assert exit 0, matching bash's `start_flow . --quiet`)
  const startArgs = [
    'start',
    '.',
    ...flowlibArgs,
    '--wait',
    '--quiet',
    '--wait-for-recheck',
    waitForRecheck,
    '--file-watcher',
    fileWatcher,
    '--log-file',
    logFile,
    '--monitor-log-file',
    monitorLogFile,
    '--long-lived-workers',
    longLivedWorkers,
  ];
  const startResult = await execFilePromise(flowBin, startArgs, {
    cwd: testDir,
    env,
    maxBuffer: 100 * 1024 * 1024,
  });
  if (startResult.code !== 0) {
    stderrOutput += startResult.stderr;
    return {
      output,
      stderr: stderrOutput,
      errorCode: startResult.code,
    };
  }

  // Run autofix exports on each file, ensuring the server is stopped
  // even if an autofix call fails.
  try {
    for (const file of files) {
      const autofixResult = await execFilePromise(
        flowBin,
        ['autofix', 'exports', '--strip-root', '--in-place', file],
        {
          cwd: testDir,
          env,
          maxBuffer: 100 * 1024 * 1024,
        },
      );
      stderrOutput += autofixResult.stderr;
    }
  } finally {
    // Stop server
    await execFilePromise(flowBin, ['stop', '.'], {
      cwd: testDir,
      env,
      maxBuffer: 100 * 1024 * 1024,
    });
  }

  // Keep copies of autofix-ed files
  for (const file of files) {
    await fs.promises.copyFile(
      join(testDir, file),
      join(testDir, file + '.autofix'),
    );
  }

  // Compare autofix-ed with original
  for (const file of files) {
    const orig = await fs.promises.readFile(
      join(testDir, file + '.orig'),
      'utf8',
    );
    const autofix = await fs.promises.readFile(
      join(testDir, file + '.autofix'),
      'utf8',
    );
    if (orig.replace(/\r/g, '') !== autofix.replace(/\r/g, '')) {
      output += '>>> ' + file + '\n';
      output += autofix.replace(/\r/g, '') + '\n';
    }
  }

  // Compare codemod-ed and autofix-ed files using normal diff format
  // (matching bash's plain `diff`, not `diff -u`) for cross-platform
  // compatibility instead of shelling out to `diff`.
  output += '\n=== Diff between codemod-ed & autofix-ed ===\n';
  for (const file of files) {
    const codemod = await fs.promises.readFile(
      join(testDir, file + '.codemod'),
      'utf8',
    );
    const autofix = await fs.promises.readFile(
      join(testDir, file + '.autofix'),
      'utf8',
    );
    if (codemod.replace(/\r/g, '') !== autofix.replace(/\r/g, '')) {
      const patch = normalDiff(codemod, autofix);
      output += '>>> ' + file + '\n' + patch + '\n\n';
    }
  }

  if (codemodStCode !== 0 && codemodStCode !== 2) {
    errorCode = codemodStCode;
  }

  return {output, stderr: stderrOutput, errorCode};
}

module.exports = {runAnnotateExports, normalDiff};
