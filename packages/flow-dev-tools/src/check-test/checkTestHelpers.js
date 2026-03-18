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
const {join, basename, resolve, delimiter} = require('path');

const {format} = require('util');

const {execFilePromise, existsAsync} = require('./checkExecFilePromise');

const EXIT_OK = 0;
const EXIT_ONE = 1;
const EXIT_ERRS = 2;
const EXIT_INVALID_FLOWCONFIG = 8;
const EXIT_SERVER_ALREADY_RUNNING = 11;
const EXIT_COULD_NOT_FIND_FLOWCONFIG = 12;
const EXIT_USAGE = 64;

// Convert a BRE (Basic Regular Expression) pattern to JS regex syntax.
// In BRE:
//   - + ? | { } ( ) are literal unless backslash-escaped
//   - \+ \? \| \{ \} \( \) are metacharacters (quantifier, alternation, group)
// In JS regex (ERE-like):
//   - + ? | { } ( ) are metacharacters by default
//   - \+ \? \| \{ \} \( \) are literal
// Additionally, BRE does not have \d, \w, \b, \s, \D, \W, \B, \S shorthand
// classes. In BRE, \d matches literal 'd', etc. In JS regex, \d matches
// digits. This function converts these so a BRE pattern works as JS regex.
const BRE_JS_ONLY_ESCAPES = new Set(['d', 'D', 'w', 'W', 'b', 'B', 's', 'S']);

function _convertBREtoJS(pattern: string): string {
  return pattern.replace(
    /(\\\\)|\\([+?|{}()dDwWbBsS])|([+?|{}()])/g,
    (match, escapedBackslash, escaped, breLiteral) => {
      if (escapedBackslash) {
        // \\\\ → keep as-is (literal backslash)
        return escapedBackslash;
      }
      if (escaped) {
        if (BRE_JS_ONLY_ESCAPES.has(escaped)) {
          // \d \w \b \s etc. → literal character in BRE (no special meaning).
          // In JS regex these are shorthand classes, so we must NOT pass
          // them through as \d. Instead, return the literal character.
          return escaped;
        }
        // \+ \? \| \{ \} \( \) → remove backslash (JS metachar)
        return escaped;
      }
      // Unescaped + ? | { } ( ) → add backslash (JS literal)
      return '\\' + breLiteral;
    },
  );
}

class TestContext {
  _flowBin: string;
  _testDir: string;
  _outBuffer: string;
  _logFile: string;
  _monitorLogFile: string;
  _waitForRecheck: string;
  _fileWatcher: string;
  _longLivedWorkers: string;
  _savedState: boolean;
  _noFlowlib: boolean;
  _envOverrides: {[string]: string};

  EXIT_OK: number;
  EXIT_ONE: number;
  EXIT_ERRS: number;
  EXIT_INVALID_FLOWCONFIG: number;
  EXIT_SERVER_ALREADY_RUNNING: number;
  EXIT_COULD_NOT_FIND_FLOWCONFIG: number;
  EXIT_USAGE: number;

  constructor(opts: {
    flowBin: string,
    testDir: string,
    logFile: string,
    monitorLogFile: string,
    noFlowlib: boolean,
    waitForRecheck: string,
    fileWatcher: string,
    longLivedWorkers: string,
    savedState: boolean,
    tempDir: string,
  }) {
    this._flowBin = opts.flowBin;
    this._testDir = opts.testDir;
    this._outBuffer = '';
    this._logFile = opts.logFile;
    this._monitorLogFile = opts.monitorLogFile;
    this._waitForRecheck = opts.waitForRecheck;
    this._fileWatcher = opts.fileWatcher;
    this._longLivedWorkers = opts.longLivedWorkers;
    this._savedState = opts.savedState;
    this._noFlowlib = opts.noFlowlib;
    this._envOverrides = {
      FLOW_TEMP_DIR: opts.tempDir,
      IN_FLOW_TEST: '1',
      FLOW_LOG_LEVEL: 'debug',
      FLOW: opts.flowBin,
      FLOW_LOG_FILE: opts.logFile,
      FLOW_MONITOR_LOG_FILE: opts.monitorLogFile,
    };

    this.EXIT_OK = EXIT_OK;
    this.EXIT_ONE = EXIT_ONE;
    this.EXIT_ERRS = EXIT_ERRS;
    this.EXIT_INVALID_FLOWCONFIG = EXIT_INVALID_FLOWCONFIG;
    this.EXIT_SERVER_ALREADY_RUNNING = EXIT_SERVER_ALREADY_RUNNING;
    this.EXIT_COULD_NOT_FIND_FLOWCONFIG = EXIT_COULD_NOT_FIND_FLOWCONFIG;
    this.EXIT_USAGE = EXIT_USAGE;
  }

  get flowLogFile(): string {
    return this._logFile;
  }

  get flowMonitorLogFile(): string {
    return this._monitorLogFile;
  }

  getOutput(): string {
    return this._outBuffer;
  }

  print(text: string): void {
    this._outBuffer += text;
  }

  printf(fmt: string, ...args: Array<mixed>): void {
    this._outBuffer += format(fmt, ...args);
  }

  _getEnv(): {[string]: string | void} {
    // Include tests_bin in PATH so the flow server can find helper scripts
    // (e.g. fetch_saved_state.sh for --saved-state-fetcher local).
    // Matches bash's `PATH="$THIS_DIR/scripts/tests_bin:$PATH"` in
    // start_flow_unsafe.
    const testsBinDir = resolve(__dirname, '../../../../scripts/tests_bin');
    const currentPath = process.env.PATH || '';
    return {
      ...process.env,
      ...this._envOverrides,
      PATH: testsBinDir + delimiter + currentPath,
    };
  }

  // Extract the caller's line number from a stack trace for error messages
  // that match bash's `(line N)` format.
  _getCallerLine(): number {
    const obj: {stack?: string} = {};
    Error.captureStackTrace(obj);
    if (obj.stack) {
      // Stack: Error -> _getCallerLine -> <immediate caller> -> <caller's caller>
      // We want the line of whoever called the function that called us,
      // which is frame index 3 (0-indexed from the "Error" line).
      const frames = obj.stack.split('\n');
      for (let i = 3; i < frames.length; i++) {
        const match = frames[i].match(/:(\d+):\d+\)?$/);
        if (match) {
          return parseInt(match[1], 10);
        }
      }
    }
    return 0;
  }

  async flowCmd(
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string},
  ): Promise<[number, string, string]> {
    const cwd = (opts && opts.cwd) || this._testDir;
    const result = await execFilePromise(
      this._flowBin,
      args,
      {
        cwd,
        env: this._getEnv(),
      },
      opts && opts.stdin,
    );
    return [result.code, result.stdout, result.stderr];
  }

  async assertExit(
    expectedCode: number,
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string, noprint?: boolean},
  ): Promise<void> {
    // When called directly (not via assertOk/assertErrors/assertOne),
    // _getCallerLine gives us the right frame. When called via a wrapper,
    // the wrapper passes a pre-computed line to avoid the extra frame.
    const callerLine = this._getCallerLine();
    const [code, stdout] = await this.flowCmd(args, opts);
    if (!(opts && opts.noprint)) {
      this._outBuffer += stdout;
    }
    if (code !== expectedCode) {
      const lineInfo = callerLine > 0 ? ` (line ${callerLine})` : '';
      throw new Error(
        `\`${basename(this._flowBin)} ${args.join(' ')}\` expected to exit code ${expectedCode} but got ${code}${lineInfo}`,
      );
    }
  }

  // Internal: assertExit with a pre-computed caller line number.
  // Used by assertOk/assertErrors/assertOne to report the correct test
  // script line in error messages (since they add an extra stack frame).
  async _assertExitWithLine(
    callerLine: number,
    expectedCode: number,
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string, noprint?: boolean},
  ): Promise<void> {
    const [code, stdout] = await this.flowCmd(args, opts);
    if (!(opts && opts.noprint)) {
      this._outBuffer += stdout;
    }
    if (code !== expectedCode) {
      const lineInfo = callerLine > 0 ? ` (line ${callerLine})` : '';
      throw new Error(
        `\`${basename(this._flowBin)} ${args.join(' ')}\` expected to exit code ${expectedCode} but got ${code}${lineInfo}`,
      );
    }
  }

  async assertOk(
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string, noprint?: boolean},
  ): Promise<void> {
    // Capture caller line here (1 frame closer to the test script than
    // assertExit would be) so error messages reference the test file.
    const callerLine = this._getCallerLine();
    return this._assertExitWithLine(callerLine, EXIT_OK, args, opts);
  }

  async assertErrors(
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string, noprint?: boolean},
  ): Promise<void> {
    const callerLine = this._getCallerLine();
    return this._assertExitWithLine(callerLine, EXIT_ERRS, args, opts);
  }

  async assertOne(
    args: Array<string>,
    opts?: {cwd?: string, stdin?: string, noprint?: boolean},
  ): Promise<void> {
    const callerLine = this._getCallerLine();
    return this._assertExitWithLine(callerLine, EXIT_ONE, args, opts);
  }

  // Server management
  async startFlowUnsafe(
    root: string,
    ...options: Array<string>
  ): Promise<number> {
    // Handle saved state mode: create saved state first, then start
    // with --saved-state-fetcher (matching bash start_flow_unsafe).
    if (this._savedState) {
      let flowconfigName = '.flowconfig';
      for (let i = 0; i < options.length; i++) {
        if (options[i] === '--flowconfig-name' && i + 1 < options.length) {
          flowconfigName = options[i + 1];
        }
      }

      const ok = await this.createSavedState(root, flowconfigName);
      if (!ok) {
        return 1;
      }

      const args = ['start', root];
      if (this._noFlowlib) {
        args.push('--no-flowlib');
      }
      args.push(
        '--wait',
        '--wait-for-recheck',
        this._waitForRecheck,
        '--saved-state-fetcher',
        'local',
        '--saved-state-no-fallback',
        '--file-watcher',
        this._fileWatcher,
        '--log-file',
        this._logFile,
        '--monitor-log-file',
        this._monitorLogFile,
        '--long-lived-workers',
        this._longLivedWorkers,
        ...options,
      );
      const [code] = await this.flowCmd(args);
      return code;
    }

    const args = ['start', root];
    if (this._noFlowlib) {
      args.push('--no-flowlib');
    }
    args.push(
      '--wait',
      '--wait-for-recheck',
      this._waitForRecheck,
      '--file-watcher',
      this._fileWatcher,
      '--log-file',
      this._logFile,
      '--monitor-log-file',
      this._monitorLogFile,
      '--long-lived-workers',
      this._longLivedWorkers,
      ...options,
    );
    const [code] = await this.flowCmd(args);
    return code;
  }

  async startFlow(root: string, ...options: Array<string>): Promise<void> {
    const code = await this.startFlowUnsafe(root, ...options);
    if (code !== 0) {
      throw new Error(`flow start exited with code ${code}`);
    }
  }

  async createSavedState(
    root: string,
    flowconfigName: string = '.flowconfig',
  ): Promise<boolean> {
    try {
      // Start server
      const startArgs = ['start', root];
      if (this._noFlowlib) {
        startArgs.push('--no-flowlib');
      }
      startArgs.push(
        '--wait',
        '--wait-for-recheck',
        this._waitForRecheck,
        '--lazy-mode',
        'none',
        '--file-watcher',
        this._fileWatcher,
        '--flowconfig-name',
        flowconfigName,
        '--log-file',
        this._logFile,
        '--monitor-log-file',
        this._monitorLogFile,
        '--long-lived-workers',
        this._longLivedWorkers,
      );
      const [startCode] = await this.flowCmd(startArgs);
      if (startCode !== 0) return false;

      try {
        // Save state
        const savedStateFile = join(root, '.flow.saved_state');
        const changesFile = join(root, '.flow.saved_state_file_changes');
        const [saveCode] = await this.flowCmd([
          'save-state',
          '--root',
          root,
          '--out',
          savedStateFile,
          '--flowconfig-name',
          flowconfigName,
        ]);
        if (saveCode !== 0) return false;

        // Touch changes file
        await fs.promises.writeFile(changesFile, '');
        return true;
      } finally {
        // Always stop the server after starting it
        await this.flowCmd(['stop', '--flowconfig-name', flowconfigName, root]);
      }
    } catch (e) {
      return false;
    }
  }

  // File ops with force-recheck (mirrors fs.sh)
  // Handles both files and directories (matches bash rm -rf behavior)
  async remove(...files: Array<string>): Promise<void> {
    for (const file of files) {
      const fullPath = join(this._testDir, file);
      if (await existsAsync(fullPath)) {
        const stat = await fs.promises.lstat(fullPath);
        if (stat.isDirectory()) {
          await fs.promises.rm(fullPath, {recursive: true, force: true});
        } else {
          await fs.promises.unlink(fullPath);
        }
      }
    }
    await this.assertOk(['force-recheck', ...files], {noprint: true});
  }

  async move(src: string, dst: string): Promise<void> {
    await fs.promises.rename(
      join(this._testDir, src),
      join(this._testDir, dst),
    );
    await this.assertOk(['force-recheck', src, dst], {noprint: true});
  }

  async copy(src: string, dst: string): Promise<void> {
    await fs.promises.copyFile(
      join(this._testDir, src),
      join(this._testDir, dst),
    );
    await this.assertOk(['force-recheck', src, dst], {noprint: true});
  }

  // Raw file ops (no recheck)
  async copyFile(src: string, dst: string): Promise<void> {
    await fs.promises.copyFile(
      join(this._testDir, src),
      join(this._testDir, dst),
    );
  }

  async removeFile(filePath: string): Promise<void> {
    await fs.promises.unlink(join(this._testDir, filePath));
  }

  async renameFile(src: string, dst: string): Promise<void> {
    await fs.promises.rename(
      join(this._testDir, src),
      join(this._testDir, dst),
    );
  }

  async writeFile(filePath: string, contents: string): Promise<void> {
    await fs.promises.writeFile(join(this._testDir, filePath), contents);
  }

  async readFile(filePath: string): Promise<string> {
    return await fs.promises.readFile(join(this._testDir, filePath), 'utf8');
  }

  async mkdir(dir: string): Promise<void> {
    await fs.promises.mkdir(join(this._testDir, dir), {recursive: true});
  }

  // Query helpers
  async queryAtPos(
    query: string,
    file: string,
    line: number,
    col: number,
    ...flags: Array<string>
  ): Promise<void> {
    this.printf('%s:%d:%d\n', file, line, col);
    // Match bash's `echo "Flags:" "${flags[@]}"` which omits the trailing
    // space when the flags array is empty.
    if (flags.length > 0) {
      this.print('Flags: ' + flags.join(' ') + '\n');
    } else {
      this.print('Flags:\n');
    }
    const fileContent = await fs.promises.readFile(
      join(this._testDir, file),
      'utf8',
    );
    const args = [
      query,
      file,
      String(line),
      String(col),
      '--strip-root',
      ...flags,
    ];
    const result = await execFilePromise(
      this._flowBin,
      args,
      {
        cwd: this._testDir,
        env: this._getEnv(),
      },
      fileContent,
    );
    if (result.code !== 0) {
      throw new Error(
        `\`flow ${args.join(' ')}\` expected exit 0 but got ${result.code}`,
      );
    }
    this._outBuffer += result.stdout;
    this.print('\n');
  }

  async queriesInFile(
    query: string,
    file: string,
    ...argFlags: Array<string>
  ): Promise<void> {
    const filePath = join(this._testDir, file);
    const content = await fs.promises.readFile(filePath, 'utf8');
    const lines = content.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      // Match lines that start with // and contain ^
      if (/^\/\/.*\^/.test(line)) {
        const lineNum = i + 1; // 1-based
        const caretIndex = line.indexOf('^');
        const col = caretIndex + 1; // 1-based column
        const targetLine = lineNum - 1; // The line above the marker

        // Extract per-line flags after the ^.
        // Use split('^')[1] to match bash's `awk -F'^' '{ print $2 }'`
        // which extracts text between the first and second ^ only (not
        // all text after the first ^).
        const parts = line.split('^');
        const afterCaret = (parts[1] || '').trim();
        const lineFlags = afterCaret ? afterCaret.split(/\s+/) : [];

        const allFlags = [...argFlags, ...lineFlags];
        await this.queryAtPos(query, file, targetLine, col, ...allFlags);
      }
    }
  }

  // Text filtering
  // Returns matching lines with a trailing newline (matching bash grep output).
  // The pattern is treated as bash grep BRE (Basic Regular Expression).
  // In BRE, +, ?, |, {, }, (, ) are literal unless backslash-escaped.
  // In JS regex (ERE-like), these are metacharacters by default.
  // This method converts BRE semantics to JS regex semantics.
  grepLines(text: string, pattern: string): string {
    const brePattern = _convertBREtoJS(pattern);
    let re;
    try {
      re = new RegExp(brePattern);
    } catch (e) {
      // If the pattern is still invalid regex, return empty (matching
      // bash grep's behavior of printing nothing on invalid BRE).
      return '';
    }
    const lines = text.split('\n');
    // Remove trailing empty element produced by split when input ends with \n.
    // Without this, patterns that match empty strings (e.g. '.*', '') would
    // include a spurious extra line, diverging from bash grep behavior.
    if (lines.length > 0 && lines[lines.length - 1] === '') {
      lines.pop();
    }
    const matched = lines.filter(line => re.test(line));
    if (matched.length === 0) {
      return '';
    }
    return matched.join('\n') + '\n';
  }

  sedReplace(text: string, search: string, replace: string): string {
    const brePattern = _convertBREtoJS(search);
    const re = new RegExp(brePattern, 'g');

    // Bash sed processes text line-by-line: each line's pattern space does NOT
    // include the trailing newline.  This means `.` and negated character
    // classes like `[^a]` never match `\n`.  We replicate this by splitting on
    // `\n`, applying the regex to each line individually, and re-joining.
    const trailingNewline = text.endsWith('\n');
    const lines = text.split('\n');
    // If the text ended with \n, split produces a trailing empty element — remove it
    // so we don't process a phantom empty line.
    if (trailingNewline && lines[lines.length - 1] === '') {
      lines.pop();
    }

    // Handle bash sed replacement semantics:
    // - & in the replacement refers to the matched text
    // - \& is a literal &
    // - \N is a back-reference to capture group N
    // - $ has no special meaning (unlike JS)
    const applyReplace = (line: string): string =>
      line.replace(re, (matched, ...groups) => {
        let result = '';
        for (let i = 0; i < replace.length; i++) {
          const ch = replace[i];
          if (ch === '\\' && i + 1 < replace.length) {
            const next = replace[i + 1];
            if (next === '&') {
              // \& → literal &
              result += '&';
              i++;
            } else if (next >= '1' && next <= '9') {
              // \N → capture group N
              const groupIndex = parseInt(next, 10) - 1;
              // groups array from replace callback: groups[0..n-1] are captures,
              // followed by offset and the full string. Only use capture groups.
              if (
                groupIndex < groups.length - 2 &&
                groups[groupIndex] != null
              ) {
                result += groups[groupIndex];
              }
              i++;
            } else if (next === '\\') {
              result += '\\';
              i++;
            } else if (next === 'n') {
              // \n → newline (GNU sed extension)
              result += '\n';
              i++;
            } else if (next === 't') {
              // \t → tab (GNU sed extension)
              result += '\t';
              i++;
            } else {
              result += '\\' + next;
              i++;
            }
          } else if (ch === '&') {
            // & → the matched text
            result += matched;
          } else {
            result += ch;
          }
        }
        return result;
      });

    const transformed = lines.map(applyReplace);
    return transformed.join('\n') + (trailingNewline ? '\n' : '');
  }

  // Log inspection
  async showSkippingStats(logFile: string): Promise<void> {
    this.print('\n========Skipping stats========\n');
    try {
      const content = await fs.promises.readFile(logFile, 'utf8');
      const mergeMatch = content.match(/Merge skipped \d+ of \d+ modules/g);
      if (mergeMatch && mergeMatch.length > 0) {
        this.print(mergeMatch[mergeMatch.length - 1] + '\n');
      }
      const checkMatch = content.match(/Check will skip \d+ of \d+ files/g);
      if (checkMatch && checkMatch.length > 0) {
        this.print(checkMatch[checkMatch.length - 1] + '\n');
      }
    } catch (e) {
      // Log file may not exist
    }
  }

  // Utility
  sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

module.exports = {
  TestContext,
  EXIT_OK,
  EXIT_ONE,
  EXIT_ERRS,
  EXIT_INVALID_FLOWCONFIG,
  EXIT_SERVER_ALREADY_RUNNING,
  EXIT_COULD_NOT_FIND_FLOWCONFIG,
  EXIT_USAGE,
};
