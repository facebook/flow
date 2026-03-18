/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {exec, execFile} = require('child_process');
const fs = require('fs');

export type ExecResult = {
  code: number,
  stdout: string,
  stderr: string,
};

// Default timeout: 10 minutes. Prevents the test runner from hanging
// indefinitely if a flow command (e.g. `flow stop`, `flow start --wait`)
// becomes unresponsive.
const DEFAULT_TIMEOUT_MS = 10 * 60 * 1000;

async function existsAsync(p: string): Promise<boolean> {
  try {
    await fs.promises.access(p);
    return true;
  } catch {
    return false;
  }
}

// Characters that can be backslash-escaped inside double quotes in bash.
// For any other character, the backslash is preserved literally.
const DOUBLE_QUOTE_ESCAPABLE = new Set(['$', '`', '"', '\\', '\n']);

function splitShellArgs(argString: string): Array<string> {
  const args = [];
  let current = '';
  let quote: 'single' | 'double' | null = null;
  // Track whether we've entered a quoting context for the current arg.
  // Needed to preserve empty quoted args (e.g. '' or "") as empty strings.
  let hadQuote = false;

  for (let i = 0; i < argString.length; i++) {
    const ch = argString[i];

    if (quote === 'single') {
      if (ch === "'") {
        quote = null;
      } else {
        current += ch;
      }
      continue;
    }

    if (quote === 'double') {
      if (ch === '"') {
        quote = null;
      } else if (ch === '\\') {
        if (i + 1 < argString.length) {
          const next = argString[i + 1];
          if (DOUBLE_QUOTE_ESCAPABLE.has(next)) {
            // These characters are escaped: backslash is consumed.
            current += next;
          } else {
            // All other characters: backslash is preserved literally.
            current += '\\' + next;
          }
          i++;
        } else {
          current += '\\';
        }
      } else {
        current += ch;
      }
      continue;
    }

    if (ch === "'") {
      quote = 'single';
      hadQuote = true;
    } else if (ch === '"') {
      quote = 'double';
      hadQuote = true;
    } else if (/\s/.test(ch)) {
      if (current !== '' || hadQuote) {
        args.push(current);
        current = '';
        hadQuote = false;
      }
    } else if (ch === '\\') {
      if (i + 1 < argString.length) {
        current += argString[i + 1];
        i++;
      } else {
        current += '\\';
      }
    } else {
      current += ch;
    }
  }

  if (quote !== null) {
    throw new Error('Unterminated quote in argument string: ' + argString);
  }

  if (current !== '' || hadQuote) {
    args.push(current);
  }

  return args;
}

function execFilePromise(
  cmd: string,
  args: Array<string>,
  options?: Object,
  stdinData?: string,
): Promise<ExecResult> {
  return new Promise(resolve => {
    const proc = execFile(
      cmd,
      args,
      {
        ...options,
        maxBuffer: (options && options.maxBuffer) || 100 * 1024 * 1024,
        timeout: (options && options.timeout) || DEFAULT_TIMEOUT_MS,
      },
      (err, stdout, stderr) => {
        let code = 0;
        if (err) {
          // err.code is the exit code (number) for normal non-zero exits,
          // but can be a string for system errors (e.g. 'ENOENT',
          // 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER'). Default to 1 for
          // non-numeric codes.
          const rawCode = (err: any).code;
          code = typeof rawCode === 'number' ? rawCode : 1;
        }
        resolve({code, stdout: String(stdout), stderr: String(stderr)});
      },
    );
    if (stdinData != null) {
      // Prevent unhandled 'error' events (e.g. EPIPE) if the child
      // exits before all stdin data is written.
      proc.stdin.on('error', () => {});
      proc.stdin.write(stdinData);
      proc.stdin.end();
    }
  });
}

// Execute a command string through the shell, matching bash's `eval "$cmd"`.
// Used for .testconfig cmd values that may contain shell constructs like
// pipes or redirections.
function execShellPromise(
  cmdString: string,
  options?: Object,
  stdinData?: string,
): Promise<ExecResult> {
  return new Promise(resolve => {
    const proc = exec(
      cmdString,
      {
        ...options,
        maxBuffer: (options && options.maxBuffer) || 100 * 1024 * 1024,
        timeout: (options && options.timeout) || DEFAULT_TIMEOUT_MS,
      },
      (err, stdout, stderr) => {
        let code = 0;
        if (err) {
          const rawCode = (err: any).code;
          code = typeof rawCode === 'number' ? rawCode : 1;
        }
        resolve({code, stdout: String(stdout), stderr: String(stderr)});
      },
    );
    if (stdinData != null) {
      proc.stdin.on('error', () => {});
      proc.stdin.write(stdinData);
      proc.stdin.end();
    }
  });
}

module.exports = {
  execFilePromise,
  execShellPromise,
  existsAsync,
  splitShellArgs,
};
