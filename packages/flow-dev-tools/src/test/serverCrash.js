/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

// A crash prints the head of what the dying process logged, because that is where the cause is:
// a Rust panic leads with its message and location and then spends tens of lines on a backtrace,
// so a tail-only excerpt shows the backtrace of the last panic and loses every header. The tail is
// kept too, in case the process said something after the first thing went wrong.
const HEAD_LINES = 40;
const TAIL_LINES = 20;
const PART_CHARS = 4000;

// What the harness saw when the process it spawned went away. `stderr` is everything that process
// wrote; note that `flow server` is the monitor, so a panic inside the server it supervises lands
// in a log file rather than here.
export type ServerExit = $ReadOnly<{
  code: number | null,
  signal: string | null,
  stderr: string,
}>;

// What Flow appended to one of its logs during the step that killed it. The harness marks the
// logs' lengths as each step begins, so this window is bounded by the test's own timeline rather
// than by guessing which part of the file is interesting.
export type ServerLog = $ReadOnly<{
  name: string,
  contents: string,
}>;

// The declarations that make a server exit legitimate. Both are made before the fact: the harness
// knows when it stopped the server itself, and a step says `dontMindServerDeath()` (or asks to
// `waitUntilServerStatus(_, 'stopped')`) when the death is the thing under test.
export type ExitContext = $ReadOnly<{
  harnessStoppedIt: boolean,
  stepAllowsDeath: boolean,
}>;

/**
 * Whether a server exit should be reported as a crash.
 *
 * The line is the harness's expectation, not the manner of death. A Rust panic, a fatal signal and
 * an orderly-but-unrequested exit are one event to a test: the server it was driving is gone, so
 * every later observation in the step describes the absence rather than the behaviour under test.
 *
 * In particular the exit code is not the discriminator. Flow exits deliberately with
 * `Exit.Restart` and `Exit.Autostop`, and a step that did not ask for either is just as broken as
 * one that hit a panic — `newtests/lsp/restartOnReinit` declares its restart with
 * `dontMindServerDeath()` for precisely that reason.
 *
 * Client commands cannot reach this decision, which is why no exit code is a parameter here:
 * `flow` exits non-zero for reasons tests assert on — 2 for type errors, 6 for no server running,
 * 7 for out of retries — and none of them means the server died.
 */
function isCrash(context: ExitContext): boolean {
  return !context.harnessStoppedIt && !context.stepAllowsDeath;
}

function describeTermination(exit: ServerExit): string {
  if (exit.signal != null) {
    return `killed by signal ${exit.signal}`;
  }
  if (exit.code != null) {
    return `exited with code ${exit.code}`;
  }
  return 'exited for a reason the harness could not read';
}

const TRUNCATED = '...(truncated)';

// Each part is cut from the end away from the interesting edge: the head is worth reading from its
// first character, the tail from its last.
function clampHead(text: string): string {
  return text.length <= PART_CHARS
    ? text
    : text.slice(0, PART_CHARS) + TRUNCATED;
}

function clampTail(text: string): string {
  return text.length <= PART_CHARS
    ? text
    : TRUNCATED + text.slice(text.length - PART_CHARS);
}

function excerpt(text: string): string {
  const trimmed = text.replace(/\s+$/, '');
  if (trimmed === '') {
    return '(nothing)';
  }
  const lines = trimmed.split('\n');
  const headLines = lines.slice(0, HEAD_LINES);
  // Taken from what the head left behind, so the two never overlap however few lines there are.
  // One enormous line therefore yields a head and no tail, rather than the same text twice.
  const tailLines = lines.slice(HEAD_LINES).slice(-TAIL_LINES);
  const elided = lines.length - headLines.length - tailLines.length;
  const parts = [clampHead(headLines.join('\n'))];
  if (elided > 0) {
    parts.push(`... ${elided} lines elided ...`);
  }
  if (tailLines.length > 0) {
    parts.push(clampTail(tailLines.join('\n')));
  }
  return parts.join('\n');
}

/**
 * The failure text for a crash.
 *
 * The log excerpts are the payload: a Rust panic prints its message, location and backtrace to
 * whichever log belongs to the process that died, and a process dying any other way leaves its
 * last words there too. Every log is shown rather than the one guessed to be interesting, so this
 * does not depend on which Flow process crashed or on what it printed. The spawned process's own
 * stderr is the fallback for a death early enough that no log exists.
 */
function describeServerCrash(
  exit: ServerExit,
  logs: $ReadOnlyArray<ServerLog>,
  tmpDir: string,
): string {
  const sources =
    logs.length > 0
      ? logs.map(log => ({label: log.name, text: log.contents}))
      : [{label: 'the spawned process stderr', text: exit.stderr}];
  return [
    `flow server crashed: ${describeTermination(exit)}`,
    `full logs are under ${tmpDir}`,
    ...sources.map(
      source =>
        `${source.label}, during the step that crashed:\n${excerpt(source.text)}`,
    ),
  ].join('\n');
}

/**
 * The part of a log that belongs to the current step, given the length it had when the step began.
 *
 * A file shorter than its mark was rotated out from under us — Flow renames `.log` to `.log.old`
 * when a server restarts — so the file sitting here now is a fresh one and all of it is new.
 */
function logWindow(bytes: Buffer, mark: number): string {
  return bytes.subarray(bytes.length < mark ? 0 : mark).toString('utf8');
}

/**
 * The logs worth showing for a crash: what Flow itself wrote, and not `test.log`, which is the
 * harness's own record of the actions it took.
 *
 * `.log.old` counts. A restart rotates the dying server's log there, so in the one case where the
 * names move it holds the evidence and the `.log` beside it belongs to its replacement. Outside
 * that case it does not grow during a step and drops out for being empty.
 */
function isServerLogName(name: string): boolean {
  return name !== 'test.log' && /\.(log|monitor_log)(\.old)?$/.test(name);
}

module.exports = {
  describeServerCrash,
  HEAD_LINES,
  isCrash,
  isServerLogName,
  logWindow,
  TAIL_LINES,
};
