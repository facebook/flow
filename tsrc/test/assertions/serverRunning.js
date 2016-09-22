/* @flow */

import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';

function isRunning(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (e) {
    return e.code === 'EPERM';
  }
}

export default function(
  expected: boolean,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const pid = env.getServerPid();
    const actual = pid == null ? 'never started' : isRunning(pid);
    const suggestion = { method: 'serverRunning', args: [actual] };
    return simpleDiffAssertion(
      String(expected),
      String(actual),
      assertLoc,
      reason,
      "server to be running",
      suggestion,
    );
  };
}
