declare function invariant(condition: unknown, ...args: Array<unknown>): void;
declare function takesString(s: string): void;

function laterArgReassignsRefinedBinding(y: unknown, z: number): void {
  invariant(typeof y === 'string', (y = z));
  y as string; // error: y holds z (number); the condition described the earlier value
  y as number;
}

function pureLaterArgStillRefines(y: unknown, msg: string): void {
  invariant(typeof y === 'string', msg);
  y as string;
  y as number; // error: genuine refinement is preserved
}

function laterArgReadUsesRefinedEnv(x: unknown): void {
  // TODO: replaying later args re-records their reads in the refined env,
  // so the unknown-to-string error here is suppressed. Once replay skips
  // read-recording, this call should error.
  invariant(typeof x === 'string', takesString(x));
}
