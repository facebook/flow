declare function invariant(condition: unknown, ...args: Array<unknown>): asserts condition;
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
  invariant(typeof x === 'string', takesString(x)); // error: later args are evaluated before the assertion applies
}

function callAfterPropertyRead(box: {value: ?string}): void {
  if (box.value != null) {
    invariant(box.value.trim() !== ''); // no error: `box.value` is read before the call
  }
}

function laterArgWritesValueReadEarlier(value: ?number): void {
  let x: ?number = value;
  if (x != null) {
    invariant(x.toFixed() !== '', (x = null)); // no error: `x` is a number when it is read
  }
}

function laterArgAssignsValueReadEarlier(): void {
  let x: number;
  invariant((x as number) > 0, (x = 1)); // error: `x` is read before it is assigned
}
