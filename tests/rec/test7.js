/**
 * @flow
 */

declare class ImmutableSet<T> {
  toArray(): T[];
}

declare function foo(): ImmutableSet<string>;

function bar() {
  let value;

  if (true) {
    value = foo();
  }

  if (value instanceof ImmutableSet) {
    /* $FlowExpectedError This is an expected error in constrained-writes/ssa env mode
     * Modifying this file to not be an error anymore in those modes cancels out
     * its ability to track a potential regression (see D6231177) in the non-ssa env
     * mode. This regression is not common and it's also not clear how to trigger
     * it otherwise in env-mode ssa. Finally, this code is not a performance hazzard
     * for env-mode ssa, since we don't propagate ImmutableSet<any> to `value` for
     * values that fail the condition. */
    value = value.toArray();
  }
}
