/**
 * @flow
 * @jsx Test.f
 */

const Test = {f: (c: () => void): void => {}};
function Component(): void {}
const a: empty = <Component />; // Error: void ~> empty
