function takesAString(x: string): void {}

function runTest(y: number): void {
  takesAString(
    /* $FlowFixMe[incompatible-call] - suppressing the error op location should also work */
    y,
  );
}
