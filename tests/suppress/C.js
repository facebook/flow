function takesAString(x: string): void {}

function runTest(y: number): void {
  takesAString(
    /* $FlowFixMe[incompatible-type] - suppressing the error op location should also work */
    y,
  );
}
