function takesAString(x: string): void {}

function runTest(y: number): void {
  /* $FlowFixMe - suppressing the error op location should also work */
  takesAString(
    y,
  );
}
